package reachablelinkage.classpath

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarFile}
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

import reachablelinkage.classfile.{ClassInfo, ClassParser, Member}
import reachablelinkage.symbols.{ClassId, DescId, NameId, Symbols}

/**
 * A jar's classes, parsed once and kept on disk, so a later run (of any project) reads them back
 * instead of parsing them again. A summary is a pure function of the jar's class files and of the
 * parser, so it is named by a fingerprint of the jar's classes and carries the parser version.
 *
 * Ids are interned per run, so a summary has its own string tables and its own local ids. A [[Reader]]
 * decodes one class at a time and maps local ids to the current run's ids, interning a name only when
 * a decoded class uses it, which keeps class loading lazy.
 *
 * File layout (big-endian ints): magic, format version, parser version, 32-byte fingerprint; then 13
 * header ints (own-class count; class, name and descriptor string counts; offsets of the three string
 * offset tables and blobs, of the own-class index sorted by name, and of the per-local-class record
 * table; and the size of everything before the checksum, which catches a truncated file); the sections; then one record per class;
 * then a CRC-32 of all of that, so a damaged file (a flipped bit, a cut, bytes added) is never decoded: a length read from it could be
 * anything. The file is `body` followed by the 4-byte checksum. A record is: access, superclass, interfaces,
 * methods and fields as (name, descriptor, access), the three reference arrays, the three per-method
 * start arrays, the guards, and the probe strings. Anything unexpected (wrong magic, version or
 * fingerprint, a short file, a bad offset) means the file is not used.
 */
private[classpath] object JarSummary:
  val FormatVersion: Int = 2
  private val Magic: Int            = 0x524c4a53 // "RLJS"
  private val FingerprintBytes: Int = 32
  private val HeaderStart: Int      = 12 + FingerprintBytes
  private val HeaderInts: Int       = 13
  private val HeaderEnd: Int        = HeaderStart + HeaderInts * 4

  // Indices of the header ints.
  private val NOwn = 0; private val NClassStr = 1; private val NNameStr = 2; private val NDescStr = 3
  private val ClassOffs = 4; private val ClassBlob = 5; private val NameOffs = 6; private val NameBlob = 7
  private val DescOffs = 8; private val DescBlob = 9; private val SortedOwn = 10; private val RecordTable = 11
  private val TotalSize = 12

  /**
   * Names, sizes and CRCs of the entries the classes come from (the versioned entry for a class a multi-release jar
   * overlays, see [[JarEntries]]), read from the central directory without touching class data. A different release that
   * resolves to different entries therefore has a different fingerprint, and a different summary.
   */
  def fingerprint(resolved: Vector[(String, JarEntry)]): Array[Byte] =
    val md = MessageDigest.getInstance("SHA-256")
    resolved.foreach: (_, e) =>
      md.update(e.getName.getBytes(UTF_8))
      md.update(0.toByte)
      md.update(java.nio.ByteBuffer.allocate(16).putLong(e.getSize).putLong(e.getCrc).array())
    md.digest()

  def fileName(fingerprint: Array[Byte]): String =
    "jar-" + fingerprint.take(8).map(b => f"${b & 0xff}%02x").mkString + ".summary"

  /**
   * The summary of `jar`: read from `cacheDirectory` if a valid one is there, else built (every
   * class parsed once) and saved. Never throws; a failure means no reader and the caller parses.
   */
  def open(jar: JarFile, resolved: Vector[(String, JarEntry)], cacheDirectory: Path): (Reader | Null, SummaryOutcome) =
    try
      val fp   = fingerprint(resolved)
      val file = cacheDirectory.resolve(fileName(fp))
      readFile(file, fp) match
        case Some(reader) => (reader, SummaryOutcome.Loaded)
        case None =>
          val bytes = build(jar, resolved, fp)
          val saved = writeFile(file, bytes)
          // Map the saved file so the bytes do not stay on the heap; if saving failed, use them as they are.
          val reader = (if saved then readFile(file, fp) else None).getOrElse(new Reader(ByteBuffer.wrap(bytes)))
          (reader, SummaryOutcome.Built)
    catch case _: java.io.IOException | _: RuntimeException => (null, SummaryOutcome.Unavailable)

  /** Parses every class of the jar with a fresh symbol table of its own and encodes the result. */
  private[classpath] def build(jar: JarFile, resolved: Vector[(String, JarEntry)], fingerprint: Array[Byte]): Array[Byte] =
    val syms  = new Symbols
    val infos = ArrayBuffer.empty[ClassInfo]
    resolved.foreach: (_, e) =>
      val bytes = Using.resource(jar.getInputStream(e))(in => Streams.readAll(in, e.getSize))
      // A class that does not parse gets no record: the reader says "absent" and the caller parses it.
      ClassParser.parse(bytes, syms).foreach(infos += _)
    encode(infos.toVector, syms, fingerprint)

  private def encode(infos: Vector[ClassInfo], syms: Symbols, fingerprint: Array[Byte]): Array[Byte] =
    val sink = new ByteSink
    sink.putInt(Magic); sink.putInt(FormatVersion); sink.putInt(ClassParser.Version); sink.putBytes(fingerprint)
    val headerAt = sink.size
    var h        = 0
    while h < HeaderInts do { sink.putInt(0); h += 1 }
    def header(i: Int, v: Int): Unit = sink.setInt(headerAt + 4 * i, v)
    header(NOwn, infos.length); header(NClassStr, syms.classCount); header(NNameStr, syms.nameCount); header(NDescStr, syms.descCount)

    def strings(count: Int, string: Int => String, offsHeader: Int, blobHeader: Int): Unit =
      val blob = new ByteSink
      val offs = new Array[Int](count + 1)
      var i    = 0
      while i < count do
        offs(i) = blob.size
        blob.putBytes(string(i).getBytes(UTF_8))
        i += 1
      offs(count) = blob.size
      header(offsHeader, sink.size)
      offs.foreach(sink.putInt)
      header(blobHeader, sink.size)
      sink.putBytes(blob.toArray)
    strings(syms.classCount, i => syms.className(ClassId(i)), ClassOffs, ClassBlob)
    strings(syms.nameCount, i => syms.name(NameId(i)), NameOffs, NameBlob)
    strings(syms.descCount, i => syms.desc(DescId(i)), DescOffs, DescBlob)

    header(SortedOwn, sink.size)
    infos.map(_.id.value).sortBy(i => syms.className(ClassId(i))).foreach(sink.putInt)

    // The record table is filled in as the records are written.
    val tableAt = sink.size
    header(RecordTable, tableAt)
    var t = 0
    while t < syms.classCount do { sink.putInt(-1); t += 1 }

    def ints(a: Array[Int]): Unit = { sink.putInt(a.length); a.foreach(sink.putInt) }
    def members(ms: Array[Member]): Unit =
      sink.putInt(ms.length)
      ms.foreach(m => { sink.putInt(m.name.value); sink.putInt(m.desc.value); sink.putInt(m.access) })
    infos.foreach: info =>
      sink.setInt(tableAt + 4 * info.id.value, sink.size)
      sink.putInt(info.access)
      sink.putInt(info.superName.value)
      sink.putInt(info.interfaces.length); info.interfaces.foreach(i => sink.putInt(i.value))
      members(info.methods); members(info.fields)
      ints(info.methodRefs); ints(info.fieldRefs); ints(info.classRefs)
      ints(info.methodRefStart); ints(info.fieldRefStart); ints(info.classRefStart)
      ints(info.guards)
      sink.putInt(info.encodedProbes.length)
      info.encodedProbes.foreach: p =>
        val b = p.getBytes(UTF_8)
        sink.putInt(b.length); sink.putBytes(b)
      ints(info.newRefs); ints(info.fieldWrites)
      ints(info.signatureTypes); ints(info.literalArgs); ints(info.annotations)
    header(TotalSize, sink.size)
    val body = sink.toArray
    val crc  = new java.util.zip.CRC32()
    crc.update(body)
    body ++ java.nio.ByteBuffer.allocate(4).putInt(crc.getValue.toInt).array()

  /** A reader over `file` if it is a valid summary for `fingerprint` made by this parser; else None. */
  private[classpath] def readFile(file: Path, fingerprint: Array[Byte]): Option[Reader] =
    try
      if !Files.isRegularFile(file) then None
      else
        val buffer = Using.resource(FileChannel.open(file, StandardOpenOption.READ))(ch => ch.map(FileChannel.MapMode.READ_ONLY, 0, ch.size))
        if valid(buffer, fingerprint) then Some(new Reader(buffer)) else None
    catch case _: java.io.IOException | _: RuntimeException => None

  private def valid(b: ByteBuffer, fingerprint: Array[Byte]): Boolean =
    b.capacity >= HeaderEnd && b.getInt(0) == Magic && b.getInt(4) == FormatVersion && b.getInt(8) == ClassParser.Version && {
      val stored = new Array[Byte](FingerprintBytes)
      val view   = b.duplicate()
      view.position(12)
      view.get(stored)
      java.util.Arrays.equals(stored, fingerprint)
    } && (0 until HeaderInts).forall(i => b.getInt(HeaderStart + 4 * i) >= 0) && {
      val bodySize = b.getInt(HeaderStart + 4 * TotalSize)
      bodySize >= HeaderEnd && b.capacity == bodySize + 4 && {
        // The whole body must match its checksum before any length in it is believed.
        val crc = new java.util.zip.CRC32()
        val body = b.duplicate()
        body.position(0)
        body.limit(bodySize)
        crc.update(body)
        crc.getValue.toInt == b.getInt(bodySize)
      }
    }

  /** Writes atomically (a temporary file, then a move) so a reader never sees half a file. */
  private[classpath] def writeFile(file: Path, bytes: Array[Byte]): Boolean =
    try
      Files.createDirectories(file.getParent)
      val tmp = Files.createTempFile(file.getParent, "jar-", ".tmp")
      try
        Files.write(tmp, bytes)
        try Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch case _: java.nio.file.AtomicMoveNotSupportedException => Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING)
        true
      finally Files.deleteIfExists(tmp)
    catch case _: java.io.IOException | _: RuntimeException => false

  /** Decodes classes from one summary, mapping its local ids to the ids of the run asking. */
  final class Reader(buf: ByteBuffer):
    private def header(i: Int): Int = buf.getInt(HeaderStart + 4 * i)
    // A second view of the same bytes to read ranges with: the absolute bulk `get(index, bytes)` is Java 13, and a relative one needs a position.
    private val cursor = buf.duplicate()
    private def bytesAt(index: Int, bytes: Array[Byte]): Unit =
      cursor.position(index)
      cursor.get(bytes)
    private val nOwn       = header(NOwn)
    private val nClassStr  = header(NClassStr)
    private val nNameStr   = header(NNameStr)
    private val nDescStr   = header(NDescStr)

    // The id maps belong to one run's symbol table; a different one starts them again.
    private var mappedFor: Symbols | Null = null
    private var classMap: Array[Int]      = new Array[Int](0)
    private var nameMap: Array[Int]       = new Array[Int](0)
    private var descMap: Array[Int]       = new Array[Int](0)

    private def string(offsHeader: Int, blobHeader: Int, i: Int): String =
      val offs  = header(offsHeader)
      val start = buf.getInt(offs + 4 * i)
      val end   = buf.getInt(offs + 4 * (i + 1))
      val bytes = new Array[Byte](end - start)
      bytesAt(header(blobHeader) + start, bytes)
      new String(bytes, UTF_8)

    /** The local id of an own class by name, or -1: a binary search of the names, sorted. */
    private def findOwn(name: String): Int =
      val sorted = header(SortedOwn)
      var lo     = 0
      var hi     = nOwn - 1
      var found  = -1
      while found < 0 && lo <= hi do
        val mid   = (lo + hi) >>> 1
        val local = buf.getInt(sorted + 4 * mid)
        val cmp   = name.compareTo(string(ClassOffs, ClassBlob, local))
        if cmp == 0 then found = local
        else if cmp < 0 then hi = mid - 1
        else lo = mid + 1
      found

    /** The class `internalName` from this summary with ids of `syms`, or null if the jar has no record for it. */
    def info(internalName: String, syms: Symbols): ClassInfo | Null =
      val local = findOwn(internalName)
      if local < 0 then null
      else
        val at = buf.getInt(header(RecordTable) + 4 * local)
        if at < 0 then null
        else
          if mappedFor ne syms then
            classMap = Array.fill(nClassStr)(-1)
            nameMap = Array.fill(nNameStr)(-1)
            descMap = Array.fill(nDescStr)(-1)
            mappedFor = syms
          decode(at, local, syms)

    private def mapClass(local: Int, syms: Symbols): Int =
      if local < 0 then local
      else
        val known = classMap(local)
        if known >= 0 then known
        else
          val id = syms.classId(string(ClassOffs, ClassBlob, local)).value
          classMap(local) = id
          id

    private def mapName(local: Int, syms: Symbols): Int =
      val known = nameMap(local)
      if known >= 0 then known
      else
        val id = syms.nameId(string(NameOffs, NameBlob, local)).value
        nameMap(local) = id
        id

    private def mapDesc(local: Int, syms: Symbols): Int =
      val known = descMap(local)
      if known >= 0 then known
      else
        val id = syms.descId(string(DescOffs, DescBlob, local)).value
        descMap(local) = id
        id

    // The read position, advanced by the section readers below, in the order the encoder wrote the sections. A Reader is not shared between threads.
    private var p = 0

    private def int(): Int =
      val v = buf.getInt(p)
      p += 4
      v

    private def rawInts(): Array[Int] =
      val a = new Array[Int](int())
      var i = 0
      while i < a.length do { a(i) = int(); i += 1 }
      a

    private def readInterfaces(syms: Symbols): Array[ClassId] =
      val interfaces = new Array[ClassId](int())
      var i          = 0
      while i < interfaces.length do { interfaces(i) = ClassId(mapClass(int(), syms)); i += 1 }
      interfaces

    private def readMembers(syms: Symbols): Array[Member] =
      val ms = new Array[Member](int())
      var i  = 0
      while i < ms.length do
        val name = mapName(int(), syms)
        val desc = mapDesc(int(), syms)
        ms(i) = Member(NameId(name), DescId(desc), int())
        i += 1
      ms

    /** (class, name, descriptor, flags) per method reference. */
    private def readMethodRefs(syms: Symbols): Array[Int] =
      val refs = new Array[Int](int())
      var i    = 0
      while i < refs.length do
        refs(i) = mapClass(int(), syms)
        refs(i + 1) = mapName(int(), syms)
        refs(i + 2) = mapDesc(int(), syms)
        refs(i + 3) = int()
        i += 4
      refs

    /** (class, name, descriptor) per field reference. */
    private def readFieldRefs(syms: Symbols): Array[Int] =
      val refs = new Array[Int](int())
      var i    = 0
      while i < refs.length do
        refs(i) = mapClass(int(), syms)
        refs(i + 1) = mapName(int(), syms)
        refs(i + 2) = mapDesc(int(), syms)
        i += 3
      refs

    private def readClassRefs(syms: Symbols): Array[Int] =
      val refs = new Array[Int](int())
      var i    = 0
      while i < refs.length do { refs(i) = mapClass(int(), syms); i += 1 }
      refs

    private def readProbes(): Array[String] =
      val probes = new Array[String](int())
      var i      = 0
      while i < probes.length do
        val bytes = new Array[Byte](int())
        bytesAt(p, bytes)
        p += bytes.length
        probes(i) = new String(bytes, UTF_8)
        i += 1
      probes

    /** (kind, member, class) per signature type. */
    private def readSignatureTypes(syms: Symbols): Array[Int] =
      val types = new Array[Int](int())
      var i     = 0
      while i < types.length do
        types(i) = int()
        types(i + 1) = int()
        types(i + 2) = mapClass(int(), syms)
        i += 3
      types

    /** (owner, method name, class) per class literal handed to a call. */
    private def readLiteralArgs(syms: Symbols): Array[Int] =
      val args = new Array[Int](int())
      var i    = 0
      while i < args.length do
        args(i) = mapClass(int(), syms)
        args(i + 1) = mapName(int(), syms)
        args(i + 2) = mapClass(int(), syms)
        i += 3
      args

    /** (kind, member, annotation type, class value or -1) per annotation entry. */
    private def readAnnotations(syms: Symbols): Array[Int] =
      val entries = new Array[Int](int())
      var i       = 0
      while i < entries.length do
        entries(i) = int()
        entries(i + 1) = int()
        entries(i + 2) = mapClass(int(), syms)
        val value = int()
        entries(i + 3) = if value < 0 then -1 else mapClass(value, syms)
        i += 4
      entries

    private def decode(start: Int, ownLocal: Int, syms: Symbols): ClassInfo =
      p = start
      val access         = int()
      val superName      = mapClass(int(), syms)
      val interfaces     = readInterfaces(syms)
      val methods        = readMembers(syms)
      val fields         = readMembers(syms)
      val methodRefs     = readMethodRefs(syms)
      val fieldRefs      = readFieldRefs(syms)
      val classRefs      = readClassRefs(syms)
      val methodRefStart = rawInts()
      val fieldRefStart  = rawInts()
      val classRefStart  = rawInts()
      val guards         = rawInts()
      val probes         = readProbes()
      val newRefs        = rawInts()
      val fieldWrites    = rawInts()
      val signatureTypes = readSignatureTypes(syms)
      val literalArgs    = readLiteralArgs(syms)
      val annotations    = readAnnotations(syms)
      ClassInfo(ClassId(mapClass(ownLocal, syms)), access, ClassId(superName), interfaces, methods, fields,
        methodRefs, fieldRefs, classRefs, methodRefStart, fieldRefStart, classRefStart, guards, probes, annotations, newRefs, fieldWrites, signatureTypes, literalArgs)
