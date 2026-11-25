package reachablelinkage.classpath

import scala.annotation.tailrec
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** The classes of the running JDK and the module each lives in. Listing the roughly 32,000 entries through the module system costs 300 to 400 ms on a cold JVM,
  * so it can be kept on disk: a file per JDK image, keyed by the image's identity and read back in tens of milliseconds. The cache is best effort: anything
  * wrong with it (missing, corrupt, from another JDK, unwritable) means listing the JDK afresh, never an error.
  *
  * @param modules
  *   module names, sorted
  * @param names
  *   internal class names (`java/lang/Object`), first module wins for a duplicate
  * @param moduleOf
  *   for each class, its index into `modules`
  * @param unreadable
  *   modules that could not be listed (their classes are absent from `names`); a listing with any is never cached
  */
private[classpath] final class JdkListing(val modules: Array[String], val names: Array[String], val moduleOf: Array[Int], val unreadable: List[String] = Nil)

private[classpath] object JdkListing:
  private val Magic: Int = 0x524c4a31 // "RLJ1"

  /** The listing, from `cacheDirectory` if it has a valid one, else by listing the JDK (and saving it there). */
  def load(cacheDirectory: Option[Path]): JdkListing =
    val key  = imageKey()
    val file = for dir <- cacheDirectory; k <- key yield dir.resolve(fileName(k))
    file
      .flatMap(f => key.flatMap(readCached(f, _)))
      .getOrElse:
        val listing = list()
        for f <- file; k <- key if listing.unreadable.isEmpty do writeCached(f, k, listing)
        listing

  /** Identity of the running JDK's runtime image; None when it has no `lib/modules` to take it from. */
  private[classpath] def imageKey(): Option[String] =
    val image = Paths.get(System.getProperty("java.home")).resolve("lib").resolve("modules")
    if !Files.isRegularFile(image) then None
    else
      // An image that cannot be read just now has no key, so nothing is cached for it.
      try
        val parts = Seq("java.runtime.version", "java.vm.version", "java.vendor").map(System.getProperty(_, "")) ++
          Seq(Files.size(image).toString, Files.getLastModifiedTime(image).toMillis.toString)
        Some(parts.mkString("|"))
      catch case _: java.io.IOException => None

  private[classpath] def fileName(key: String): String =
    val digest = MessageDigest.getInstance("SHA-256").digest(key.getBytes("UTF-8"))
    "jdk-" + digest.take(8).map(b => f"${b & 0xff}%02x").mkString + ".listing"

  /** Lists every class of every system module, in module-name order, through the module system; a module that cannot be read is left out and named in
    * `unreadable`.
    */
  private[classpath] def list(): JdkListing =
    val refs       = java.lang.module.ModuleFinder.ofSystem().findAll().asScala.toVector.sortBy(_.descriptor.name)
    val modules    = refs.map(_.descriptor.name).toArray
    val names      = Array.newBuilder[String]
    val owner      = Array.newBuilder[Int]
    val seen       = new java.util.HashSet[String]()
    val unreadable = List.newBuilder[String]
    refs.zipWithIndex.foreach: (ref, m) =>
      try
        val reader = ref.open()
        try
          reader
            .list()
            .iterator()
            .asScala
            .foreach: resource =>
              if resource.endsWith(".class") && resource != "module-info.class" then
                val name = resource.dropRight(6)
                if seen.add(name) then
                  names += name
                  owner += m
        finally reader.close()
      catch case _: java.io.IOException => unreadable += modules(m)
    JdkListing(modules, names.result(), owner.result(), unreadable.result())

  /** The listing in `file` if it is valid and was made for `key`; None for anything else. */
  private[classpath] def readCached(file: Path, key: String): Option[JdkListing] =
    try
      if !Files.isRegularFile(file) then None
      else
        val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(file), 1 << 16))
        try
          if in.readInt() != Magic || in.readUTF() != key then None
          else
            val modules = Array.fill(in.readInt())(in.readUTF())
            val count   = in.readInt()
            if modules.length > Short.MaxValue || count < 0 || count > 5_000_000 then None
            else
              val names = new Array[String](count)
              val owner = new Array[Int](count)
              // Reads every record in order; false as soon as one names a module that does not exist.
              @tailrec def readFrom(i: Int): Boolean =
                i >= count || {
                  val m = in.readShort()
                  owner(i) = m
                  names(i) = in.readUTF()
                  m >= 0 && m < modules.length && readFrom(i + 1)
                }
              if readFrom(0) then Some(JdkListing(modules, names, owner)) else None
        finally in.close()
    catch case _: java.io.IOException | _: RuntimeException => None

  /** Writes atomically (a temporary file, then a move) so a reader never sees half a file. */
  private[classpath] def writeCached(file: Path, key: String, listing: JdkListing): Unit =
    try
      Files.createDirectories(file.getParent)
      val tmp = Files.createTempFile(file.getParent, "jdk-", ".tmp")
      try
        val out = new DataOutputStream(new BufferedOutputStream(Files.newOutputStream(tmp), 1 << 16))
        try
          out.writeInt(Magic)
          out.writeUTF(key)
          out.writeInt(listing.modules.length)
          listing.modules.foreach(out.writeUTF)
          out.writeInt(listing.names.length)
          @tailrec def writeFrom(i: Int): Unit =
            if i < listing.names.length then
              out.writeShort(listing.moduleOf(i))
              out.writeUTF(listing.names(i))
              writeFrom(i + 1)
          writeFrom(0)
        finally out.close()
        try Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        catch case _: java.nio.file.AtomicMoveNotSupportedException => Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING)
      finally Files.deleteIfExists(tmp)
    catch case _: java.io.IOException | _: RuntimeException => ()
