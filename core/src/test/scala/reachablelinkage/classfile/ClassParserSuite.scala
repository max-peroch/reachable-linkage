package reachablelinkage.classfile

import reachablelinkage.symbols.Symbols

class ClassParserSuite extends munit.FunSuite:

  private def bytesOf(c: Class[?]): Array[Byte] =
    val in = c.getClassLoader.getResourceAsStream(c.getName.replace('.', '/') + ".class")
    try in.readAllBytes()
    finally in.close()

  test("parses supertypes, members and method references") {
    val syms = new Symbols
    val info = ClassParser.parse(bytesOf(classOf[Symbols]), syms).toOption.get
    assertEquals(syms.className(info.id), "reachablelinkage/symbols/Symbols")
    assertEquals(syms.className(info.superName), "java/lang/Object")
    assert(info.methods.exists(m => syms.name(m.name) == "classId"))
    assert(info.methodRefs.length % 4 == 0)
    assert(info.methodRefs.length > 0)
  }

  test("rejects class files newer than we support") {
    val bytes = bytesOf(classOf[Symbols]).clone()
    bytes(6) = 0x7f; bytes(7) = 0x7f
    assert(ClassParser.parse(bytes, new Symbols).isLeft)
  }

  test("each way a class file can be rejected has its own error") {
    val good = bytesOf(classOf[Symbols])
    val syms = new Symbols
    assertEquals(ClassParser.parse(good.take(5), syms).left.toOption, Some(ClassFileError.Truncated))
    val newer = good.clone(); newer(6) = 0x7f; newer(7) = 0x7f
    assertEquals(ClassParser.parse(newer, syms).left.toOption, Some(ClassFileError.UnsupportedVersion(0x7f7f)))
    val damaged = good.take(40)
    assert(ClassParser.parse(damaged, syms).left.exists(_.isInstanceOf[ClassFileError.Malformed]))
    assert(ClassParser.parse(good, syms).isRight)
  }
