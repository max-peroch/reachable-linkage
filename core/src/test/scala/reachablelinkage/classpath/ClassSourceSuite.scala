package reachablelinkage.classpath

import java.nio.file.Paths

import reachablelinkage.symbols.Symbols

class ClassSourceSuite extends munit.FunSuite:

  test("lists and reads a directory entry") {
    val dir = Paths.get(classOf[Symbols].getProtectionDomain.getCodeSource.getLocation.toURI)
    val src = ClassSource.open(dir).toOption.get
    try
      assert(src.classNames.contains("reachablelinkage/symbols/Symbols"))
      assert(src.read("reachablelinkage/symbols/Symbols").isDefined)
      assert(src.read("does/not/Exist").isEmpty)
    finally src.close()
  }

  test("opening something that is not there, or not a jar, says which") {
    val dir     = java.nio.file.Files.createTempDirectory("sources")
    val missing = dir.resolve("nope.jar")
    assertEquals(ClassSource.open(missing).left.toOption, Some(SourceError.NotFound(missing)))
    val notAJar = dir.resolve("text.jar")
    java.nio.file.Files.writeString(notAJar, "this is not a zip file")
    assert(ClassSource.open(notAJar).left.exists:
      case SourceError.CannotOpen(p, _) => p == notAJar
      case _                            => false)
  }
