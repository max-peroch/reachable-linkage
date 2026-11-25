package reachablelinkage.fixture

import java.nio.file.{Files, Path, Paths}

/** Runs a program in a real child JVM with class-load logging on, as a user would, and returns the log. */
object RunLog:
  /**
   * @param classpath what the child JVM runs with
   * @param main      a class with `public static void main(String[])`
   * @return the log file (`-Xlog:class+load`), whether or not the program failed
   */
  def of(classpath: Seq[Path], main: String): Path =
    val log  = Files.createTempFile("class-load", ".log")
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val proc = new ProcessBuilder(javaBin, s"-Xlog:class+load:file=$log", "-cp", classpath.mkString(java.io.File.pathSeparator), main)
      .redirectErrorStream(true).start()
    proc.getInputStream.readAllBytes()
    require(proc.waitFor() == 0, s"the child JVM failed: $main")
    log
