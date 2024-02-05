import java.io.{File, PrintWriter}
import java.util.concurrent.{Executors, TimeUnit, TimeoutException}
import scala.io.Source

package object util {

  def createTempFile(prefix: String, suffix: String): File = {
    val tmp = File.createTempFile(prefix, suffix)
    tmp.deleteOnExit()
    tmp
  }

  def writeToTempFile(prefix: String, suffix: String, content: String): File = {
    val tmp = createTempFile(prefix, suffix)
    writeToFile(tmp, content)
    tmp
  }

  def writeToPath(path: String, content: String): Unit = {
    writeToFile(new File(path), content)
  }

  def writeToFile(file: File, content: String): Unit = {
    new PrintWriter(file) {
      try {
        write(content)
      } finally {
        close()
      }
    }
  }

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    try source.getLines().mkString("\n")
    finally source.close
  }

  def assertSame[T](a: T, b: T, msg: String)
    (implicit same: AssertSame[T]): Unit =
  {
    same(a, b, msg)
  }

  def withExecutor[T](f: => T): T = {
    import opencl.executor._

    Executor.loadLibrary()
    Executor.init()
    try f
    finally Executor.shutdown()
  }

  def time[T](block: => T): (Long, T) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (end - start, result)
  }

  def printTime[T](msg: String, block: => T): T = {
    val (elapsed, result) = time(block)
    println(s"${msg}: ${prettyTime(elapsed)}")
    result
  }

  def prettyTime(nanoseconds: Long): String = {
    val microseconds = nanoseconds / 1000
    val µs = microseconds % 1000
    val milliseconds = microseconds / 1000
    val ms = milliseconds % 1000
    val seconds = milliseconds / 1000
    val s = seconds % 60
    val mn = seconds / 60

    val µsStr = s"${µs}µs"
    val msStr = if (ms > 0) s"${ms}ms " else ""
    val sStr = if (s > 0) s"${s}s " else ""
    val mnStr = if (mn > 0) s"${mn}mn " else ""
    s"${mnStr}${sStr}${msStr}${µsStr}"
  }

  case class MemoryStats(used: Long, total: Long) {
    def pretty(): String =
      s"used: ${prettyMem(used)}, " +
      s"total: ${prettyMem(total)}"

    def max(other: MemoryStats): MemoryStats = {
      MemoryStats(this.used max other.used, this.total max other.total)
    }
  }

  def memStats(): MemoryStats = {
    val runtime = Runtime.getRuntime
    val used = runtime.totalMemory - runtime.freeMemory
    MemoryStats(used, runtime.totalMemory)
  }

  def prettyMem(bytes: Long): String = {
    val kibibyte = bytes / 1024
    val kib = kibibyte % 1024
    val mebibyte = kibibyte / 1024
    val mib = mebibyte % 1024
    val gib = mebibyte / 1024

    val kibStr = if (kib > 0) s"${kib}KiB" else ""
    val mibStr = if (mib > 0) s"${mib}MiB " else ""
    val gibStr = if (gib > 0) s"${gib}GiB " else ""
    s"${gibStr}${mibStr}${kibStr}"
  }

  def dotPrintTmp(
    name: String,
    r: elevate.core.RewriteResult[rise.elevate.Rise]
  ): Unit = r match {
    case elevate.core.Success(p) => dotPrintTmp(name, p)
    case _ =>
  }

  def dotPrintTmp(prefix: String, e: rise.core.Expr): Unit = {
    import scala.language.postfixOps
    import scala.sys.process._

    val dotString = rise.core.dotPrinter.generateDotString(e,
      printTypes = false,
      inlineLambdaIdentifier = true,
      applyNodes = false)
    val dotFile = File.createTempFile(prefix, ".dot")
    writeToFile(dotFile, dotString)
    val dotPath = dotFile.getPath
    val svgPath = dotPath.replace(".dot", ".svg")
    (s"dot -Tsvg $dotPath -o $svgPath" !!)
    println(s"wrote $dotPath.svg and .dot")
  }
}
