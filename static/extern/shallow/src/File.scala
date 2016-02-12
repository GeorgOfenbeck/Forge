package LOWERCASE_DSL_NAME.shallow

object ForgeFileReader {
  def readLines[A:Manifest](path: String)(f: String => A): ForgeArray[A] = {
    readLinesUnstructured(path)((line, buf) => ForgeArrayBuffer.array_buffer_append(buf, f(line)))
  }
  

  def readLinesUnstructured[A:Manifest](path: String)(append: (String, ForgeArrayBuffer[A]) => Unit): ForgeArray[A] = {
    val file = new java.io.File(path)
    val input = new java.io.BufferedReader(new java.io.FileReader(file))
    val out = new ForgeArrayBuffer[A](0)
    var line = input.readLine()
    while (line != null) {
      append(line, out)
      line = input.readLine()
    }
    input.close()
    out.toArray
  }
}
