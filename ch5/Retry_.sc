def retry[T](max: Int, delay: Int = 100)(f: => T): T = {
  var tries: Int = 0
  var result: Option[T] = None
  while (result == None) {
    try { result = Some(f) }
    catch {case e: Throwable =>
      tries += 1
      if (tries > max) throw e
      else {
        println(s"failed, retry #$tries")
      }
      Thread.sleep(math.pow(delay, tries).toLong)
    }
  }
  result.get
}
