trait StrParser[T]{ 
  def parse(s: String): T
}
object StrParser{
  implicit object ParseInt extends StrParser[Int] {
    def parse(s: String) = s.toInt
  }
  implicit object ParseBool extends StrParser[Boolean] {
    def parse(s: String) = s.toBoolean
  }
  implicit object ParseDouble extends StrParser[Double] {
    def parse(s: String) = s.toDouble
  }

  implicit def ParseSeq[T: StrParser] = {
    new StrParser[Seq[T]] {
      def parse(s: String): Seq[T] = {
        splitExpression(s).map(implicitly[StrParser[T]].parse)
      }
    }
  }

  implicit def ParseTuple[T: StrParser, V: StrParser] =
    new StrParser[(T, V)] {
      def parse(s: String): (T, V) = {
        val Seq(left, right) = splitExpression(s)
        (implicitly[StrParser[T]].parse(left), implicitly[StrParser[V]].parse(right))
      }
    }

}

def parseFromString[T: StrParser](s: String): T = implicitly[StrParser[T]].parse(s)

def parseFromConsole[T: StrParser](s: String) = {
  implicitly[StrParser[T]].parse(scala.Console.in.readLine())
}

def splitExpression(s: String): Seq[String] = {
  assert(s.head == "[")
  assert(s.last == "]")
  val indices = collection.mutable.ArrayDeque.empty[Int]
  var openBrackets: Int = 0
  for(i <- Range(1, s.length-1)){
    s(i) match {
      case '[' => openBrackets += 1
      case ']' => openBrackets -= 1
      case ',' => if (openBrackets == 0) indices += 1
      case _ => //
    }
  }
  val allIndices: Seq[Int] = Seq(0) ++ indices ++ Seq(s.length - 1)
  for(i <- Range(1, allIndices.length))
    yield s.substring(allIndices(i - 1) + 1, allIndices(i))
}
