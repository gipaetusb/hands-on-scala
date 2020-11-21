trait StrWriter[T]{ 
  def write(t: T): String 
}
object StrWriter{
  implicit object WriteInt extends StrWriter[Int] {
    def write(t: Int) = t.toString
  }
  implicit object WriteBool extends StrWriter[Boolean] {
    def write(t: Boolean) = t.toString
  }
  implicit object WriteDouble extends StrWriter[Double] {
    def write(t: Double) = t.toString
  }

  implicit def WriteSeq[T: StrWriter] = {
    new StrWriter[Seq[T]] {
      def write(t: Seq[T]) = {
        t.map(implicitly[StrWriter[T]].write).mkString("[", ",", "]")
      }
    }
  }

  implicit def ParseTuple[T: StrWriter, V: StrWriter] =
    new StrWriter[(T, V)] {
      def write(t: (T, V)) = {
        val left: T = t._1
        val right: V = t._2
        "[" + implicitly[StrWriter[T]].write(left) + "," + implicitly[StrWriter[V]].write(right) + "]"
      }
    }

}

def writeToString[T: StrWriter](t: T): String = implicitly[StrWriter[T]].write(t)

def writeToConsole[T](t: T)(implicit writer: StrWriter[T]): Unit = {
  scala.Console.out.println(writer.write(t))
}
