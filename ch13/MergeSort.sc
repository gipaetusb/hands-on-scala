import scala.concurrent._, duration.Duration.Inf, java.util.concurrent.Executors
implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

// [T: Ordering] is the context bound syntax; this signature declares that we can take and return any
// IndexedSeq and the items can be of any type T as long as there exist an implicit Ordering[T]
// So, it's equivalent to write
// def myFunc[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {implicitly[Ordering[T]].doSomething(items)}
// or
// def myFunc(items: IndexedSeq)(implicit val o = Ordering[T]): IndexedSeq[T] = {o.doSomething(itmes)}
def mergeSortParallel[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  Await.result(mergeSortFuture(items), Inf)
}

def mergeSortFuture[T: Ordering](items: IndexedSeq[T]): Future[IndexedSeq[T]] = {
  if (items.length <= 8) Future.successful(mergeSortSequential(items))
  else {
    val (left, right) = items.splitAt(items.length / 2)
    mergeSortFuture(left).zip(mergeSortFuture(right)).map{
      case (sortedLeft, sortedRight) => merge(sortedLeft, sortedRight)
    }
  }
}

def mergeSortSequential[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    merge(mergeSortSequential(left), mergeSortSequential(right))
  }
}

def merge[T: Ordering](sortedLeft: IndexedSeq[T], sortedRight: IndexedSeq[T]): IndexedSeq[T] = {
  var (leftIdx, rightIdx) = (0, 0)
  val output = IndexedSeq.newBuilder[T]
  while (leftIdx < sortedLeft.length || rightIdx < sortedRight.length) {
    val takeLeft = (leftIdx < sortedLeft.length, rightIdx < sortedRight.length) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true) => implicitly[Ordering[T]].lt(sortedLeft(leftIdx), sortedRight(rightIdx))
    }
    if (takeLeft) {
      output += sortedLeft(leftIdx)
      leftIdx += 1
    } else {
      output += sortedRight(rightIdx)
      rightIdx += 1
    }
  }
  output.result()
}
