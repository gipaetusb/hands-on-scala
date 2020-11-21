/*
 * How shall I pass implict val to functions? In particular, the ExecutionContext for futures. When do I pass it explicitly and when not? What does scala.concurrent.ExecutionContext.Implicits.global do?
*/


/* https://github.com/handsonscala/handsonscala/blob/v1/examples/6.2%20-%20GenericMergeSort/MergeSort.sc
 *
 * [T: Ordering] is the context bound syntax; this signature declares that we can take and return any
 * IndexedSeq and the items can be of any type T as long as there exist an implicit Ordering[T]
 * So, it should be equivalent to write
 * def myFunc[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {implicitly[Ordering[T]].doSomething(items)}
 * or
 * def myFunc(items: IndexedSeq)(implicit o: Ordering[T]): IndexedSeq[T] = {o.doSomething(itmes)}
*/
