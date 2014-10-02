import cmpsci220._
import cmpsci220.hw.measurement._

object Solution extends MeasurementFunctions {

  def time[A, B](f : A => B, a : A) : Long = {
    val startTime = System.currentTimeMillis()
    f(a)
    val endTime = System.currentTimeMillis()
    return endTime-startTime
  }

  def averageTime[A, B](n: Int, f: A => B, a: A) : Double = {
    var counter = 0
    val startTime = System.currentTimeMillis()
    for( counter <- 1 to n)
    {
      f(a)
    }
    val endTime = System.currentTimeMillis()
    endTime-startTime
  }

  def revOrder(n : Int) : List[Int] = n match {
    case 0 => Empty()
    case n => Cons(n, revOrder(n-1))
    
  }

  def randomInts(n : Int) : List[Int] = n match{
    case 0 => Empty()
    case n => Cons(util.Random.nextInt(), randomInts(n-1))
  }

  def insertAllOrdList(values: List[Int]): OrdList =  {
     insertAll(emptyOrdList, insertOrdList, values )
  }

  def insertAllBST(values: List[Int]): BST = {
     insertAll(emptyBST, insertBST, values )
  }

  def insertAllAVL(values: List[Int]): AVL = {
     insertAll(emptyAVL, insertAVL, values )
  }

  def isMemberAllOrdList(values: List[Int], lst: OrdList) : Boolean = {
    isMemberAll(isMemberOrdList, values, lst)
  }

  def isMemberAllBST(values: List[Int], bst: BST) : Boolean = {
isMemberAll(isMemberBST, values, bst)  }

  def isMemberAllAVL(values: List[Int], avl: AVL) : Boolean = {
isMemberAll(isMemberAVL, values, avl)  }

  def insertAll[T](empty : T, insert : (Int, T) => T, values: List[Int]): T = values match{
    case Empty() => empty
    case Cons(head,tail) => insertAll(insert(head, empty), insert, tail)
  }

  def isMemberAll[T](isMember : (Int, T) => Boolean,
                     values: List[Int],
                     container: T): Boolean = values match{
                    case Empty() => true
                    case Cons(head,tail) => 
                    {
                          if (isMember(head, container)){
                            isMemberAll(isMember, tail, container)
                          }
                          else
                          {
                            false
                          }
                      
                    }

                  }
}