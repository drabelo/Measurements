import cmpsci220._
import cmpsci220.hw.measurement._
import org.scalatest._
import Solution._

class testSuite extends FunSuite {

test("time test") {
    assert(Solution.time((n:Int) => Thread.sleep(n), 5) <= 6)
  }

  test("time test with N") {
    assert(Solution.averageTime(2,(n:Int) => Thread.sleep(n), 5) <= 12)
  }

  test("revOrder(5)") {
  assert(revOrder(5) == List(5, 4, 3, 2, 1))
	}

	test("revOrder(0)") {
  	assert(revOrder(0) == Empty())
	}

	test("randomInts(5)") {
  	assert(length(randomInts(5)) == 5)
	}

	test("Orderedlist insertAll Test") {
  	assert(insertAllOrdList(List(5,2,6,3,8,1)) == insertAllOrdList(List(1,2,3,5,6,8)))
	}

	test("BST insertAll Test") {
		
  	assert(insertAllBST(List(5,2,6)) == insertAllBST(List(5,2,6)))
	}

	test("AVL insertAll Test") {
  	assert(insertAllAVL(List(5,2,6)) == insertAllAVL(List(6,2,5)))
	}


	test("Orderedlist is memberAll Test") {
  	assert(isMemberAllOrdList(List(1,2,3,8), insertAllOrdList(List(1,2,3,5,6,8))) == true)
	}

	test("BST is memberAll Test") {
  	assert(isMemberAllBST(List(5,2), insertAllBST(List(5,2,6))) == true)
	}

	test("AVL is memberAll Test") {
  	assert(isMemberAllAVL(List(5,2), insertAllAVL(List(5,2,6))) == true)
	}

	def timeInsertAll[A](insertAll: List[Int] => A, trials: Int)(values: List[Int]): (Double, Double) = {
  val n = length(values).toDouble
  val t = averageTime(trials, insertAll, values)
  (n, t / n)
}

test("timing insertAllAVL on random input") {
  // You may need to tweak this data to suit your computer
  val data = map((x: Int) => randomInts(x), revOrder(16))

  val timing = map(timeInsertAll(insertAllAVL, 1), data)
  println(timing)
}

test("timing insertAllBLT on random input") {
  // You may need to tweak this data to suit your computer
  println()
  val data = map((x: Int) => randomInts(x), revOrder(16))

  val timing = map(timeInsertAll(insertAllBST, 1), data)
  println(timing)
}

test("timing insertAllOrdList on random input") {
  // You may need to tweak this data to suit your computer
  println()
  val data = map((x: Int) => randomInts(x), revOrder(5))

  val timing = map(timeInsertAll(insertAllOrdList, 1), data)
  println(timing)
}

}






