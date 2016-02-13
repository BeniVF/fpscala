package fpinscala.gettingstarted

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object GettingStartedSpec extends Properties("GettingStartedSpec") {
	import MyModule._
	import PolymorphicFunctions._

	val positiveNumbers = Gen.posNum[Int]

	property("fibbonacci") = forAll(positiveNumbers) { (a: Int) =>
		if (a == 0 || a == 1)
			fib(a) == a
		else
			fib(a) == fib(a-1) + fib(a-2)
	}
	val genIntArray = Gen.containerOf[Array, Int](
		Gen.chooseNum(Int.MinValue, Int.MaxValue)
	)
	val genSortedIntArray = genIntArray.map(_.sorted.zipWithIndex.map{case (value, index) => value+2*index}.sorted)

	property("sorted array") = forAll(genSortedIntArray) { (a: Array[Int]) =>
		isSorted(a, (a: Int, b: Int) => a<=b) == true
	}

	property("unsorted array") = forAll(genSortedIntArray) { (c: Array[Int]) =>
		if (c.length > 2 ) 
			isSorted(c, (a: Int, b: Int) => a>b || b == a) == false
		else true
	}



}
