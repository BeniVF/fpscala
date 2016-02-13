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
	val genIntArray = Gen.containerOfN[Array, Int](100,
		Gen.chooseNum(Int.MinValue, Int.MaxValue)
	)
	val genSortedIntArray = genIntArray.map(_.sorted.zipWithIndex.map{case (value, index) => value+2*index}.sorted)

	property("sorted array") = forAll(genSortedIntArray) { (a: Array[Int]) =>
		isSorted(a, (a: Int, b: Int) => a<=b) == true
	}

	property("unsorted array") = forAll(genSortedIntArray) { (c: Array[Int]) =>
		isSorted(c, (a: Int, b: Int) => a>b || b == a) == false
	}

	property("curry") = forAll { (a: Int, b: Int) =>
		val sum = (a:Int,b: Int) => a+b
		curry(sum)(a)(b) == sum(a,b)
	}

	property("uncurry") = forAll { (a: Int, b: Int) =>
		val sum = (a :Int) => (b:Int) => a+b
		uncurry(sum)(a, b) == sum(a)(b)
	}

	property("compose") = forAll { (a: Int) =>
		val add =  (a:Int) => a+1
		val duplicate =  (a:Int) => a*2
		compose(add, duplicate)(a) == add(duplicate(a))
	}
}
