package fpinscala.gettingstarted

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object GettingStartedSpec extends Properties("GettingStartedSpec") {
	import MyModule._

	val positiveNumbers = Gen.posNum[Int]

	property("fibbonacci") = forAll(positiveNumbers) { (a: Int) =>
		if (a == 0 || a == 1)
			fib(a) == a
		else
			fib(a) == fib(a-1) + fib(a-2)
	}

}
