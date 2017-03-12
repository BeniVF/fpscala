package fpinscala.monoid

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}


object MonoidLawsSpec extends Properties("GettingStartedSpec") {

  val intNumbers  = Arbitrary.arbInt.arbitrary
  val string = Arbitrary.arbString.arbitrary
  val boolean  = Arbitrary.arbBool.arbitrary
  val option  = Arbitrary.arbOption[Int].arbitrary
  val list  = Gen.listOf(intNumbers)
  val functionIntToInt  = Arbitrary.arbFunction1[Int, Int].arbitrary


  def monoidLaws[A](monoid: Monoid[A], aGen: Gen[A]) = forAll(aGen, aGen, aGen) { (x: A, y: A, z:A ) =>
        monoid.op(monoid.op(x, y), monoid.zero) == monoid.op(x, monoid.op(y, monoid.zero)) &&
        monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
  }

  def endoMonoidLaws[A](monoid: Monoid[A => A], aFunGen: Gen[A => A], aGen: Gen[A]) = forAll(aFunGen, aFunGen, aFunGen, aGen) {
    (x: A => A, y: A => A, z: A => A, a: A ) =>
    monoid.op( monoid.op(x, y), monoid.zero).apply(a) == monoid.op(x, monoid.op(y, monoid.zero)).apply(a) &&
      monoid.op(monoid.op(x, y), z).apply(a) == monoid.op(x, monoid.op(y, z)).apply(a)
  }

  property("int addition monoids") = monoidLaws(Monoid.intAddition, intNumbers)

  property("string addition monoids") = monoidLaws(Monoid.stringAddition, string)

  property("list addition monoids") = monoidLaws(Monoid.listAddition[Int], list)

  property("int multiplication monoids") = monoidLaws(Monoid.intMultiplication, intNumbers)

  property("boolean or monoids") = monoidLaws(Monoid.booleanOr, boolean)

  property("boolean and monoids") = monoidLaws(Monoid.booleanAnd, boolean)

  property("option of int monoids") = monoidLaws(Monoid.option[Int], option)

  property("endfunction of int monoids") = endoMonoidLaws(Monoid.endo[Int], functionIntToInt, intNumbers)

}
