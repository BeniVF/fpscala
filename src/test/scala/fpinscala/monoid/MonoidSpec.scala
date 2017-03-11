package fpinscala.monoid

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}


object MonoidSpec extends Properties("GettingStartedSpec") {

  implicit val intNumbers  = Arbitrary.arbInt.arbitrary
  implicit val boolean  = Arbitrary.arbBool.arbitrary
  implicit val option  = Arbitrary.arbOption[Int].arbitrary
  implicit val functionIntToInt  = Arbitrary.arbFunction1[Int, Int].arbitrary


  def monoidLaws[A](monoid: Monoid[A])(implicit aGen: Gen[A]) = forAll(aGen, aGen, aGen) { (x: A, y: A, z:A ) =>
    monoid.op(monoid.op(x, y), monoid.zero) == monoid.op(x, monoid.op(y, monoid.zero)) &&
      monoid.op(monoid.op(x, y), z) == monoid.op(x, monoid.op(y, z))
  }

  def endoMonoidLaws[A](monoid: Monoid[A => A])(implicit aFunGen: Gen[A => A], aGen: Gen[A]) = forAll(aFunGen, aFunGen, aFunGen, aGen) {
    (x: A => A, y: A => A, z: A => A, a: A ) =>
    val op = monoid.op(x, y)
    monoid.op(op, monoid.zero).apply(a) == monoid.op(x, monoid.op(y, monoid.zero)).apply(a) &&
      monoid.op(monoid.op(x, y), z).apply(a) == monoid.op(x, monoid.op(y, z)).apply(a)
  }

  property("int addition monoids") = monoidLaws(Monoid.intAddition)

  property("int multiplication monoids") = monoidLaws(Monoid.intMultiplication)

  property("boolean or monoids") = monoidLaws(Monoid.booleanOr)

  property("boolean and monoids") = monoidLaws(Monoid.booleanAnd)

  property("option of int monoids") = monoidLaws(Monoid.option[Int])

  property("endfunction of int monoids") = endoMonoidLaws(Monoid.endo[Int])

}


