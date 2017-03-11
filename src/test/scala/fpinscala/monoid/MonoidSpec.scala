package fpinscala.monoid

import org.scalatest.FlatSpec

import org.scalatest.Matchers._

class MonoidSpec extends FlatSpec {

  it should "fold map using monoids" in {
    Monoid.foldMap(List(1,2,3), Monoid.stringAddition)(_.toString) shouldBe "123"
    Monoid.foldMap(List("5","2","3"), Monoid.intMultiplication)(_.toInt) shouldBe 30
    Monoid.foldMap(List("5","2","3"), Monoid.intAddition)(_.toInt) shouldBe 10
  }

  it should "fold map v using monoids" in {
    Monoid.foldMapV(Vector(1,2,3), Monoid.stringAddition)(_.toString) shouldBe "123"
    Monoid.foldMapV(Vector("5","2","3"), Monoid.intMultiplication)(_.toInt) shouldBe 30
    Monoid.foldMapV(Vector("5","2","3"), Monoid.intAddition)(_.toInt) shouldBe 10
  }

}
