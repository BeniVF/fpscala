package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class OptionSpec extends FlatSpec {

  import Option._

  it should "get or else the value" in {
    None.getOrElse("No value!") shouldBe "No value!"
    Some(1).getOrElse(3) shouldBe 1
    Some("value").getOrElse("No value!") shouldBe "value"
  }

  it should "map" in {
    None.map(identity) shouldBe None
    Some(3).map(_ * 10) shouldBe Some(30)
    Some(3).map(_ + 10) shouldBe Some(13)
  }

  it should "flat map" in {
    for {
      x <- Some(1)
      y <- Some(2)
    } yield x + y shouldBe 3

    for {
      x <- Some(1)
      y <- None
    } yield x + y shouldBe None

  }

  it should "filter" in {
    Some(2).filter(_ % 2 == 0) shouldBe Some(2)
    Some(3).filter(_ % 2 == 0) shouldBe None
    None.filter(_ => true) shouldBe None
  }

  it should "orElse" in {
    None orElse Some(1) shouldBe Some(1)
    Some(2) orElse Some(1) shouldBe Some(2)
  }

  it should "map2" in {
    map2(Some(2), Some(5))(_ * _) shouldBe Some(10)
    map2[Int, Int, Int](Some(2), None)(_ * _) shouldBe None
  }

  it should "sequence" in {
    sequence(List(None)) shouldBe None
    sequence(List()) shouldBe Some(List())
    sequence(List(Some(1), Some(2), None)) shouldBe None
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  it should "traverse" in {
    traverse(List())(identity) shouldBe Some(List())
    val f = (x: Int) => if (x % 2 == 0) Some(x * x) else None
    traverse(List(2, 4))(f) shouldBe Some(List(4, 16))
    traverse(List(1, 2, 4))(f) shouldBe None

  }

}
