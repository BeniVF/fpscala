package fpinscala.errorhandling

import org.scalatest._
import Matchers._

class OptionSpec extends FlatSpec {
  import Option._

  "Option" should "be able to map" in {
    val none: Option[Double] = None
    none.map(_ * 10) shouldBe None
    Some(2).map(_ + 1) shouldBe Some(3)
    Some("4").map(_.toInt) shouldBe Some(4)
  }

  it should "be able to get or else " in {
    None.getOrElse(5) shouldBe 5
    Some(10).getOrElse(7) shouldBe 10
  }

  it should "be able to flatMap" in {
    (for {
      x <- Some(20)
      y <- Some(5)
    } yield x + y) shouldBe Some(25)
    (for {
      x <- Some("20")
      y <- None
    } yield x + y) shouldBe None
  }

  it should "be able to orElse" in {
    None orElse None shouldBe None
    None orElse Some(10) shouldBe Some(10)
    Some("a") orElse None shouldBe Some("a")
  }

  it should "be able filter" in {
  	None.filter(_ => true) shouldBe None
  	Some(2).filter(_%2 == 0) shouldBe Some(2)
  	Some("aa").filter(_.length == 3) shouldBe None
  }

  it should "calculate the variance" in {
  	variance(List()) shouldBe None
  	variance(List(2, 2)) shouldBe Some(0)
  	variance(List(8, 16)) shouldBe Some(16)
  }

  it should "be able map2 " in {
  	map2(Some(2), None)(_ + _) shouldBe None
  	map2(Some(2), Some(5))(_ + _) shouldBe Some(7)
  }

  it should "be able to sequence" in {
    sequence(List(None)) shouldBe None
    sequence(List(None, Some(1))) shouldBe None
    sequence(List(Some(3), None, Some(1))) shouldBe None   
    sequence(List(None, Some(3), None, Some(1))) shouldBe None  
    sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  it should "be able to traverse" in {
    traverse(List(2, 4, 5))(a => if (a % 2 == 0) None else Some(a)) shouldBe None
    traverse(List(2, 4, 5))(a => Some(a * 2)) shouldBe Some(List(4, 8, 10))
    
  }

}