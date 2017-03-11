package fpinscala.datastructures
import org.scalatest._
import Matchers._

class TreeSpec extends FlatSpec {
	import Tree._
	it should "size of Tree" in {
		size(Leaf("value")) shouldBe 1
		size(Branch(Leaf("value"), Leaf("another"))) shouldBe 3
	}

}