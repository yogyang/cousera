package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.compiletime.ops.boolean
import scala.util.{Try,Success,Failure}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = 
    for {
      e <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(e, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h = insert(b, insert(a, empty))
    val m = if ord.lteq(a, b) then a else b
    findMin(h) == m
  }

  property("insert1") = forAll { (a: A) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h)) == true
  }


  property("sorted sequence") = forAll { (h: H) => 
    def rec(h: H): List[A] = h match {
      case h if isEmpty(h) => Nil
      case h => findMin(h) :: rec(deleteMin(h))
    }

    val l = rec(h)
    l.zip(l.tail).forall(_ <= _)
  }

  property("melding 3 times and deleting 3 mins, next min are equal") = forAll { (h: H) =>
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      isEmpty(h2) || {
        println(s"min h1:${findMin(h1)}")
        println(s"min h2:${findMin(h2)}")
        findMin(h1) == findMin(h2)
      }
  }

  property("melt") = forAll { (h1: H, h2: H) =>
    val min1 = Try(findMin(h1)).toOption
    val min2 = Try(findMin(h2)).toOption
    val expected = if (min1.isDefined && min2.isDefined) then Some(ord.min(min1.get, min2.get))
    else Seq(min1, min2).find(_.isDefined).flatten
  
    Try(findMin(meld(h1, h2))).toOption == expected
  }

