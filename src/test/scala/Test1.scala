import org.junit.Test
import org.junit.Assert._
import ListR._

class Test1 {
  val l = List(1, 2, 3, 4)

  @Test def tailTest =
    assertEquals(tailR(l), l.tail)

  @Test def setHeadTest =
    assertEquals(setHead(l, 7), 7 :: l.tail)

  @Test def dropTest =
    assertEquals(drop[Int](List(1, 2, 3, 4), 2), List(3, 4))

  @Test def initTest =
    assertEquals(init(l), List(1, 2, 3))

  @Test def lengthTest =
    assertEquals(length(l), 4)

  @Test def foldLeftTest =
    assertEquals(foldLeft(l, 0)(_ + _), 10)

  @Test def sumTest =
    assertEquals(sum(l), 10)

  @Test def productTest =
    assertEquals(product(l), 24)

  @Test def foldLengthTest =
    assertEquals(foldLength(l), 4)

  @Test def reverseTest =
    assertEquals(List(4, 3, 2, 1), reverse(l))

  @Test def appendTest =
    assertEquals(List(1, 2, 3, 4, 5, 6), append(l, List(5, 6)))

  @Test def appendAllTest =
    assertEquals(
      List(1, 2, 3, 4, 5, 6),
      appendAll(List(List(1, 2), List(3, 4), List(5, 6)))
    )

  @Test def addOneTest =
    assertEquals(List(2, 3, 4, 5), addOne(l))

  @Test def dblToStringTest =
    assertEquals(dblToString(List(2d, 3.0d, 4d)), List("2.0", "3.0", "4.0"))

  @Test def mapTest =
    assertEquals(List(2, 3, 4, 5), map(l)(_ + 1))

  @Test def filterTest =
    assertEquals(List(2, 4), filter(l)(_ % 2 == 0))

  @Test def flatMapTest =
    assertEquals(List(1, 1, 2, 2, 3, 3, 4, 4), flatMap(l)(i => List(i, i)))

  @Test def filterFMTest =
    assertEquals(List(2, 4), filterFM(l)(_ % 2 == 0))

  @Test def zipWithTest =
    assertEquals(List(5, 7, 9, 11),
      zipWith[Int](l, List(4, 5, 6, 7))(_+_))

  @Test def hasSubsequenceTest =
    assertTrue(hasSubsequence(l, List(2,3)))

}
