import org.junit.Test
import org.junit.Assert._
import Tree._

class TreeTest {
  val t: Tree[Int] = Branch(
    Branch(Leaf(1), Branch(Leaf(2), Leaf(3))),
      Leaf(4)
  )

  val tPlusOne: Tree[Int] = Branch(
    Branch(Leaf(2), Branch(Leaf(3), Leaf(4))),
      Leaf(5)
  )

  @Test
  def sizeTest = assertEquals(7, size(t))

  @Test
  def maxTest = assertEquals(4, maximum(t))

  @Test
  def depthTest = assertEquals(3, depth[Int](t))

  @Test
  def mapTest = assertEquals(tPlusOne, map(t)(_ + 1))

  @Test
  def fSizeTest = assertEquals(7, fSize(t))

  @Test
  def fMaxTest = assertEquals(4, fMax(t))

  @Test
  def fDepthTest = assertEquals(3, depth[Int](t))

  @Test
  def fMapTest = assertEquals(tPlusOne, fMap(t)(_ + 1))
}
