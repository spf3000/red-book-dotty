
enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])
}
object Tree {
  def size(t: Tree[_]): Int =
    t match
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)

  def maximum(t: Tree[Int]): Int =
    t match
      case Leaf(i) => i
      case Branch(l,r) => maximum(l) max maximum(r)

  def depth[A](t: Tree[A]): Int =
    t match
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))

  def fold[A,B](t: Tree[A], f: A => B)(g: (B,B) => B): B =
    t match
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l, f)(g), fold(r, f)(g))

  def fSize[A](t: Tree[A]): Int = fold[A,Int](t, _ => 1)((a,b) => 1 + (a + b))

  def fMax(t: Tree[Int]): Int = fold[Int,Int](t, identity)(_ max _)

  def fDepth[A](t: Tree[A]): Int = fold[A,Int](t, _ => 0)((a,b) => 1 + a max b)

  def fMap[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A,Tree[B]](t, el => Leaf(f(el)))(Branch.apply[B])

}
