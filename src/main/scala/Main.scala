object ListR {

  def tailR[T](l: List[T]): List[T] =
    l match
      case Nil => Nil
      case h :: tail => tail

  def setHead[T](l: List[T], el: T): List[T] =
    l match
      case Nil => Nil
      case h :: tail => el :: tail

  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l else l match
      case Nil => Nil
      case h :: t => drop(t, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match
      case Nil => Nil
      case h :: t => if f(h) then dropWhile(t, f) else l

  def init[A](l: List[A]): List[A] =
    l match
      case Nil => Nil
      case h :: t :: Nil => h :: Nil
      case h :: t => h :: init(t)

  def length[A](l: List[A]): Int =
    l.foldRight(0)((a,b) => b + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    l match
      case Nil => z
      case h :: t => foldLeft(t, f(z,h))(f)

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def foldLength[A](l: List[A]) = foldLeft(l, 0)((a,b) => a + 1)

  def reverse[A](l: List[A]) =
    foldLeft(l, Nil: List[A])((b,a) => a :: b)

  def append[A](l: List[A], l2: List[A]) =
    l2.foldLeft(l)((b,a) => b :+ a)

  def appendAll[A](l: List[List[A]]): List[A] =
    l.foldLeft(Nil: List[A])(append(_,_))

  def addOne(l: List[Int]): List[Int] =
    l match
      case Nil => Nil
      case h :: t => (h + 1) :: addOne(t)

  def dblToString(l: List[Double]): List[String] =
    l match
      case Nil => Nil
      case h :: t => h.toString :: dblToString(t)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match
      case Nil => Nil
      case h :: t => f(h) :: map(t)(f)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match
      case Nil => Nil
      case h :: t => if f(h) then h :: filter(t)(f) else filter(t)(f)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    l match
      case Nil => Nil
      case h :: t => append(f(h), flatMap(t)(f))

  def filterFM[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(el => if f(el) then List(el) else Nil)

  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
    (l1, l2) match
      case (Nil, _) => Nil
      case (h :: t, h2 :: t2) => f(h, h2) :: zipWith(t, t2)(f)
      case (h :: t, Nil) => h :: t

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match
     case Nil => false
     case xs@(h :: t) =>
       xs.take(sub.length) == sub || hasSubsequence(t, sub)





}
