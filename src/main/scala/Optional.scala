package labfp

import scala.collection.immutable


enum Set[+A]:
  case Empty
  case NonEmpty private[Set] (a: A, rest: Set[A])

  def foldLeft[A,B](set: Set[A], z: B)(f: (A,B) => B): B =
    set match
      case Empty => z
      case NonEmpty(xh, xr) => foldLeft(xr, f(xh, z))(f)

  def contains[A](el: A): Boolean =
    foldLeft(this, true)( (xh, z) => if xh!=el then z else false)

  def insert[A](el: A, xs: Set[A] = this): Set[A] =
    if contains(el) then
      NonEmpty(el, xs)
    else
      xs


object Set:
  def empty[A]: Set[A] = Empty
  def apply[A](xs: A*): Set[A] = of(xs*)
  def of[A](xs: A*): Set[A] =
    xs.foldLeft(Empty: Set[A]) { case (acc,x) => acc.insert(x) }


enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  def insert[A](el: A, xs: List[A] = this): List[A] =
      Cons(el, xs)

  def reverse: List[A] =
    def go[A](xs: List[A], res: List[A] = Nil): List[A] =
      xs match
        case Nil => res
        case Cons(xh, xl) => go(xl, Cons(xh, res))
    go(this)


object List:
  def empty[A]: List[A] = Nil
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] =
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }.reverse


enum BSTree:
  case Empty
  case Branch private[BSTree] (key: Int ,left: BSTree, right: BSTree)

object BSTree:
  def insert(el: Int, tree: BSTree): BSTree =
    def go(xs: BSTree): BSTree =
      xs match
        case Empty => Branch(el, Empty, Empty)
        case Branch(x, xl, xr)=>
          if x > el then
            Branch(x, go(xl), xr)
          else
            Branch(x, xl, go(xr))
    go(tree)

  def apply(xs: Int*): BSTree =
    xs.foldLeft(Empty: BSTree){ (acc, x) => this.insert(x, acc) }


enum Tree[+A]:
  case Empty
  case Leaf(key: A)
  case Branch(key:A ,left: Tree[A], right: Tree[A])


  override def toString: String =
    def go(sb: StringBuilder, xs: Tree[A]): String =
      xs match
        case Empty =>
          sb.result()
        case Leaf(k) =>
          sb.append(k)
          sb.result
        case Branch(key, xl, xr) =>
          sb.append(key).append('(')
          go(if xl == Empty then sb.append(' ') else sb, xl)
          go(sb.append(',').append(' '), xr)
          sb.append(')')
          sb.result
    go(new StringBuilder(""), this)


  def foldTree[A,B](tree: Tree[A], z: B)(f: (A,B) => B): B =
  {
    tree match
      case Empty => z
      case Leaf(k) => f(k, z)
      case Branch(k, xl, xr) =>
        foldTree(xr, foldTree(xl, f(k, z))(f))(f)
  }


  def toList: List[A] =
    foldTree(this,List.Nil:List[A]){ (k,subRes) => subRes.insert(k) }

  def toSet: Set[A] =
    foldTree(this,Set.empty){ (k,subRes) => subRes.insert(k) }

  def toBst(h: A => Int): BSTree=
    foldTree(this, BSTree.Empty){ (k,subRes) => BSTree.insert(h(k),subRes) }

object Tree:
  def apply[A](arr: Array[A]): Tree[A] =
    val sizeArr:Int = arr.length
    def go(index:Int):Tree[A] =
      if index < sizeArr then
        if 2*index+1 >= sizeArr then
          Leaf(arr(index))
        else
          Branch(arr(index), go(2*index + 1), go(2*index + 2))
      else
        Empty
    go(0)

