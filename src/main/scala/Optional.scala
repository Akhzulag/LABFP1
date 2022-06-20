package labfp

import scala.collection.immutable


enum Set[+A]:
  case Empty
  case NonEmpty private[Set] (a: A, rest: Set[A])

  def foldLeft[A,B](set: Set[A], z: B)(f: (A,B) => B) : B =
    set match
      case Empty => z
      case NonEmpty(xh,xr) => foldLeft(xr, f(xh,z))(f)

  def unique[A](el: A): Boolean =
    foldLeft(this, true)( (xh,z) => if xh!=el then z else false)

  def insert[B >: A](el: B): Set[B] =
    def go(xs: Set[A]): Set[B] =
      if unique(el) then
        NonEmpty(el, xs)
      else
        xs
    go(this)

object Set:
  def empty[A]: Set[A] = Empty
  def apply[A](xs: A*): Set[A] = of(xs*)
  def of[A](xs: A*): Set[A] =
    xs.foldLeft(Empty: Set[A]) { case (acc,x) => acc.insert(x)}


enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

  def insert[B >: A](el: B): List[B] =
    def go(xs: List[A]): List[B] =
      Cons(el, xs.reverse).reverse
    go(this)

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
    xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }


case class BSTree private[BSTree] (v: Option[Int],l: Option[BSTree], r: Option[BSTree])

object BSTree:
  def EmptyBS: BSTree=
    new BSTree(None,None,None)

  def insert(el: Int, tree: BSTree): BSTree =
    def go(xs: Option[BSTree]): BSTree =
      xs match
        case Some(None, None, None) => new BSTree(Some(el), None, None)
        case None => new BSTree(Some(el),None, None)
        case Some(Some(x), xl, xr) =>
          if x > el  then
            BSTree(Option(x), Option(go(xl)), xr)
          else
            BSTree(Option(x), xl, Option(go(xr)))
    go(Option(tree))

  def apply(xs: Int*): BSTree =
    xs.foldLeft(EmptyBS: BSTree){ (acc,x) => this.insert(x,acc)}


enum Tree[+A]:
  case Empty
  case Leaf(key: A)
  case Branch(key:A ,left: Tree[A], right: Tree[A])



  override def toString: String =
    def go(sb: StringBuilder, xs: Tree[A]): String = {
      xs match {
        case Empty =>
          sb.result()
        case Leaf(k) =>
          sb.append(k)
          sb.result
        case Branch(key,xl,xr) =>
          go(sb.append('('), xl)
          go(sb.append(',').append(' '), xr)
          sb.append(')')
          sb.result
      }
    }
    go(new StringBuilder(""), this)

  def foldTree[A,B](tree: Tree[A], z: B)(f: (A,B) => B) : B=
  {
    tree match
      case Empty => z
      case Leaf(k) => f(k,z)
      case Branch(k, xl, xr) =>
        foldTree(xr, foldTree(xl,f(k,z))(f))(f)
  }

  def toList: List[A] =
    foldTree(this,List.Nil:List[A]){ (k,subRes) => subRes.insert(k) }

  def toSet: Set[A] =
    foldTree(this,Set.empty){ (k,subRes) =>
      if (subRes.unique(k)) then
        subRes.insert(k)
      else
        subRes
    }

  def toBst(h: A => Int): BSTree=
    foldTree(this, BSTree.EmptyBS){ (k,subRes) => BSTree.insert(h(k),subRes) }

object Tree:
  def apply[A](arr: Array[A]): Tree[A] =
    var index:Int = 0
    var size:Int = arr.size
    def go(index:Int):Tree[A] =
      var oneIn = false
      if (index < size)
        if(2*index+1 > size-1) then
          Leaf(arr(index))
        else
          Branch(arr(index), go(2*index+1), go(2*index+2))
      else
        Empty
    go(0)

