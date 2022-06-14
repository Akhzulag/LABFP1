package labfp

import scala.collection.immutable
import scala.collection.mutable

enum Set[+A]:
  case Empty
  case NonEmpty private[Set] (a: A, rest: Set[A])

  def foldLeft[A,B](set: Set[A], z: B)(f: (A,B) => B) : B =
    set match
      case Empty => z
      case NonEmpty(xh,xr) => foldLeft(xr, f(xh,z))(f)

  def reverse: Set[A] =
    def go[A](xs: Set[A], res: Set[A] = Empty ): Set[A] =
      xs match
        case Empty => res
        case NonEmpty(xh, xl) => go(xl, NonEmpty(xh, res))
    go(this)

  def unique[A](el: A): Boolean =
    foldLeft(this, true)( (xh,z) => if xh!=el then z else false)

  def insert[B >: A](el: B): Set[B] =
    def go(xs: Set[A]): Set[B] =
      if unique(el) then
        NonEmpty(el, xs.reverse)
      else
        xs
    go(this).reverse

object Set:
  def empty[A]: Set[A] = Empty
  def apply[A](xs: A*): Set[A] = of(xs*)
  def of[A](xs: A*): Set[A] =
    xs.foldLeft(Empty: Set[A]) { case (acc,x) => acc.insert(x)}


enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])

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

  def toList: List[A] =
    var listTree:List[A] = List.Nil
    def go(xs: Tree[A],subRes: List[A] = List.Nil): Unit =
      xs match
        case Empty => Nil
        case Leaf(k) =>
          listTree = new List.Cons[A](k,subRes.reverse).reverse
        case Branch(k, xl, xr) =>
          listTree = new List.Cons[A](k,subRes.reverse).reverse
          go(xl,listTree)
          go(xr,listTree)
    go(this)
    listTree

  def toSet: Set[A] =
    var setTree:Set[A] = Set.Empty
    def go(xs: Tree[A],subRes: Set[A] = Set.Empty): Set[A] =
      xs match
        case Empty => setTree
        case Leaf(k) =>
          if setTree.unique(k) then
            setTree = subRes.insert(k)
          setTree
        case Branch(k, xl, xr) =>
          if setTree.unique(k) then
            setTree = subRes.insert(k)
          go(xl,setTree)
          go(xr,setTree)
    go(this)

  def toBst(f: A => Int): BSTree=
    var treeBS:BSTree = BSTree.EmptyBS
    def go(xs: Tree[A],subRes: List[A] = List.Nil): BSTree =
      xs match
        case Empty => BSTree.EmptyBS
        case Leaf(k) =>
          treeBS = BSTree.insert(f(k),treeBS)
          treeBS
        case Branch(k, xl, xr) =>
          treeBS = BSTree.insert(f(k),treeBS)
          go(xl)
          go(xr)
          treeBS
    go(this)



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
          Branch(arr(index), go(2*index+1), {
            if (!oneIn) then
              oneIn=true
              go(2*index+2)
            else
              Empty
          })
      else
        Empty
    go(0)

