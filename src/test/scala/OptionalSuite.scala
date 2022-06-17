package labfp


import munit.FunSuite
import scala.collection.mutable


class OptionalSuite extends FunSuite {
  test("(Int) convert Tree to List") {
    val expected = List(3, 4, 5, 6, 7)
    val actual = Tree(Array(3, 4, 7, 5, 6)).toList
    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to List") {
    val expected = List(4, 5)
    val actual = Tree(Array(4, 5)).toList
    assertEquals(actual, expected)
  }

  test("(Empty) Tree to List") {
    val expected = List()
    val actual = Tree.Empty.toList
    assertEquals(actual, expected)
  }


  test("(Int) convert Tree to Set") {
    val expected = Set(3, 4, 6, 7, 5)
    val actual = Tree(Array(3, 4, 5, 6, 7)).toSet
    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to Set") {
    val expected = Set(5, 2, 6, 0)
    val actual = Tree(Array(5, 2, 0, 6)).toSet
    assertEquals(actual, expected)
  }

  test("(Char) convert Tree to Set") {
    val expected = Set('S', 'S', 'C', 'A')
    val actual = Tree(Array('S', 'S', 'S', 'S', 'C', 'A')).toSet
    assertEquals(actual, expected)
  }

  test("(Empty) convert Tree to Set") {
    val expected = Set()
    val actual = Tree.Empty.toSet
    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to BSTree") {
    val expected = BSTree(4,3,7,1,2,5)
    val actual = Tree(Array(4,3,7,1,2,5)).toBst( x => x )
    assertEquals(actual, expected)
  }

  test("(Int) (All same) convert Tree to BSTree") {
    val expected = BSTree(4,4,4,4,4,4)
    val actual = Tree(Array(4,4,4,4,4,4)).toBst( x => x )
    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to BSTree") {
    val expected = BSTree(0,1,3,7,8,4,9,2,5,6)
    val actual = Tree(Array(0,1,2,3,4,5,6,7,8,9)).toBst( x => x )
    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to BSTree") {
    val expected = BSTree(3,1,4,0,2)
    val actual = Tree(Array('3','1','2','0','4')).toBst( x => x.toInt-48 )
    assertEquals(actual, expected)
  }

  test("(String) convert Tree to BSTree") {
    val expected = BSTree(1,2,3,5,8)
    val actual = Tree(Array[String]("o","as","thirteen","six","seven")).toBst( x => x.size )
    assertEquals(actual, expected)
  }


  test("(Int) convert Tree to BSTree") {
    val expected = BSTree(0,1,2,3,4,5)
    val actual = Tree(Array(0,1,4,2,3,5)).toBst( x => x )
    assertEquals(actual, expected)
  }

  test("(Empty) convert Tree to BSTree") {
    val expected = BSTree()
    val actual = Tree.Empty.toBst( x => x )
    assertEquals(actual, expected)
  }


  test("(Int) convert Tree to String"){
    val expected = "((1, 2), (5, ))"
    val actual = Tree(Array(4,3,7,1,2,5)).toString

    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to String"){
    val expected = "((1, 2), 7)"
    val actual = Tree(Array(4,3,7,1,2)).toString

    assertEquals(actual, expected)
  }

  test("(Int) convert Tree to String"){
    val expected = "(5, )"
    val actual = Tree(Array(4,5)).toString

    assertEquals(actual, expected)
  }

  test("(Int) convert Leaf to String"){
    val expected = "5"
    val actual = Tree(Array(5)).toString
    assertEquals(actual, expected)
  }


  test("(Empty) convert Tree to String"){
    val expected = ""
    val actual = Tree.Empty.toString
    assertEquals(actual, expected)
  }
}
