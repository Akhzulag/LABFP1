package labfp


import munit.FunSuite
import scala.collection.mutable


class OptionalSuite extends FunSuite {

  test("test Tree's constructor"){
    val expected = Tree.Leaf(3)
    val actual = Tree(Array(3))
    assertEquals(actual, expected)
  }

  test("test Tree's constructor"){
    val expected = Tree.Branch(1,Tree.Leaf(0),Tree.Empty)
    val actual = Tree(Array(1,0))
    assertEquals(actual, expected)
  }

  test("test Tree's constructor"){
    val expected = Tree.Branch(1,Tree.Leaf(4),Tree.Leaf(3))
    val actual = Tree(Array(1, 4, 3))
    assertEquals(actual, expected)
  }

  test("test Tree's constructor"){
    val expected = Tree.Branch(1,Tree.Branch(4,Tree.Branch(5,Tree.Leaf(7),Tree.Leaf(8)),Tree.Leaf(6)),Tree.Branch(3,Tree.Leaf(9),Tree.Leaf(10)))
    val actual = Tree(Array(1,4,3,5,6,9,10,7,8))
    assertEquals(actual, expected)
  }


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

  test("(String) convert Tree to Set") {
    val expected = Set("Tree","BSTree", "Set","List")
    val actual = Tree(Array("Tree", "BSTree", "BSTree", "Set", "List", "List","Set")).toSet
    assertEquals(actual, expected)
  }

  test("(Empty) convert Tree to Set") {
    val expected = Set()
    val actual = Tree.Empty.toSet
    assertEquals(actual, expected)
  }

  test("extreme Tree to Set"){
    //BIG TEST
    var i:Int = 1e5.toInt
    val arr = new Array[Char](1e7.toInt)
    for(i <- 0 to 1e7.toInt-1){
      arr(i) = 's'
      arr(i) = 's'
    }
    val expected = Set('s')
    val actual = Tree(arr).toSet
    assertEquals(actual, expected)
  }

  test("extreme Tree to Set"){
    //BIG TEST
    var i:Int = 1e5.toInt
    val arr = new Array[Char](1e7.toInt)
    for(i <- 0 to 1e7.toInt-1){
      if i%2==0 then
        arr(i) = 's'
      else
        arr(i) = 'a'
    }
    val expected = Set('s','a')
    val actual = Tree(arr).toSet
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
