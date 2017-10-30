package com.mvinesn.ninetynine

import java.util.NoSuchElementException

import org.scalatest.Inspectors._
import org.scalatest.{FlatSpec, Matchers}


class ListsSpec extends FlatSpec with Matchers {

  import com.mvinesn.ninetynine.Lists._

  "P01" should "find the last element of a list" in {
    last(List(1, 2, 3, 4, 5)) shouldBe 5
    last(List("test")) shouldBe "test"

    intercept[NoSuchElementException](last(List()))
  }


  "P02" should "find the last but one element of a list" in {
    penultimate(List(5, 4, 2, 4, 5, 6, 0)) shouldBe 6
    penultimate(List("one", "two")) shouldBe "one"

    intercept[NoSuchElementException](penultimate(List("only")))
    intercept[NoSuchElementException](penultimate(List()))
  }


  "P03" should "find the Kth element of a list" in {
    nth(3, List("zero", "one", "two", "three", "four")) shouldBe "three"
    nth(1, List(1, 2)) shouldBe 2
    nth(0, List(1, 2)) shouldBe 1

    intercept[IndexOutOfBoundsException](nth(2, List(1, 2)))
    intercept[IndexOutOfBoundsException](nth(-1, List(1, 2)))
  }


  "P04" should "find the numver of elements of a list" in {
    // length is also a keyword for matchers
    Lists.length(List()) shouldBe 0
    Lists.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }


  "P05" should "reverse a list" in {
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    reverse(List("one")) shouldBe List("one")
    reverse(List()) shouldBe List()
  }


  "P06" should "find out whether a list is a palindrome" in {
    isPalindrome(List(1, 2, 3, 2, 1)) shouldBe true
    isPalindrome(List("single")) shouldBe true
    isPalindrome(List()) shouldBe true
    isPalindrome(List(1, 2, 3)) shouldBe false
    isPalindrome(List(1, 2, 3, 4, 5, 6, 7)) shouldBe false
  }


  "P07" should "flatten a nested list structure" in {
    flatten(List(List(1, 2, 3), 2, List(3, 2, 1))) shouldBe List(1, 2, 3, 2, 3, 2, 1)
    flatten(List(List("a", "b", "c"), 2, List(2.5, 2.3, 1.0))) shouldBe List("a", "b", "c", 2, 2.5, 2.3, 1.0)
  }


  "P08" should "eliminate consecutive duplicates of list elements" in {
    compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
    compress(List(List(1, 2, 3), List(1, 2, 3), List(1, 2))) shouldBe List(List(1, 2, 3), List(1, 2))
    compress(List("ka", "chu", "chu", "chu", "qi", "fol", "fol")) shouldBe List("ka", "chu", "qi", "fol")
    compress(List(2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5, 2, 3, 4, 5, 2, 3, 4, 5)
    compress(List()) shouldBe List()
  }


  "P09" should "pack consecutive duplicates of list elements into sublists" in {
    pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }


  "P10" should "produce a run-length encoding of a list" in {
    encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }


  "P11" should "produce a modified run-length encoding" in {
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))
    encodeModified2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List(Right(4, 'a), Left('b), Right(2, 'c), Right(2, 'a), Left('d), Right(4, 'e))
  }


  "P12" should "decode a run-length encoded list" in {
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    decode(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

    decode2(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    decode2(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }


  "P13" should "produce a run-length encoding of a list" in {
    encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
  }


  "P14" should "duplicate the elements of a list" in {
    duplicate(List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }


  "P15" should "duplicate the elements of a list a given number of times" in {
    duplicateN(3, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
    duplicateN(5, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'a, 'a, 'a, 'a, 'b, 'b, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd, 'd, 'd)
    duplicateN(1, List('a, 'b, 'c, 'c, 'd)) shouldBe List('a, 'b, 'c, 'c, 'd)
    duplicateN(0, List('a, 'b, 'c, 'c, 'd)) shouldBe List()
  }


  "P16" should "drop every Nth element from a list" in {
    drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }


  "P17" should "split a list into two parts" in {
    split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe(List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    split(3, List('a, 'b, 'c)) shouldBe(List('a, 'b, 'c), List())
    split(3, List()) shouldBe(List(), List())
  }


  "P18" should "extract a slice from a list" in {
    slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g)
  }


  "P19" should "rotate a list N places to the left" in {
    rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) shouldBe List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }


  "P20" should "remove the Kth element from a list" in {
    removeAt(1, List('a, 'b, 'c, 'd)) shouldBe(List('a, 'c, 'd), 'b)
    intercept[IndexOutOfBoundsException](removeAt(4, List('a, 'b, 'c, 'd)) shouldBe(List('a, 'c, 'd), 'b))
  }


  "P21" should "insert an element at a given position into a list" in {
    insertAt('new, 1, List('a, 'b, 'c, 'd)) shouldBe List('a, 'new, 'b, 'c, 'd)
    insertAt('new, 0, List()) shouldBe List('new)
    insertAt('new, 1, List('old)) shouldBe List('old, 'new)
    insertAt('new, 4, List('a, 'b, 'c, 'd)) shouldBe List('a, 'b, 'c, 'd, 'new)
    intercept[IndexOutOfBoundsException](insertAt('new, 5, List('a, 'b, 'c, 'd)))
    intercept[IndexOutOfBoundsException](insertAt('new, -1, List('a, 'b, 'c, 'd)))
  }


  "P22" should "create a list containing all integers within a given range" in {
    range(4, 9) shouldBe List(4, 5, 6, 7, 8, 9)
    range(9, 4) shouldBe List(9, 8, 7, 6, 5, 4)
    range(1, 1) shouldBe List(1)
  }


  "P23" should "extract a given number of randomly selected elements from a list" in {

    val list = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
    val result = randomSelect(3, list)

    result.length shouldBe 3
    result.distinct shouldBe result
    forAll(result) { res => list should contain(res) }

    intercept[IllegalArgumentException](randomSelect(16, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))
  }


  "P24" should "draw N different random numbers from the set 1..M" in {
    val result = lotto(6, 49)

    result.length shouldBe 6
    result.distinct shouldBe result
    forAll(result) { res => range(1, 49) should contain(res) }

  }


  "P25" should "generate a random permutation of the elements of a list" in {
    val list = List('a, 'b, 'c, 'd, 'e, 'f)
    val result = randomPermute(list)

    result should contain theSameElementsAs list
    result.length shouldBe list.length

    // At least one out of 10 permutations should be different than the original list
    val permutations = (1 to 10).map(_ => randomPermute(list))
    val comparisons = permutations.map(perm => perm == list)
    forAtLeast(1, comparisons) { c => c shouldBe false }
  }

  "P26" should "generate the combinations of K distinct objects chosen from the N elements of a list." in {
    val list3 = List('a, 'b, 'c)
    combinations(2, list3).toSet.equals(Set(List('a, 'b), List('a, 'c), List('b, 'c))) shouldBe true

    val list5 = List('a, 'b, 'c, 'd, 'e)
    combinations(3, list5).toSet.equals(
      Set(
        List('a, 'b, 'c),
        List('a, 'b, 'd),
        List('a, 'b, 'e),
        List('a, 'c, 'd),
        List('a, 'c, 'e),
        List('a, 'd, 'e),
        List('b, 'c, 'd),
        List('b, 'c, 'e),
        List('b, 'd, 'e),
        List('c, 'd, 'e)
      )
    ) shouldBe true
  }

}
