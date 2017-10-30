package com.mvinesn.ninetynine

import scala.annotation.tailrec


object Lists {

  //P01: Find the last element of a list.
  @tailrec
  def last[T](list: List[T]): T = {
    list match {
      case head :: Nil => head
      case head :: tail => last(tail)
      case Nil => throw new NoSuchElementException(s"Empty list doesn't have last element")
    }
  }


  // P02: Find the last but one element of a list.
  @tailrec
  def penultimate[T](list: List[T]): T = {
    list match {
      case head :: tailHead :: Nil => head
      case head :: tail => penultimate(tail)
      case _ => throw new NoSuchElementException(s"The list has too few elements")
    }
  }


  // P03: Find the Kth element of a list
  @tailrec
  def nth[T](n: Int, list: List[T]): T = {
    (n, list) match {
      case (m, lst) if m < 0 => throw new IndexOutOfBoundsException(s"negative index")
      case (0, x :: tail) => x
      case (m, x :: tail) => nth(m - 1, tail)
      case (m, Nil) => throw new IndexOutOfBoundsException(s"the array has fewer than $n elements")
    }
  }


  // P04: Find the number of elements of a list
  def length[T](list: List[T]): Int = {

    @tailrec
    def lengthRec(list: List[T], result: Int): Int = {
      list match {
        case Nil => result
        case head :: tail => lengthRec(tail, result + 1)
      }
    }
    lengthRec(list, 0)
  }


  // P05: Reverse a list.
  def reverse[T](list: List[T]): List[T] = {

    @tailrec
    def reverseRec(list: List[T], result: List[T]): List[T] = {
      list match {
        case Nil => result
        case head :: tail => reverseRec(tail, head :: result)
      }
    }
    reverseRec(list, List())
  }


  // P06: Find out whether a list is a palindrome
  def isPalindrome[T](list: List[T]): Boolean = {
    reverse(list) == list
  }


  // P07: Flatten a nested list structure
  def flatten(list: List[Any]): List[Any] =
    list.flatMap {
      case lst: List[_] => flatten(lst)
      case notList => List(notList)
    }


  // P08: Eliminate consecutive duplicates of list elements
  // If a list contains repeated elements they should be replaced with a single copy of the element.
  // The order of the elements should not be changed.
  def compress[T](list: List[T]): List[T] = {

    @tailrec
    def compressRecursive(lst: List[T], previous: Option[T], compressed: List[T]): List[T] = {

      (lst, previous) match {

        case (Nil, _) => compressed

        case (head :: tail, Some(prev)) if head == prev => compressRecursive(tail, Some(prev), compressed)

        // The following case produce the same result, i.e. don't compress and set the current head as the
        // previous element for next iteration:
        //   (head:: tail, Some(prev)) if head != prev
        //   (head:: tail, None)
        case (head :: tail, _) => compressRecursive(tail, Some(head), compressed ::: List(head))
      }
    }
    compressRecursive(list, Option.empty[T], List())
  }


  // P09: Pack consecutive duplicates of list elements into sublists
  // If a list contains repeated elements they should be placed in separate sublists.
  def pack[T](list: List[T]): List[List[T]] = {

    @tailrec
    def packRecursive(lst: List[T], previous: Option[T], packed: List[List[T]]): List[List[T]] = {

      (lst, previous) match {

        case (Nil, _) => packed

        // In this case we prepend the list. As we may need to add the next element to a list, it seems easier
        // to handle in the head. Not the most efficient as we will need to reverse the list at the end.
        case (head :: tail, Some(prev)) if prev == head =>
          packRecursive(tail, Some(head), List(packed.head ::: List(head)) ::: packed.tail)

        // Same argument as in P08: Do not add to the current list of lists, but create a new one and add it to the result.
        // As in the above case we prepend the list.
        // Valid for the following cases:
        //   (head:: tail, Some(prev)) if head != prev
        //   (head:: tail, None)
        case (head :: tail, _) => packRecursive(tail, Some(head), List(List(head)) ::: packed)
      }
    }

    packRecursive(list, Option.empty[T], List()).reverse
  }


  // P10: Run-length encoding of a list
  // Use the result of problem P09 to implement the so-called run-length encoding data compression method.
  // Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E
  def encode[T](list: List[T]): List[(Int, T)] =
    pack(list).map(lst => (length(lst), lst.head))


  // P11: Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the
  // result list. Only elements with duplicates are transferred as (N, E) terms.
  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list).map {
      case (1, e) => e
      case (n, e) => (n, e)
    }
  }

  // P11: Another solution to P11 is to use Either
  def encodeModified2[T](list: List[T]): List[Either[T, (Int, T)]] = {
    encode(list).map {
      case (1, e) => Left(e)
      case (n, e) => Right((n, e))
    }
  }


  // P12: Decode a run-length encoded list
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version
  def decode[T](list: List[(Int, T)]): List[T] = {

    @tailrec
    def decodeRec(list: List[(Int, T)], result: List[T]): List[T] =
      list match {
        case Nil => result
        case (n, e) :: tail => decodeRec(list.tail, result ++ (for (i <- 1 to n) yield e).toList)
      }

    decodeRec(list, List())
  }


  // P12: Another solution
  def decode2[T](list: List[(Int, T)]): List[T] = {
    list.flatMap { case (n, e) => List.fill(n)(e)}
  }


  // P13: Run-length encoding of a list (direct solution)
  // Implement the so-called run-length encoding data compression method directly. I.e. don't use other
  // methods you've written (like P09's pack); do all the work directly.
  def encodeDirect[T](list: List[T]): List[(Int, T)] = {

    @tailrec
    def encodeDirectRecursive(list: List[T], result: List[(Int, T)]): List[(Int, T)] = {
      list match {
        case Nil => result
        case head :: tail =>
          val (prefix, suffix) = list.span(elem => elem == head)
          encodeDirectRecursive(suffix, result ::: List((prefix.length, head)))
      }
    }

    encodeDirectRecursive(list, List())
  }


  // P14: Duplicate the elements of a list
  def duplicate[T](list: List[T]): List[T] = list.flatMap(elem => List(elem, elem))


  // P15:  Duplicate the elements of a list a given number of times.
  def duplicateN[T](n: Int, list: List[T]): List[T] = list.flatMap(elem => List.fill(n)(elem))

  // TODO exceptions for invalid n
  // P16: Drop every Nth element from a list.
  def drop[T](n: Int, list: List[T]): List[T] = {

    @tailrec
    def dropRecursive(n: Int, list: List[T], result: List[T]): List[T] = list match {
      case Nil => result
      case head :: tail => dropRecursive(n, list.drop(n), result ::: list.take(n - 1))
    }

    dropRecursive(n, list, List())
  }

  // TODO exceptions for invalid n
  // P17: Split a list into two parts
  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {

    @tailrec
    def splitRecursive(k: Int, list: List[T], prefix: List[T]): (List[T], List[T]) =
      list match {
        case Nil => (prefix, Nil)
        case head :: tail if k > 0 => splitRecursive(k - 1, tail, prefix ::: List(head))
        case lst => (prefix, lst)
      }

    splitRecursive(n, list, List())
  }

  // P18: Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the elements from and
  // including the Ith element up to but not including the Kth element of the original
  // list. Start counting the elements with 0.

  // TODO exceptions for invalid i, k
  def slice[T](i: Int, k: Int, list: List[T]): List[T] = {

    @tailrec
    def sliceRecursive(idx: Int, list: List[T], result: List[T]): List[T] =
      list match {
        case Nil => result
        case head :: tail if idx < i => sliceRecursive(idx + 1, tail, result)
        case head :: tail if idx < k => sliceRecursive(idx + 1, tail, result ::: List(head))
        case _ => result
      }

    sliceRecursive(0, list, List())
  }


  // P19: Rotate a list N places to the left
  def rotate[T](n: Int, list: List[T]): List[T] = {

    val offset = n % list.length

    if (n == 0)
      list
    else if (n > 0)
      list.takeRight(list.length - offset) ::: list.take(offset)
    else
      list.takeRight(-offset) ::: list.take(list.length + offset)
  }


  // P20: Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. Elements are numbered from 0.
  def removeAt[T](n: Int, list: List[T]): (List[T], T) = {
    if (n > list.length - 1) throw new IndexOutOfBoundsException(s"$n cannot be larger than ${list.length - 1}")

    @tailrec
    def removeAtRecursive(idx: Int, list: List[T], result: (List[T], T)): (List[T], T) = list match {
      case Nil => result
      case head :: tail if idx < n => removeAtRecursive(idx + 1, tail, (result._1 ::: List(head), head))
      case head :: tail => (result._1 ::: tail, head)
    }

    removeAtRecursive(0, list, (List(), list.head))
  }


  // P21: Insert an element at a given position into a list
  def insertAt[T](x: T, n: Int, list: List[T]): List[T] = {
    if (n < 0 || n > list.length) throw new IndexOutOfBoundsException(s"n cannot be less than 0 nor greater than ${list.length}")

    val (prefix, suffix) = list.splitAt(n)
    prefix ::: List(x) ::: suffix
  }


  // P22: create a list containing all integers within a given range
  def range(i: Int, k: Int): List[Int] = {

    @tailrec
    def rangeRec(m: Int, n: Int, result: List[Int]): List[Int] =
      if (m == n)
        result ::: List(m)

      else if (m < n)
        rangeRec(m + 1, n, result ::: List(m))

      else
        rangeRec(m - 1, n, result ::: List(m))

    rangeRec(i, k, List())
  }


  // P23: Extract a given number of randomly selected elements from a list
  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    require(n <= list.length, s"Cannot extract $n elements from a list with only ${list.length} elements")
    import util.Random
    val rand = new Random()

    @tailrec
    def randomSelectRecursive(m: Int, theList: List[T], result: List[T]): List[T] = {
      if (m <= 0)
        result
      else {
        val idx = rand.nextInt(theList.length)
        val (newList, elem) = removeAt(idx, theList)
        randomSelectRecursive(m - 1, newList, result ::: List(elem))
      }
    }

    randomSelectRecursive(n, list, List())
  }


  // P24: Lotto: Draw N different random numbers from the set 1..M
  def lotto(n: Int, m: Int): List[Int] = randomSelect(n, range(1, m))


  // P25: Generate a random permutation of the elements of a list
  // Hint: Use the solution of problem P23
  def randomPermute[T](list: List[T]): List[T] = randomSelect(list.length, list)


  // P26: Generate the combinations of K distinct objects chosen from the N elements of a list.
  // Easy solution: use the standard library
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    list.combinations(n).toStream.toList
  }


}
