package edu.knoldus

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    def bSearch(lower: Int, upper: Int): Int = {
      if (lower > upper) return -1

      val mid = upper-lower/2

      if (array(mid) == elem) mid
      else if (array(mid) > elem) bSearch(lower, mid - 1)
      else bSearch(mid + 1, upper)
    }

    val result=bSearch(0, array.length-1)
    if (result == (-1)) false
    else true
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    if (array.isEmpty)
      false
    else if (array.head == elem)
      true
    else linearSearch(array.tail, elem)
  }

}
