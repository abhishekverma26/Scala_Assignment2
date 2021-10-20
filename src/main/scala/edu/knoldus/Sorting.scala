package edu.knoldus

import scala.util.control.Breaks.{break, breakable}

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {
    for ( i <- 1 until array.length) {
      breakable {
        for ( j <- (1 to i).reverse) {
          if (array(j-1) < array(j)) {
            break
          } else {
            val temp = array(j)
            array(j) = array(j-1)
            array(j-1) = temp
          }
        }
      }
    }
    array
  }

  def selectionSort(array: Array[Int], first: Int = 0, second:Int = 0): Array[Int] = {
    if (first >= array.length - 1) array

    else if (second >= array.length - 1) {
      selectionSort(array, first + 1)
    }

    else {
      for (j <- first until array.length) {
        if (array(j) < array(second)) {
          val temp = array(second)
          array(second) = array(j)
          array(j) = temp
        }
      }
      selectionSort(array, first, second + 1)
    }
    array
  }

  def bubbleSort(array: Array[Int]): Array[Int] = {
    var Swapping = false

    for (i <- 0 until array.length - 1)
      if (array(i + 1) < array(i)) {
        val temp = array(i)
        array(i) = array(i + 1)
        array(i + 1) = temp
        Swapping = true
      }

    if (Swapping) bubbleSort(array)
    else array

  }

}