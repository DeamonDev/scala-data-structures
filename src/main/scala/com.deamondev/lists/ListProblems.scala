package com.deamondev.lists

import scala.annotation.tailrec
import java.util.Random

sealed abstract class RList[+T] { 
    def head: T 
    def tail: RList[T]
    def isEmpty: Boolean
    def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
    def apply(index: Int): T
    def length: Int 
    def reverse: RList[T]
    def ++[S >: T](anotherList: RList[S]): RList[S]
    def removeAt(index: Int): RList[T]

    // the big 3
    def map[S](f: T => S): RList[S]
    def flatMap[S](f: T => RList[S]): RList[S]
    def filter(f: T => Boolean): RList[T]

    def rle: RList[(T, Int)] 
    def duplcateEach(k: Int): RList[T]
    def rotate(k: Int): RList[T]
    def sample(k: Int): RList[T]

    //my own functions..
    def removeFirst(k: Int): RList[T]
    def removeLast(k: Int): RList[T]

}

case object RNil extends RList[Nothing] { 
    override def head: Nothing = throw new NoSuchElementException
    override def tail: RList[Nothing] = RNil
    override def isEmpty: Boolean = true

    override def toString: String = "[]"

    override def apply(index: Int): Nothing = throw new NoSuchElementException

    override def length: Int = 0

    override def reverse: RList[Nothing] = this

    override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

    override def removeAt(index: Int): RList[Nothing] = RNil

    override def map[S](f: Nothing => S): RList[S] = RNil

    override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

    override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

    override def rle: RList[(Nothing, Int)] = RNil

    override def duplcateEach(k: Int): RList[Nothing] = RNil

    override def rotate(k: Int): RList[Nothing] = RNil

    override def removeFirst(k: Int): RList[Nothing] = RNil

    override def removeLast(k: Int): RList[Nothing] = RNil

    override def sample(k: Int): RList[Nothing] = RNil
   
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] { 
    override def isEmpty: Boolean = false

    override def toString: String = { 

        @tailrec
        def toStringTailRec(remaining: RList[T], result: String): String = { 
            if (remaining.isEmpty) result 
            else if (remaining.tail.isEmpty) s"$result${remaining.head}"
            else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
        }

        "[" + toStringTailRec(this, "") + "]"
    }

    override def apply(index: Int): T = {
        
        @tailrec
        def apply0(list: RList[T], acc: (Int, Int)): T = {
            if (acc._1 == acc._2) list.head
            else apply0(list.tail, (acc._1 + 1, acc._2))
        }
        if (index < 0) throw new NoSuchElementException
        else apply0(this, (0, index))
    }

    override def length: Int = {

        @tailrec
        def length0(list: RList[T], currentIndex: Int): Int = list match { 
            case h :: t if (t != RNil) => length0(t, currentIndex + 1)
            case h :: t if (t == RNil) => currentIndex
        }

        length0(this, 1)
    }

    override def reverse: RList[T] = {

        @tailrec
        def reverse0(remainingList: RList[T], reverseBuild: RList[T]): RList[T] = { 
            if (remainingList.tail.length == 0) remainingList.head :: reverseBuild
            else reverse0(remainingList.tail, remainingList.head :: reverseBuild)
        }

        reverse0(this, RNil)
    }

    override def ++[S >: T](anotherList: RList[S]): RList[S] = {
      
        @tailrec
        def tr(remainingList: RList[S], concatenateBuild: RList[S]): RList[S] = { 
            if (remainingList.isEmpty) concatenateBuild
            else tr(remainingList.tail, remainingList.head :: concatenateBuild)
        }

        tr(this.reverse, anotherList)
    }

    override def removeAt(index: Int): RList[T] = {

        @tailrec
        def removeAt0(remainingList: RList[T], newList: RList[T], currentIndex: Int): RList[T] = { 
            if (currentIndex == this.length) newList
            else if (currentIndex == index) removeAt0(remainingList.tail, newList,currentIndex + 1)
            else removeAt0(remainingList.tail, remainingList.head :: newList, currentIndex + 1)
        }

        if (index < 0 || index >= this.length) throw new IndexOutOfBoundsException
        else removeAt0(this, RNil, 0).reverse
    }

    override def map[S](f: T => S): RList[S] = { 

        @tailrec       
        def map0(remainingList: RList[T], newList: RList[S]): RList[S] = { 
            if (remainingList.isEmpty) newList
            else map0(remainingList.tail, f(remainingList.head) :: newList)
        }

        map0(this, RNil).reverse
    }

    override def flatMap[S](f: T => RList[S]): RList[S] = { 
        @tailrec       
        def flatMap0(remainingList: RList[T], newList: RList[S]): RList[S] = { 
            if (remainingList.isEmpty) newList
            else flatMap0(remainingList.tail, f(remainingList.head) ++ newList)
        }

        flatMap0(this, RNil).reverse
    }

    override def filter(f: T => Boolean): RList[T] = { 
        
        @tailrec
        def filter0(remainingList: RList[T], newList: RList[T]): RList[T] = {
            if (remainingList.isEmpty) newList
            else if (!f(remainingList.head)) filter0(remainingList.tail, newList)
            else filter0(remainingList.tail, remainingList.head :: newList)
        }

        filter0(this, RNil).reverse
    }

    override def rle: RList[(T, Int)] = { 
        val len = this.length

        def rle0(remainingList: RList[T], newList: RList[(T, Int)], currentIndex: Int): RList[(T, Int)] = {

            if (currentIndex == len) return newList
            else { // we are in the [0, len - 1] interval 
                if (remainingList.head == newList.head._1)
                    rle0(remainingList.tail, (remainingList.head, newList.head._2 + 1) :: newList.tail, currentIndex + 1)
                else
                    rle0(remainingList.tail, (remainingList.head, 1) :: newList, currentIndex + 1)
            }
        }
        
        rle0(this.tail, (this.head, 1) :: RNil, 1).reverse
    }

    override def duplcateEach(k: Int): RList[T] = {
        val len = this.length

        @tailrec
        def duplcateEach0(remainingList: RList[T], newList: RList[T], outerIndex: Int, innerIndex: Int): RList[T] = { 
            if (outerIndex == len - 1 && innerIndex == k) remainingList.head :: newList
            else if (outerIndex != len - 1 && innerIndex == k) duplcateEach0(remainingList.tail, remainingList.head :: newList, outerIndex + 1, 1)
            else duplcateEach0(remainingList, remainingList.head :: newList, outerIndex, innerIndex + 1)
        }

        duplcateEach0(this, RNil, 0, 1).reverse
    }

    override def removeFirst(k: Int): RList[T] = { 
        val len = this.length

        @tailrec
        def removeFirst0(newList: RList[T], currentIndex: Int): RList[T] = { 
            if (currentIndex == k - 1) newList.tail
            else removeFirst0(newList.tail, currentIndex + 1)
        }

        removeFirst0(this, 0)
    }

    override def removeLast(k: Int): RList[T] = this.reverse.removeFirst(k).reverse

    override def rotate(k: Int): RList[T] = this.removeFirst(k) ++ this.removeLast(this.length - k)

    override def sample(k: Int): RList[T] = { 
        val rand = new Random()
        val len = this.length

        def sample0(newList: RList[T], currentIndex: Int): RList[T] = { 
            if (currentIndex == k) apply(rand.nextInt(len)) :: newList
            else sample0(apply(rand.nextInt(len)) :: newList, currentIndex + 1)
        }

        sample0(RNil, 1)
    }

}

object ListProblems extends App { 
    val aSmallList = 1 :: 2 :: 3 :: 4 :: 5 :: 17 :: RNil
    val aConsList = 1 :: 1 :: 1 :: 1 :: 2 :: 2 :: 3 :: 4 :: 4 :: 4 :: 5 :: 5 :: 5 :: 10:: RNil
    val anotherSmallList = 18 :: 19 :: 20 :: RNil
    val simpleList = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: RNil

    // println(aSmallList ++ anotherSmallList)
    // println(aSmallList)
    // println(aSmallList.removeAt(1))
    // println(aSmallList.map(n => "a".repeat(n)))
    // println(aSmallList.filter(_ % 2 == 0))
    // println(RNil.length)
    //println((1 :: 2 :: RNil).removeAt(0))
    //println((1 :: 2 :: RNil).removeAt(0))
    //println(aConsList.rle)
    //println(simpleList.duplcateEach(5))
    //println(simpleList.removeLast(3))
    //println(simpleList.rotate(3))
    println(simpleList.sample(3))
}