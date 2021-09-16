package com.deamondev.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] { 
    def head: T 
    def tail: RList[T]
    def isEmpty: Boolean
    def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
    def apply(index: Int): T
    def length: Int 
    def reverse: RList[T]
    def ++[S >: T](anotherList: RList[S]): RList[S]
}

case object RNil extends RList[Nothing] { 
    override def head: Nothing = throw new NoSuchElementException
    override def tail: RList[Nothing] = throw new NoSuchElementException
    override def isEmpty: Boolean = true

    override def toString: String = "[]"

    override def apply(index: Int): Nothing = throw new NoSuchElementException

    override def length: Int = 0

    override def reverse: RList[Nothing] = this

    override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
   
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
}

object ListProblems extends App { 
    val aSmallList = 1 :: 2 :: 3 :: 4 :: 5 :: 17 :: RNil
    val anotherSmallList = 18 :: 19 :: 20 :: RNil
    println(aSmallList.length)
    //println(aSmallList.apply(2))
    println(RNil.length)
    //println(aSmallList.reverse)
    println(aSmallList ++ anotherSmallList)
}