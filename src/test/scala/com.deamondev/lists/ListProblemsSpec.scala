package test.scala.com.deamondev.lists

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import com.deamondev.lists._

class ListProblemsSpec extends AnyWordSpec {

    "The RList" when { 
        "empty" should { 
            "throw NoSuchElementException while taking a head" in { 
                val emptyRList = RNil
                assertThrows[NoSuchElementException] { 
                    emptyRList.head
                }
            }

            "throw NoSuchElementException while taking a tail" in { 
                val emptyRList = RNil
                assertThrows[NoSuchElementException] { 
                    emptyRList.tail 
                }
            }

            "have length one after appending one element to it" in { 
                val emptyRList = RNil
                val nonEmptyRList = 17 :: RNil
                assert(nonEmptyRList.length == 1)
            }

            "be empty after reverse" in { 
                val emptyRList = RNil
                assert(emptyRList.reverse == emptyRList)
            }

            "be neutral w.r.t. concatenation with another list" in { 
                val emptyRList = RNil
                val anotherList = 1 :: 2 :: RNil
                assert(anotherList ++ emptyRList == anotherList)
            }
        }

        "non empty" should { 
            "have positive length" in { 
                val nonEmptyRList = 1 :: 2 :: RNil
                assert(nonEmptyRList.length > 0)
            }

            "be equal to itself after reversing it twice" in {
                val nonEmptyRList = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
                assert(nonEmptyRList.reverse.reverse == nonEmptyRList)
            }
        }

        "equal [1,2,3,4,5]" should { 
            "after reverse equal to [5,4,3,2,1]" in { 
                val myRList = 1 :: 2 :: 3 :: 4 :: 5 :: RNil
                assert(myRList.reverse == 5 :: 4 :: 3 :: 2 :: 1 :: RNil)
            }
        }
    }
    
    //   "The Hello object" should "say hello" in {
    //     Hello.greeting shouldEqual "hello"
    //   }
}