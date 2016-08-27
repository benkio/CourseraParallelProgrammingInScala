package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  test("balance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("", true)
  }

  test("balance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
        s"balance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("other string balanced and not balanced"){
    def check(input: String, expected: Boolean) =
      assert(balance(input.toArray) == expected,
             s"balance($input) should be $expected")

    check("(o(ui)(oeui)(ueo)(d))",true)
    check("((d((du)o))o)",true)
    check("(()uoe(((o))d()o))",true)

    check("((d((ou(((iuo))",false)
    check("(d)o)a)",false)
    check("(o()ue(o)(o(o)o",false)

  }

/*
 *  Par Copy and paste tests
 */


  test("parBalance should work for empty string") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance($input) should be $expected")

    check("", true)
  }

  test("parBalance should work for string of length 1") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance($input) should be $expected")

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("parBalance should work for string of length 2") {
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
        s"parBalance($input) should be $expected")

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("other string parBalanced and not parBalanced"){
    def check(input: String, expected: Boolean) =
      assert(parBalance(input.toArray, 4) == expected,
             s"parBalance($input) should be $expected")

    check("(o(ui)(oeui)(ueo)(d))",true)
    check("((d((du)o))o)",true)
    check("(()uoe(((o))d()o))",true)

    check("((d((ou(((iuo))",false)
    check("(d)o)a)",false)
    check("(o()ue(o)(o(o)o",false)

  }


}
