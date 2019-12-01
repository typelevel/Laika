///*
// * Copyright 2012-2019 the original author or authors.
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *      http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package laika.parse.hocon
//
//import com.typesafe.config.ConfigFactory
//import laika.config.ConfigParser
//import org.scalatest.{Matchers, WordSpec}
//
///**
//  * @author Jens Halm
//  */
//class TempHoconErrorSpec extends WordSpec with Matchers {
//
//  def tempCompare(input: String): Unit = {
//    val typesafe = try {
//      val c = ConfigFactory.parseString(input).resolve
//      s"unexpected success '${c.getString("a")}'"
//    } catch {
//      case e: Exception => e.getMessage
//    }
//    val laika = ConfigParser.parse(input).resolve.fold(
//      _.message,
//      c => s"unexpected success '${c.get[String]("a")}'"
//    )
//
//    println()
//    println(s"typesafe: $typesafe")
//    println(s"laika:    $laika")
//    println()
//  }
//
//  "The temp error messages of the HOCON parser" should {
//
//    "report an invalid escape sequence" in {
//      val input =
//        """
//          |a = "abcd \x zoo"
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report a missing closing quote in a multiline string" in {
//      val input =
//        """
//          |a = +++foo 
//          |  bar baz"
//          |
//          |b = 9
//        """.stripMargin.replaceAllLiterally("+++", "\"\"\"")
//      tempCompare(input)
//    }
//
//    "report a missing closing quote" in {
//      val input =
//        """
//          |a = "foo bar baz
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an unsupported newline in a quoted string" in {
//      val input =
//        """
//          |a = "foo 
//          |  bar baz"
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an illegal character in an unquoted string" in {
//      val input =
//        """
//          |a = foo bar ? * & = baz
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report a key without a value" in {
//      val input =
//        """
//          |a = foo bar , baz
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report a substitution value with an invalid key" in {
//      val input =
//        """
//          |a = ${b#c}
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report a substitution value with a missing closing brace" in {
//      val input =
//        """
//          |a = ${foo
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an array with an invalid value" in {
//      val input =
//        """
//          |a = [7, foo bar ? * & = baz, 9]  
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an array without closing ]" in {
//      val input =
//        """
//          |a = [7, 8, 9 
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an object with an invalid key" in {
//      val input =
//        """
//          |a {
//          |  foo bar ? baz = 5
//          |  val2 = 7
//          |}  
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an object with an invalid value" in {
//      val input =
//        """
//          |a {
//          |  val1 = foo bar ? baz
//          |  val2 = 7
//          |}    
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report an object without closing }" in {
//      val input =
//        """
//          |a {
//          |  val1 = 5
//          |  val2 = 7
//          |
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report missing '=' or ':' between key and value" in {
//      val input =
//        """
//          |a {
//          |  val1 = 5
//          |  val2 7
//          |}
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "report missing separator between field" in {
//      val input =
//        """
//          |a { val1 = 5   val2 = 7 }
//          |
//          |b = 9
//        """.stripMargin
//      tempCompare(input)
//    }
//
//    "yo yo yulli yops" in {
//      import cats.implicits._
//
//      val es = List[Either[String, String]](Right("0"), Right("b"), Right("d"))
//      print(es.sequence)
//    }
//
//  }
//
//}
