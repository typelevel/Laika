/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.ast

import laika.ast.Path._
import laika.ast.RelativePath.{Current, Parent}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathAPISpec extends AnyWordSpec 
                  with Matchers {

  
  private val abs_a = Root / "a"
  private val abs_b = abs_a / "b"
  private val abs_c = abs_b / "c"

  private val rel_a = Current / "a"
  private val rel_b = rel_a / "b"
  private val rel_c = rel_b / "c"

  private val up_a = Parent(1) / "a"
  private val up_b = up_a / "b"
  private val up_c = up_b / "c"
  
  
  "The String decoder for paths" should {

    "decode an absolute path with three segments" in {
      PathBase.parse("/foo/bar/baz") shouldBe (Root / "foo" / "bar" / "baz")
    }

    "decode an absolute path with one segment" in {
      PathBase.parse("/foo") shouldBe (Root / "foo")
    }

    "ignore trailing slashes for absolute paths" in {
      PathBase.parse("/foo/") shouldBe (Root / "foo")
    }
    
    "decode the root path from a single slash" in {
      PathBase.parse("/") shouldBe Root
    }

    "decode a relative path with three segments" in {
      PathBase.parse("foo/bar/baz") shouldBe (Current / "foo" / "bar" / "baz")
    }

    "decode a relative path with one segment" in {
      PathBase.parse("foo/") shouldBe (Current / "foo")
    }

    "ignore trailing slashes for relative paths" in {
      PathBase.parse("foo/") shouldBe (Current / "foo")
    }

    "decode the current relative path from the empty string" in {
      PathBase.parse("") shouldBe Current
    }

    "decode the current relative path from a dot" in {
      PathBase.parse(".") shouldBe Current
    }

    "decode a relative path to a parent with three segments" in {
      PathBase.parse("../foo/bar/baz") shouldBe (Parent(1) / "foo" / "bar" / "baz")
    }
    
    "decode a relative path to a parent with one segment" in {
      PathBase.parse("../foo/") shouldBe (Parent(1) / "foo")
    }

    "decode a relative path to a parent three levels up" in {
      PathBase.parse("../../../foo/") shouldBe (Parent(3) / "foo")
    }

    "decode a relative path to a parent two levels up without any path segments" in {
      PathBase.parse("../../") shouldBe Parent(2)
    }

    "decode a relative path to a parent one level up without any path segments" in {
      PathBase.parse("../") shouldBe Parent(1)
    }

    "decode a relative path to a parent two levels up without trailing slash" in {
      PathBase.parse("../..") shouldBe Parent(2)
    }

    "decode a relative path to a parent one level up without trailing slash" in {
      PathBase.parse("..") shouldBe Parent(1)
    }
    
    "ignore leading slashes when RelativePath.parse is invoked directly" in {
      RelativePath.parse("/foo/") shouldBe (Current / "foo")
    }

    "assume leading slashes when Path.parse is invoked directly" in {
      Path.parse("foo/") shouldBe (Root / "foo")
    }

  }
  
  "The parent property" should {
    
    "return Root for Root" in {
      Root.parent shouldBe Root
    }

    "return Root for a path with a single segment" in {
      abs_a.parent shouldBe Root
    }

    "return a path with 2 segments for an absolute path with 3 segments" in {
      abs_c.parent shouldBe abs_b
    }

    "return Parent(1) for Current" in {
      Current.parent shouldBe Parent(1)
    }

    "return Current for a path with a single segment" in {
      rel_a.parent shouldBe Current
    }

    "return a path with 2 segments for a relative path with 3 segments" in {
      rel_c.parent shouldBe rel_b
    }

    "return Parent(2) for Parent(1)" in {
      Parent(1).parent shouldBe Parent(2)
    }

    "return Parent(1) for a path with a single segment" in {
      up_a.parent shouldBe Parent(1)
    }

    "return a path with 2 segments for a parent path with 3 segments" in {
      up_c.parent shouldBe up_b
    }
    
  }
  
  "The depth property" should {
    
    "be 0 for Root" in {
      Root.depth shouldBe 0
    }

    "be 1 for a path with one segment" in {
      abs_a.depth shouldBe 1
    }

    "be 3 for a path with three segments" in {
      abs_c.depth shouldBe 3
    }
    
  }

  "The suffix property" should {

    "be empty for Root" in {
      Root.suffix shouldBe None
    }

    "be empty for Current" in {
      Current.suffix shouldBe None
    }

    "be empty for Parent(1)" in {
      Parent(1).suffix shouldBe None
    }

    "be empty for an absolute path without suffix" in {
      abs_a.suffix shouldBe None
    }

    "be empty for a relative path without suffix" in {
      rel_a.suffix shouldBe None
    }

    "be defined for an absolute path with suffix" in {
      (abs_c / "foo.jpg").suffix shouldBe Some("jpg")
    }

    "be defined for an absolute path with suffix and fragment" in {
      (abs_c / "foo.jpg#baz").suffix shouldBe Some("jpg")
    }

    "be defined for a relative path with suffix" in {
      (rel_c / "foo.jpg").suffix shouldBe Some("jpg")
    }

    "be defined for a relative path with suffix and fragment" in {
      (rel_c / "foo.jpg#baz").suffix shouldBe Some("jpg")
    }
    
    "be defined after calling withSuffix on an absolute path without suffix" in {
      val result = abs_c.withSuffix("foo")
      result.toString shouldBe "/a/b/c.foo"
      result.name shouldBe "c.foo"
      result.suffix shouldBe Some("foo")
    }

    "be defined after calling withSuffix on an absolute path without suffix with fragment" in {
      val result = (abs_c / "foo#baz").withSuffix("bar")
      result.toString shouldBe "/a/b/c/foo.bar#baz"
      result.name shouldBe "foo.bar"
      result.suffix shouldBe Some("bar")
    }

    "be defined after calling withSuffix on a relative path without suffix" in {
      val result = rel_c.withSuffix("foo")
      result.toString shouldBe "a/b/c.foo"
      result.name shouldBe "c.foo"
      result.suffix shouldBe Some("foo")
    }

    "be defined after calling withSuffix on a relative path without suffix with fragment" in {
      val result = (rel_c / "foo#baz").withSuffix("bar")
      result.toString shouldBe "a/b/c/foo.bar#baz"
      result.name shouldBe "foo.bar"
      result.suffix shouldBe Some("bar")
    }

    "be updated after calling withSuffix on an absolute path with suffix" in {
      val result = (abs_c / "foo.jpg").withSuffix("baz")
      result.toString shouldBe "/a/b/c/foo.baz"
      result.name shouldBe "foo.baz"
      result.suffix shouldBe Some("baz")
    }

    "be updated after calling withSuffix on an absolute path with suffix and with fragment" in {
      val result = (abs_c / "foo.jpg#baz").withSuffix("bar")
      result.toString shouldBe "/a/b/c/foo.bar#baz"
      result.name shouldBe "foo.bar"
      result.suffix shouldBe Some("bar")
    }

    "be updated after calling withSuffix on a relative path with suffix" in {
      val result = (rel_c / "foo.jpg").withSuffix("baz")
      result.toString shouldBe "a/b/c/foo.baz"
      result.name shouldBe "foo.baz"
      result.suffix shouldBe Some("baz")
    }

    "be updated after calling withSuffix on a relative path with suffix with fragment" in {
      val result = (rel_c / "foo.jpg#baz").withSuffix("bar")
      result.toString shouldBe "a/b/c/foo.bar#baz"
      result.name shouldBe "foo.bar"
      result.suffix shouldBe Some("bar")
    }

  }

  "The fragment property" should {

    "be empty for Root" in {
      Root.fragment shouldBe None
    }

    "be empty for Current" in {
      Current.fragment shouldBe None
    }

    "be empty for Parent(1)" in {
      Parent(1).fragment shouldBe None
    }

    "be empty for an absolute path without fragment" in {
      abs_a.fragment shouldBe None
    }

    "be empty for a relative path without fragment" in {
      rel_a.fragment shouldBe None
    }

    "be defined for an absolute path with fragment" in {
      (abs_c / "foo#baz").fragment shouldBe Some("baz")
    }

    "be defined for an absolute path with suffix and fragment" in {
      (abs_c / "foo.jpg#baz").fragment shouldBe Some("baz")
    }

    "be defined for a relative path with fragment" in {
      (rel_c / "foo#baz").fragment shouldBe Some("baz")
    }

    "be defined for a relative path with suffix and fragment" in {
      (rel_c / "foo.jpg#baz").fragment shouldBe Some("baz")
    }

    "be defined after calling withFragment on an absolute path without fragment" in {
      val result = abs_c.withFragment("baz")
      result.toString shouldBe "/a/b/c#baz"
      result.name shouldBe "c"
      result.fragment shouldBe Some("baz")
    }

    "be defined after calling withFragment on an absolute path without fragment with suffix" in {
      val result = (abs_c / "foo.jpg").withFragment("baz")
      result.toString shouldBe "/a/b/c/foo.jpg#baz"
      result.name shouldBe "foo.jpg"
      result.fragment shouldBe Some("baz")
    }

    "be defined after calling withFragment on a relative path without fragment" in {
      val result = rel_c.withFragment("baz")
      result.toString shouldBe "a/b/c#baz"
      result.name shouldBe "c"
      result.fragment shouldBe Some("baz")
    }

    "be defined after calling withFragment on a relative path without fragment with suffix" in {
      val result = (rel_c / "foo.jpg").withFragment("baz")
      result.toString shouldBe "a/b/c/foo.jpg#baz"
      result.name shouldBe "foo.jpg"
      result.fragment shouldBe Some("baz")
    }

    "be updated after calling withFragment on an absolute path with fragment" in {
      val result = (abs_c / "foo#bar").withFragment("baz")
      result.toString shouldBe "/a/b/c/foo#baz"
      result.name shouldBe "foo"
      result.fragment shouldBe Some("baz")
    }

    "be updated after calling withFragment on an absolute path with fragment and with suffix" in {
      val result = (abs_c / "foo.jpg#bar").withFragment("baz")
      result.toString shouldBe "/a/b/c/foo.jpg#baz"
      result.name shouldBe "foo.jpg"
      result.fragment shouldBe Some("baz")
    }

    "be updated after calling withFragment on a relative path with fragment" in {
      val result = (rel_c / "foo#bar").withFragment("baz")
      result.toString shouldBe "a/b/c/foo#baz"
      result.name shouldBe "foo"
      result.fragment shouldBe Some("baz")
    }

    "be updated after calling withSuffix on a relative path with fragment with suffix" in {
      val result = (rel_c / "foo.jpg#baz").withFragment("baz")
      result.toString shouldBe "a/b/c/foo.jpg#baz"
      result.name shouldBe "foo.jpg"
      result.fragment shouldBe Some("baz")
    }

  }
  
  "The path concatenation for absolute paths" should {

    "return the same instance when invoked with Current" in {
      abs_c / Current shouldBe abs_c
    }

    "return the parent when invoked with Parent(1)" in {
      abs_c / Parent(1) shouldBe abs_b
    }

    "return Root when invoked with Parent(3)" in {
      abs_c / Parent(3) shouldBe Root
    }
    
    "combine two paths with segments" in {
      Root / "foo" / "bar" / (Current / "baz") shouldBe (Root / "foo" / "bar" / "baz")
    }

    "concatenate a path pointing to a parent one level up" in {
      Root / "foo" / "bar" / (Parent(1) / "baz") should be(Root / "foo" / "baz")
    }

    "concatenate a path pointing to a parent two levels up" in {
      Root / "foo" / "bar" / (Parent(2) / "baz") should be(Root / "baz")
    }

  }

  "The path concatenation for relative paths" should {

    "return the same instance when invoked with Current" in {
      rel_c / Current shouldBe rel_c
    }

    "return the parent when invoked with Parent(1)" in {
      rel_c / Parent(1) shouldBe rel_b
    }

    "return Root when invoked with Parent(3)" in {
      rel_c / Parent(3) shouldBe Current
    }

    "return Parent(1) when invoked with Parent(4)" in {
      rel_c / Parent(4) shouldBe Parent(1)
    }

    "combine two paths with segments" in {
      Current / "foo" / "bar" / (Current / "baz") shouldBe (Current / "foo" / "bar" / "baz")
    }

    "concatenate a path pointing to a parent one level up" in {
      Current / "foo" / "bar" / (Parent(1) / "baz") shouldBe (Current / "foo" / "baz")
    }

    "concatenate a path pointing to a parent two levels up" in {
      Current / "foo" / "bar" / (Parent(2) / "baz") shouldBe (Current / "baz")
    }
    
    "append to Current" in {
      Current / (Parent(1) / "baz") shouldBe (Parent(1) / "baz")
    }

    "append to Parent(1)" in {
      Parent(1) / (Parent(1) / "baz") shouldBe (Parent(2) / "baz")
    }

    "concatenate a path pointing to a parent one level up to another path pointing one level up" in {
      Parent(1) / "foo" / "bar" / (Parent(1) / "baz") shouldBe (Parent(1) / "foo" / "baz")
    }

    "concatenate a path pointing to a parent two levels up to another path pointing one level up" in {
      Parent(1) / "foo" / "bar" / (Parent(2) / "baz") shouldBe (Parent(1) / "baz")
    }

  }

  "The relativeTo method" should {
    
    "create a relative path pointing to the same directory" in {
      (Root / "a").relativeTo(Root / "a") shouldBe Current
    }

    "create a relative path pointing to a parent directory" in {
      (Root / "a").relativeTo(Root / "a" / "b") shouldBe Parent(1)
    }

    "create a relative path pointing to a child directory" in {
      (Root / "a" / "b").relativeTo(Root / "a") shouldBe (Current / "b")
    }

    "create a relative path pointing to a sibling directory" in {
      (Root / "a" / "b").relativeTo(Root / "a" / "c") shouldBe (Parent(1) / "b")
    }

    "create a path relative path to Root" in {
      (Root / "a" / "b").relativeTo(Root) shouldBe (Current / "a" / "b")
    }

    "create a relative path pointing upwards to Root" in {
      Root.relativeTo(Root / "a" / "c") shouldBe Parent(2)
    }

    "create a relative path pointing to a fragment in the same document" in {
      (Root / "a" / "b#ref").relativeTo(Root / "a" / "b") shouldBe Current.withFragment("ref")
    }

    "preserve suffix and fragment" in {
      (Root / "a" / "b.html#ref").relative shouldBe (Current / "a" / "b.html#ref")
    }
  }
  
  "The isSubPath method" should {
    
    "be true for two identical paths" in {
      abs_c.isSubPath(abs_c) shouldBe true
    }

    "be true for a child path" in {
      abs_c.isSubPath(abs_a) shouldBe true
    }

    "be false for a parent path" in {
      abs_a.isSubPath(abs_c) shouldBe false
    }
    
  }
  
}
