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
import laika.ast.RelativePath.{CurrentDocument, CurrentTree, Parent}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathAPISpec extends AnyWordSpec 
                  with Matchers {

  
  private val abs_a = Root / "a"
  private val abs_b = abs_a / "b"
  private val abs_c = abs_b / "c"

  private val rel_a = CurrentTree / "a"
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
      PathBase.parse("foo/bar/baz") shouldBe (CurrentTree / "foo" / "bar" / "baz")
    }

    "decode a relative path with one segment" in {
      PathBase.parse("foo/") shouldBe (CurrentTree / "foo")
    }

    "ignore trailing slashes for relative paths" in {
      PathBase.parse("foo/") shouldBe (CurrentTree / "foo")
    }

    "decode the current document from the empty string" in {
      PathBase.parse("") shouldBe CurrentDocument()
    }

    "decode the current document from a hash" in {
      PathBase.parse("#") shouldBe CurrentDocument()
    }

    "decode the current relative path from a dot" in {
      PathBase.parse(".") shouldBe CurrentTree
    }

    "decode a fragment of the current document" in {
      PathBase.parse("#ref") shouldBe CurrentDocument("ref")
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
      RelativePath.parse("/foo/") shouldBe (CurrentTree / "foo")
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
      CurrentTree.parent shouldBe Parent(1)
    }

    "return CurrentTree for CurrentDocument" in {
      CurrentDocument("ref").parent shouldBe CurrentTree
    }

    "return CurrentTree for a path with a single segment" in {
      rel_a.parent shouldBe CurrentTree
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

  "The basename property" should {

    "be / for Root" in {
      Root.basename shouldBe "/"
    }

    "be . for CurrentTree" in {
      CurrentTree.basename shouldBe "."
    }

    "be empty for CurrentDocument" in {
      CurrentDocument("ref").basename shouldBe ""
    }

    "be ../ for Parent(1)" in {
      Parent(1).basename shouldBe "../"
    }

    "be defined for an absolute path without suffix" in {
      abs_a.basename shouldBe "a"
    }

    "be defined for a relative path without suffix" in {
      rel_a.basename shouldBe "a"
    }

    "be defined for an absolute path with suffix" in {
      (abs_c / "foo.jpg").basename shouldBe "foo"
    }

    "be defined for an absolute path with suffix and fragment" in {
      (abs_c / "foo.jpg#baz").basename shouldBe "foo"
    }

    "be defined for a relative path with suffix" in {
      (rel_c / "foo.jpg").basename shouldBe "foo"
    }

    "be defined for a relative path with suffix and fragment" in {
      (rel_c / "foo.jpg#baz").basename shouldBe "foo"
    }

    "be updated after calling withBasename on an absolute path without suffix" in {
      val result = abs_c.withBasename("d")
      result.toString shouldBe "/a/b/d"
      result.name shouldBe "d"
      result.basename shouldBe "d"
    }

    "be updated after calling withBasename on a relative path without suffix" in {
      val result = rel_c.withBasename("d")
      result.toString shouldBe "a/b/d"
      result.name shouldBe "d"
      result.basename shouldBe "d"
    }

    "be updated after calling withBasename on an absolute path with suffix" in {
      val result = (abs_c / "foo.jpg").withBasename("bar")
      result.toString shouldBe "/a/b/c/bar.jpg"
      result.name shouldBe "bar.jpg"
      result.basename shouldBe "bar"
    }

    "be updated after calling withBasename on an absolute path with suffix and with fragment" in {
      val result = (abs_c / "foo.jpg#baz").withBasename("bar")
      result.toString shouldBe "/a/b/c/bar.jpg#baz"
      result.name shouldBe "bar.jpg"
      result.basename shouldBe "bar"
    }

    "be updated after calling withBasename on a relative path with suffix" in {
      val result = (rel_c / "foo.jpg").withBasename("bar")
      result.toString shouldBe "a/b/c/bar.jpg"
      result.name shouldBe "bar.jpg"
      result.basename shouldBe "bar"
    }

    "be updated after calling withBasename on a relative path with suffix with fragment" in {
      val result = (rel_c / "foo.jpg#baz").withBasename("bar")
      result.toString shouldBe "a/b/c/bar.jpg#baz"
      result.name shouldBe "bar.jpg"
      result.basename shouldBe "bar"
    }
    
  }

  "The suffix property" should {

    "be empty for Root" in {
      Root.suffix shouldBe None
    }

    "be empty for CurrentTree" in {
      CurrentTree.suffix shouldBe None
    }

    "be empty for CurrentDocument" in {
      CurrentDocument("ref").suffix shouldBe None
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

    "use the longest suffix in case of multiple dots" in {
      (abs_c / "foo.tar.gz").suffix shouldBe Some("tar.gz")
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

    "be empty for CurrentTree" in {
      CurrentTree.fragment shouldBe None
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

    "be empty for the current document without a fragment" in {
      CurrentDocument().fragment shouldBe None
    }

    "be defined for the current document with a fragment" in {
      CurrentDocument("baz").fragment shouldBe Some("baz")
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

    "return the same instance when invoked with CurrentTree" in {
      abs_c / CurrentTree shouldBe abs_c
    }

    "return the parent when invoked with Parent(1)" in {
      abs_c / Parent(1) shouldBe abs_b
    }

    "return Root when invoked with Parent(3)" in {
      abs_c / Parent(3) shouldBe Root
    }
    
    "combine two paths with segments" in {
      Root / "foo" / "bar" / (CurrentTree / "baz") shouldBe (Root / "foo" / "bar" / "baz")
    }

    "concatenate a path pointing to a parent one level up" in {
      Root / "foo" / "bar" / (Parent(1) / "baz") should be(Root / "foo" / "baz")
    }

    "concatenate a path pointing to a parent two levels up" in {
      Root / "foo" / "bar" / (Parent(2) / "baz") should be(Root / "baz")
    }

    "not lose the suffix when invoked with CurrentTree" in {
      (abs_b / "c.foo") / CurrentDocument(None) shouldBe abs_b / "c.foo"
    }

  }

  "The path concatenation for relative paths" should {

    "return the same instance when invoked with Current" in {
      rel_c / CurrentTree shouldBe rel_c
    }

    "return the parent when invoked with Parent(1)" in {
      rel_c / Parent(1) shouldBe rel_b
    }

    "return CurrentTree when invoked with Parent(3)" in {
      rel_c / Parent(3) shouldBe CurrentTree
    }

    "return Parent(1) when invoked with Parent(4)" in {
      rel_c / Parent(4) shouldBe Parent(1)
    }

    "combine two paths with segments" in {
      CurrentTree / "foo" / "bar" / (CurrentTree / "baz") shouldBe (CurrentTree / "foo" / "bar" / "baz")
    }

    "concatenate a path pointing to a parent one level up" in {
      CurrentTree / "foo" / "bar" / (Parent(1) / "baz") shouldBe (CurrentTree / "foo" / "baz")
    }

    "concatenate a path pointing to a parent two levels up" in {
      CurrentTree / "foo" / "bar" / (Parent(2) / "baz") shouldBe (CurrentTree / "baz")
    }
    
    "append to CurrentTree" in {
      CurrentTree / (Parent(1) / "baz") shouldBe (Parent(1) / "baz")
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
    
    "create a relative path pointing to the same document" in {
      (Root / "a").relativeTo(Root / "a") shouldBe CurrentDocument()
    }

    "create a relative path pointing to a parent directory" in {
      (Root / "a").relativeTo(Root / "a" / "b") shouldBe Parent(1)
    }

    "create a relative path pointing to a document in the parent directory" in {
      (Root / "c").relativeTo(Root / "a" / "b") shouldBe (Parent(1) / "c")
    }

    "create a relative path pointing to a document in the same directory" in {
      (Root / "a" / "b").relativeTo(Root / "a" / "c") shouldBe (CurrentTree / "b")
    }

    "create a path relative path to Root" in {
      (Root / "a" / "b").relativeTo(Root) shouldBe (CurrentTree / "a" / "b")
    }

    "create a relative path pointing upwards to Root" in {
      Root.relativeTo(Root / "a" / "c") shouldBe Parent(2)
    }

    "create a relative path pointing to a fragment in the same document" in {
      (Root / "a" / "b#ref").relativeTo(Root / "a" / "b") shouldBe CurrentDocument("ref")
    }

    "preserve suffix and fragment" in {
      (Root / "a" / "b.html#ref").relative shouldBe (CurrentTree / "a" / "b.html#ref")
    }
  }
  
  "The isSubPath method" should {
    
    "be true for two identical paths" in {
      abs_c.isSubPath(abs_c) shouldBe true
    }

    "be true for two identical paths with suffix" in {
      abs_c.withSuffix("foo").isSubPath(abs_c.withSuffix("foo")) shouldBe true
    }

    "be false for two identical paths with different suffix" in {
      abs_c.withSuffix("foo").isSubPath(abs_c.withSuffix("bar")) shouldBe false
    }

    "be false for two identical paths when one is without suffix" in {
      abs_c.withSuffix("foo").isSubPath(abs_c) shouldBe false
    }

    "be true for a child path" in {
      abs_c.isSubPath(abs_a) shouldBe true
    }

    "be false for a parent path with a suffix" in {
      abs_c.isSubPath(abs_a.withSuffix("foo")) shouldBe false
    }

    "be false for a parent path" in {
      abs_a.isSubPath(abs_c) shouldBe false
    }
    
  }
  
}
