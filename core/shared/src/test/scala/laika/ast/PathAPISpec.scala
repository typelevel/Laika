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
import munit.{FunSuite, Location}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PathAPISpec extends FunSuite {

  
  private val abs_a = Root / "a"
  private val abs_b = abs_a / "b"
  private val abs_c = abs_b / "c"

  private val rel_a = CurrentTree / "a"
  private val rel_b = rel_a / "b"
  private val rel_c = rel_b / "c"

  private val up_a = Parent(1) / "a"
  private val up_b = up_a / "b"
  private val up_c = up_b / "c"
  
  
  def decoder (testName: String)(body: Any): Unit = test(s"decoder - $testName")(body)
  def parent (testName: String)(body: Any): Unit = test(s"parent - $testName")(body)
  def depth (testName: String)(body: Any): Unit = test(s"depth - $testName")(body)
  def basename (testName: String, actual: PathBase, expected: String): Unit = 
    test(s"basename - $testName") {
      assertEquals(actual.basename, expected)
    }
  def withBasename (testName: String, pathUnderTest: PathBase)(
    expectedToString: String,
    expectedName: String,
    expectedBasename: String
  ): Unit = {
    test(s"withBasename - $testName") {
      assertEquals(pathUnderTest.toString, expectedToString)
      assertEquals(pathUnderTest.name, expectedName)
      assertEquals(pathUnderTest.basename, expectedBasename)
    }
  }
  def suffix (testName: String, actual: PathBase, expected: Option[String]): Unit =
    test(s"suffix - $testName") {
      assertEquals(actual.suffix, expected)
    }
  def withSuffix (testName: String, pathUnderTest: PathBase)(
    expectedToString: String,
    expectedName: String,
    expectedSuffix: String
  ): Unit = {
    test(s"withBasename - $testName") {
      assertEquals(pathUnderTest.toString, expectedToString)
      assertEquals(pathUnderTest.name, expectedName)
      assertEquals(pathUnderTest.suffix, Some(expectedSuffix))
    }
  }
  def fragment (testName: String, actual: PathBase, expected: Option[String]): Unit =
    test(s"fragment - $testName") {
      assertEquals(actual.fragment, expected)
    }
  def withFragment (testName: String, pathUnderTest: PathBase)(
    expectedToString: String,
    expectedName: String,
    expectedFragment: String
  ): Unit = {
    test(s"withFragment - $testName") {
      assertEquals(pathUnderTest.toString, expectedToString)
      assertEquals(pathUnderTest.name, expectedName)
      assertEquals(pathUnderTest.fragment, Some(expectedFragment))
    }
  }
  def concatAbs (testName: String, actual: Path, expected: Path): Unit =
    test(s"path concatenation for absolute paths - $testName") { assertEquals(actual, expected) }

  def concatRel (testName: String, actual: RelativePath, expected: RelativePath): Unit =
    test(s"path concatenation for relative paths - $testName") { assertEquals(actual, expected) }

  def relativeTo (testName: String, actual: PathBase, expected: PathBase)(implicit loc: Location): Unit =
    test(s"relativeTo - $testName") { assertEquals(actual, expected) }

  def isSubPath (testName: String, actual: Boolean, expected: Boolean)(implicit loc: Location): Unit =
    test(s"isSubPath - $testName") { assertEquals(actual, expected) }
  
  
  decoder("absolute path with three segments") {
    assertEquals(PathBase.parse("/foo/bar/baz"), Root / "foo" / "bar" / "baz")
  }

  decoder("absolute path with one segment") {
    assertEquals(PathBase.parse("/foo"), Root / "foo")
  }

  decoder("ignore trailing slashes for absolute paths") {
    assertEquals(PathBase.parse("/foo/"), Root / "foo")
  }
  
  decoder("root path from a single slash") {
    assertEquals(PathBase.parse("/"), Root)
  }

  decoder("relative path with three segments") {
    assertEquals(PathBase.parse("foo/bar/baz"), CurrentTree / "foo" / "bar" / "baz")
  }

  decoder("relative path with one segment") {
    assertEquals(PathBase.parse("foo/"), CurrentTree / "foo")
  }

  decoder("ignore trailing slashes for relative paths") {
    assertEquals(PathBase.parse("foo/"), CurrentTree / "foo")
  }

  decoder("current document from the empty string") {
    assertEquals(PathBase.parse(""), CurrentDocument())
  }

  decoder("current document from a hash") {
    assertEquals(PathBase.parse("#"), CurrentDocument())
  }

  decoder("current relative path from a dot") {
    assertEquals(PathBase.parse("."), CurrentTree)
  }

  decoder("fragment of the current document") {
    assertEquals(PathBase.parse("#ref"), CurrentDocument("ref"))
  }

  decoder("relative path to parent with three segments") {
    assertEquals(PathBase.parse("../foo/bar/baz"), Parent(1) / "foo" / "bar" / "baz")
  }
  
  decoder("relative path to parent with one segment") {
    assertEquals(PathBase.parse("../foo/"), Parent(1) / "foo")
  }

  decoder("relative path to parent three levels up") {
    assertEquals(PathBase.parse("../../../foo/"), Parent(3) / "foo")
  }

  decoder("relative path to parent two levels up without any path segments") {
    assertEquals(PathBase.parse("../../"), Parent(2))
  }

  decoder("relative path to parent one level up without any path segments") {
    assertEquals(PathBase.parse("../"), Parent(1))
  }

  decoder("relative path to parent two levels up without trailing slash") {
    assertEquals(PathBase.parse("../.."), Parent(2))
  }

  decoder("relative path to parent one level up without trailing slash") {
    assertEquals(PathBase.parse(".."), Parent(1))
  }
  
  decoder("ignore leading slashes when RelativePath.parse is invoked directly") {
    assertEquals(RelativePath.parse("/foo/"), CurrentTree / "foo")
  }

  decoder("assume leading slashes when Path.parse is invoked directly") {
    assertEquals(Path.parse("foo/"), Root / "foo")
  }

    
  parent("Root for Root") {
    assertEquals(Root.parent, Root)
  }

  parent("Root for a path with a single segment") {
    assertEquals(abs_a.parent, Root)
  }

  parent("a path with 2 segments for an absolute path with 3 segments") {
    assertEquals(abs_c.parent, abs_b)
  }

  parent("Parent(1) for Current") {
    assertEquals(CurrentTree.parent, Parent(1))
  }

  parent("CurrentTree for CurrentDocument") {
    assertEquals(CurrentDocument("ref").parent, CurrentTree)
  }

  parent("CurrentTree for a path with a single segment") {
    assertEquals(rel_a.parent, CurrentTree)
  }

  parent("a path with 2 segments for a relative path with 3 segments") {
    assertEquals(rel_c.parent, rel_b)
  }

  parent("Parent(2) for Parent(1)") {
    assertEquals(Parent(1).parent, Parent(2))
  }

  parent("Parent(1) for a path with a single segment") {
    assertEquals(up_a.parent, Parent(1))
  }

  parent("a path with 2 segments for a parent path with 3 segments") {
    assertEquals(up_c.parent, up_b)
  }


  depth("0 for Root") {
    assertEquals(Root.depth, 0)
  }

  depth("1 for a path with one segment") {
    assertEquals(abs_a.depth, 1)
  }

  depth("3 for a path with three segments") {
    assertEquals(abs_c.depth, 3)
  }

    
  basename("/ for Root", Root, "/")

  basename(". for CurrentTree", CurrentTree, ".")

  basename("empty for CurrentDocument", CurrentDocument("ref"), "")

  basename("../ for Parent(1)", Parent(1), "../")

  basename("for an absolute path without suffix", abs_a, "a")

  basename("for a relative path without suffix", rel_a, "a")

  basename("for an absolute path with suffix", abs_c / "foo.jpg", "foo")

  basename("for an absolute path with suffix and fragment", abs_c / "foo.jpg#baz", "foo")

  basename("for a relative path with suffix", rel_c / "foo.jpg", "foo")

  basename("for a relative path with suffix and fragment", rel_c / "foo.jpg#baz", "foo")

  
  withBasename("called on an absolute path without suffix", abs_c.withBasename("d"))(
    expectedToString = "/a/b/d",
    expectedName = "d",
    expectedBasename = "d"
  )

  withBasename("called on a relative path without suffix", rel_c.withBasename("d"))(
    expectedToString = "a/b/d",
    expectedName = "d",
    expectedBasename = "d"
  )

  withBasename("called on an absolute path with suffix", (abs_c / "foo.jpg").withBasename("bar"))(
    expectedToString = "/a/b/c/bar.jpg",
    expectedName = "bar.jpg",
    expectedBasename = "bar"
  )


  withBasename("called on an absolute path with suffix and with fragment", (abs_c / "foo.jpg#baz").withBasename("bar"))(
    expectedToString = "/a/b/c/bar.jpg#baz",
    expectedName = "bar.jpg",
    expectedBasename = "bar"
  )


  withBasename("called on a relative path with suffix", (rel_c / "foo.jpg").withBasename("bar"))(
    expectedToString = "a/b/c/bar.jpg",
    expectedName = "bar.jpg",
    expectedBasename = "bar"
  )


  withBasename("called on a relative path with suffix with fragment", (rel_c / "foo.jpg#baz").withBasename("bar"))(
    expectedToString = "a/b/c/bar.jpg#baz",
    expectedName = "bar.jpg",
    expectedBasename = "bar"
  )


  suffix("empty for Root", Root, None)

  suffix("empty for CurrentTree", CurrentTree, None)

  suffix("empty for CurrentDocument", CurrentDocument("ref"), None)

  suffix("empty for Parent(1)", Parent(1), None)

  suffix("empty for an absolute path without suffix", abs_a, None)

  suffix("empty for a relative path without suffix", rel_a, None)

  suffix("defined for an absolute path with suffix", abs_c / "foo.jpg", Some("jpg"))

  suffix("use longest suffix in case of multiple dots", abs_c / "foo.tar.gz", Some("tar.gz"))

  suffix("defined for an absolute path with suffix and fragment", abs_c / "foo.jpg#baz", Some("jpg"))

  suffix("defined for a relative path with suffix", rel_c / "foo.jpg", Some("jpg"))

  suffix("defined for a relative path with suffix and fragment", rel_c / "foo.jpg#baz", Some("jpg")) 
  
  
  withSuffix("called on an absolute path without suffix", abs_c.withSuffix("foo"))(
    expectedToString = "/a/b/c.foo",
    expectedName = "c.foo",
    expectedSuffix = "foo"
  )

  withSuffix("called on an absolute path without suffix with fragment", (abs_c / "foo#baz").withSuffix("bar"))(
    expectedToString = "/a/b/c/foo.bar#baz",
    expectedName = "foo.bar",
    expectedSuffix = "bar"
  )

  withSuffix("called on a relative path without suffix", rel_c.withSuffix("foo"))(
    expectedToString = "a/b/c.foo",
    expectedName = "c.foo",
    expectedSuffix = "foo"
  )

  withSuffix("called on a relative path without suffix with fragment", (rel_c / "foo#baz").withSuffix("bar"))(
    expectedToString = "a/b/c/foo.bar#baz",
    expectedName = "foo.bar",
    expectedSuffix = "bar"
  )

  withSuffix("called on an absolute path with suffix", (abs_c / "foo.jpg").withSuffix("baz"))(
    expectedToString = "/a/b/c/foo.baz",
    expectedName = "foo.baz",
    expectedSuffix = "baz"
  )

  withSuffix("called on an absolute path with suffix and with fragment", (abs_c / "foo.jpg#baz").withSuffix("bar"))(
    expectedToString = "/a/b/c/foo.bar#baz",
    expectedName = "foo.bar",
    expectedSuffix = "bar"
  )

  withSuffix("called on a relative path with suffix", (rel_c / "foo.jpg").withSuffix("baz"))(
    expectedToString = "a/b/c/foo.baz",
    expectedName = "foo.baz",
    expectedSuffix = "baz"
  )

  withSuffix("called on a relative path with suffix with fragment", (rel_c / "foo.jpg#baz").withSuffix("bar"))(
    expectedToString = "a/b/c/foo.bar#baz",
    expectedName = "foo.bar",
    expectedSuffix = "bar"
  )


  fragment("empty for Root", Root, None)

  fragment("empty for CurrentTree", CurrentTree, None)

  fragment("empty for Parent(1)", Parent(1), None)

  fragment("empty for an absolute path without fragment", abs_a, None)

  fragment("empty for a relative path without fragment", rel_a, None)

  fragment("empty for the current document without a fragment", CurrentDocument(), None)

  fragment("defined for the current document with a fragment", CurrentDocument("baz"), Some("baz"))

  fragment("defined for an absolute path with fragment", abs_c / "foo#baz", Some("baz"))

  fragment("defined for an absolute path with suffix and fragment", abs_c / "foo.jpg#baz", Some("baz"))

  fragment("defined for a relative path with fragment", rel_c / "foo#baz", Some("baz"))

  fragment("defined for a relative path with suffix and fragment", rel_c / "foo.jpg#baz", Some("baz"))

  
  withFragment("called on an absolute path without fragment", abs_c.withFragment("baz"))(
    expectedToString = "/a/b/c#baz",
    expectedName = "c",
    expectedFragment = "baz"
  )

  withFragment("called on an absolute path without fragment with suffix", (abs_c / "foo.jpg").withFragment("baz"))(
    expectedToString = "/a/b/c/foo.jpg#baz",
    expectedName = "foo.jpg",
    expectedFragment = "baz"
  )

  withFragment("called on a relative path without fragment", rel_c.withFragment("baz"))(
    expectedToString = "a/b/c#baz",
    expectedName = "c",
    expectedFragment = "baz"
  )

  withFragment("called on a relative path without fragment with suffix", (rel_c / "foo.jpg").withFragment("baz"))(
    expectedToString = "a/b/c/foo.jpg#baz",
    expectedName = "foo.jpg",
    expectedFragment = "baz"
  )

  withFragment("called on an absolute path with fragment", (abs_c / "foo#bar").withFragment("baz"))(
    expectedToString = "/a/b/c/foo#baz",
    expectedName = "foo",
    expectedFragment = "baz"
  )

  withFragment("called on an absolute path with fragment and with suffix", (abs_c / "foo.jpg#bar").withFragment("baz"))(
    expectedToString = "/a/b/c/foo.jpg#baz",
    expectedName = "foo.jpg",
    expectedFragment = "baz"
  )

  withFragment("called on a relative path with fragment", (rel_c / "foo#bar").withFragment("baz"))(
    expectedToString = "a/b/c/foo#baz",
    expectedName = "foo",
    expectedFragment = "baz"
  )

  withFragment("called on a relative path with fragment with suffix", (rel_c / "foo.jpg#baz").withFragment("baz"))(
    expectedToString = "a/b/c/foo.jpg#baz",
    expectedName = "foo.jpg",
    expectedFragment = "baz"
  )


  concatAbs("return the same instance when invoked with CurrentTree", abs_c / CurrentTree, abs_c)

  concatAbs("return the immediate parent when invoked with Parent(1)", abs_c / Parent(1), abs_b)

  concatAbs("return Root when pointing to the top-most parent", abs_c / Parent(3), Root)

  concatAbs("two paths with segments", Root / "foo" / "bar" / (CurrentTree / "baz"), Root / "foo" / "bar" / "baz")

  concatAbs("path pointing to a parent one level up", Root / "foo" / "bar" / (Parent(1) / "baz"), Root / "foo" / "baz")

  concatAbs("path pointing to a parent two levels up", Root / "foo" / "bar" / (Parent(2) / "baz"), Root / "baz")

  concatAbs("keep the suffix when invoked with CurrentTree", (abs_b / "c.foo") / CurrentDocument(None), abs_b / "c.foo")
  
  
  concatRel("return the same instance when invoked with Current", rel_c / CurrentTree, rel_c)

  concatRel("return the immediate parent when invoked with Parent(1)", rel_c / Parent(1), rel_b)

  concatRel("return CurrentTree when pointing to the top-most parent", rel_c / Parent(3), CurrentTree)

  concatRel("return Parent(1) when pointing past the top-most parent", rel_c / Parent(4), Parent(1))

  concatRel("two paths with segments", 
    CurrentTree / "foo" / "bar" / (CurrentTree / "baz"), 
    CurrentTree / "foo" / "bar" / "baz")

  concatRel("path pointing to a parent one level up", 
    CurrentTree / "foo" / "bar" / (Parent(1) / "baz"), 
    CurrentTree / "foo" / "baz")

  concatRel("path pointing to a parent two levels up", 
    CurrentTree / "foo" / "bar" / (Parent(2) / "baz"), 
    CurrentTree / "baz")

  concatRel("append to CurrentTree", CurrentTree / (Parent(1) / "baz"), Parent(1) / "baz")

  concatRel("append to Parent(1)", Parent(1) / (Parent(1) / "baz"), Parent(2) / "baz")

  concatRel("path pointing to a parent one level up to another path pointing one level up", 
    Parent(1) / "foo" / "bar" / (Parent(1) / "baz"), 
    Parent(1) / "foo" / "baz")

  concatRel("path pointing to a parent two levels up to another path pointing one level up", 
    Parent(1) / "foo" / "bar" / (Parent(2) / "baz"), 
    Parent(1) / "baz")
  
  
  relativeTo("pointing to the same document", (Root / "a").relativeTo(Root / "a"), CurrentDocument())

  relativeTo("pointing to a parent tree", (Root / "a").relativeTo(Root / "a" / "b"), Parent(1))

  relativeTo("pointing to a document in the parent tree", (Root / "c").relativeTo(Root / "a" / "b"), Parent(1) / "c")

  relativeTo("pointing to a document in the same tree", (Root / "a" / "b").relativeTo(Root / "a" / "c"), CurrentTree / "b")

  relativeTo("path relative to Root", (Root / "a" / "b").relativeTo(Root), CurrentTree / "a" / "b")

  relativeTo("relative path pointing upwards to Root", Root.relativeTo(Root / "a" / "c"), Parent(2))

  relativeTo("relative path pointing to a fragment in the same document", (Root / "a" / "b#ref").relativeTo(Root / "a" / "b"), CurrentDocument("ref"))

  relativeTo("preserve suffix and fragment", (Root / "a" / "b.html#ref").relative, CurrentTree / "a" / "b.html#ref")


  isSubPath("true for two identical paths", abs_c.isSubPath(abs_c), true)

  isSubPath("true for two identical paths with suffix", abs_c.withSuffix("foo").isSubPath(abs_c.withSuffix("foo")), true)

  isSubPath("false for two identical paths with different suffix", abs_c.withSuffix("foo").isSubPath(abs_c.withSuffix("bar")), false)

  isSubPath("false for two identical paths when one is without suffix", abs_c.withSuffix("foo").isSubPath(abs_c), false)

  isSubPath("true for a child path", abs_c.isSubPath(abs_a), true)

  isSubPath("false for a parent path with a suffix", abs_c.isSubPath(abs_a.withSuffix("foo")), false)

  isSubPath("false for a parent path", abs_a.isSubPath(abs_c), false)

  
}
