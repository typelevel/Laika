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
import org.scalatest.{FlatSpec, Matchers}

class PathAPISpec extends FlatSpec 
                  with Matchers {

  
  "The Path API" should "construct an absolute path from a string" in {
    PathBase.decode("/foo/bar/baz") should be (Root / "foo" / "bar" / "baz")
  }
  
  it should "ignore trailing spaces" in {
    PathBase.decode("/foo/bar/baz/") should be (Root / "foo" / "bar" / "baz")
  }
  
  it should "construct the root path from a single slash" in {
    PathBase.decode("/") should be (Root)
  }
  
  it should "construct an absolute path with one path element" in {
    PathBase.decode("/foo/") should be (Root / "foo")
  }
  
  it should "construct a relative path from a string" in {
    PathBase.decode("foo/bar/baz") should be (Current / "foo" / "bar" / "baz")
  }
  
  it should "construct a relative path with one path element" in {
    PathBase.decode("foo/") should be (Current / "foo")
  }
  
  it should "construct an empty relative path from the empty string" in {
    PathBase.decode("") should be (Current)
  }
  
  it should "construct a relative path to a parent with one path element" in {
    PathBase.decode("../foo/") should be (Parent(1) / "foo")
  }
  
  it should "construct a relative path to a parent with three path elements" in {
    PathBase.decode("../foo/bar/baz") should be (Parent(1) / "foo" / "bar" / "baz")
  }
  
  it should "construct a relative path to a parent three levels up" in {
    PathBase.decode("../../../foo/") should be (Parent(3) / "foo")
  }
  
  it should "construct a relative path to a parent two levels up without any path elements" in {
    PathBase.decode("../../") should be (Parent(2))
  }
  
  it should "construct a relative path to a parent one level up without any path elements" in {
    PathBase.decode("../") should be (Parent(1))
  }
  
  it should "construct a relative path to a parent two levels up without trailing slash" in {
    PathBase.decode("../..") should be (Parent(2))
  }
  
  it should "construct a relative path to a parent one level up without trailing slash" in {
    PathBase.decode("..") should be (Parent(1))
  }
  
  it should "combine an absolute and a relative path" in {
    Root / "foo" / "bar" / (Current / "baz") should be (Root / "foo" / "bar" / "baz")
  }
  
  it should "combine an absolute and an empty relative path" in {
    Root / "foo" / "bar" / Current should be (Root / "foo" / "bar")
  }
  
  it should "combine an absolute and a relative path pointing to a parent one level up" in {
    Root / "foo" / "bar" / (Parent(1) / "baz") should be (Root / "foo" / "baz")
  }
  
  it should "combine an absolute and a relative path pointing to a parent two levels up" in {
    Root / "foo" / "bar" / (Parent(2) / "baz") should be (Root / "baz")
  }
  
  it should "combine an absolute and a relative path pointing to a parent two levels up without a path element" in {
    Root / "foo" / "bar" / (Parent(2)) should be (Root)
  }
  
  it should "create a relative path pointing to the same directory" in {
    (Root / "a").relativeTo(Root / "a") should be (Current)
  }
  
  it should "create a relative path pointing to a parent directory" in {
    (Root / "a").relativeTo(Root / "a" / "b") should be (Parent(1))
  }
  
  it should "create a relative path pointing to a child directory" in {
    (Root / "a" / "b").relativeTo(Root / "a") should be (Current / "b")
  }
  
  it should "create a relative path pointing to a sibling directory" in {
    (Root / "a" / "b").relativeTo(Root / "a" / "c") should be (Parent(1) / "b")
  }
  
}
