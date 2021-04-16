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

package laika.io.helper

import java.util.regex.Pattern

/**
  * @author Jens Halm
  */
trait StringOps {

  implicit class TestStringOps (val str: String) {
    
    def extract (start: String, end: String): Option[String] = {
      val rest = str.split(Pattern.quote(start)).drop(1)
      if (rest.isEmpty) None
      else rest.mkString(start).split(Pattern.quote(end)).headOption
    }

    def extractTag (tag: String): Option[String] = extract(s"<$tag>", s"</$tag>")
    
    def removeBlankLines: String = str.split('\n').flatMap { line =>
      if (line.trim.nonEmpty) Some(line)
      else None
    }.mkString("\n")

    def removeIndentation: String = str.split('\n').map(_.trim).mkString("\n")
    
  }
  
}
