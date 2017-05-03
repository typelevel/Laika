/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.util

/** Generic support for builders that allow to combine container types with
 *  matching type classes into a final result.
 * 
 *  The concrete use case for Laika is a concise and type-safe API for
 *  setting up directives or text roles. For these APIs and sample
 *  code see [[laika.parse.rst.Directives]] and [[laika.parse.rst.TextRoles]].
 * 
 *  This implementation is based on a concept outlined by Sadek Drobi in this gist: 
 *  [[https://gist.github.com/sadache/3646092]].
 *  The code used here is only a simplified subset of the demonstrated functionality.
 * 
 *  @author Sadek Drobi / Jens Halm
 */
object Builders {


  /** A wrapper for a single result value.
   */
  class Result[+A] (a: => A) {
    
    def get: A = a
    
    def map [B](f: A => B): Result[B] = new Result(f(get))
    
  }


  /** Contract for type classes that adapt a container
   *  type for use with these builders. Implementations
   *  have to know how to merge two containers as well
   *  as how to perform a simple map.
   */
  trait CanBuild [M[_]]{
    
    def apply [A,B](ma: M[A], mb: M[B]): M[A ~ B]
        
    def map [A,B](m: M[A], f: A => B): M[B]
            
  }
    
    
  /** Allows to use the `~` combinator function on all classes
   *  that have a matching `CanBuild` type class.
   */
  implicit class BuilderOps [M[_],A](ma:M[A])(implicit fcb: CanBuild[M]) {
   
    def ~ [B](mb: M[B]): Builder[M]#CanBuild2[A,B] = { 
      val b = new Builder(fcb)
      new b.CanBuild2(ma,mb)
    }
   
  }
  
  /** Builders for using combinators for up to 12 result values.
   */
  class Builder [M[_]](canBuild: CanBuild[M]) {
   
    class CanBuild2 [A1,A2](m1: M[A1], m2: M[A2]) {
   
      def ~ [A3](m3: M[A3]) = new CanBuild3(canBuild(m1, m2), m3)
   
      def apply [B](f: (A1,A2) => B): M[B] =
        canBuild.map[A1 ~ A2, B](canBuild(m1, m2), { case a1 ~ a2 => f(a1, a2)})
    }
   
    class CanBuild3 [A1,A2,A3](m1: M[A1 ~ A2], m2: M[A3]) {
   
      def ~ [A4](m3: M[A4]) = new CanBuild4(canBuild(m1, m2), m3)
   
      def apply [B](f: (A1,A2,A3) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3, B](canBuild(m1, m2), { case a1 ~ a2 ~ a3 => f(a1, a2, a3) })
    }
   
    class CanBuild4 [A1,A2,A3,A4](m1: M[A1 ~ A2 ~ A3], m2: M[A4]) {
   
      def ~ [A5](m3: M[A5]) = new CanBuild5(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4, B](canBuild(m1, m2), { case a1 ~ a2 ~ a3 ~ a4 => f(a1, a2, a3, a4) })
    }
   
    class CanBuild5 [A1,A2,A3,A4,A5](m1: M[A1 ~ A2 ~ A3 ~ A4], m2: M[A5]) {
      
      def ~ [A6](m3: M[A6]) = new CanBuild6(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 => f(a1, a2, a3, a4, a5) 
      })
    }
    
    class CanBuild6 [A1,A2,A3,A4,A5,A6](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5], m2: M[A6]) {
      
      def ~ [A7](m3: M[A7]) = new CanBuild7(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 => f(a1, a2, a3, a4, a5, a6) 
      })
    }
    
    class CanBuild7 [A1,A2,A3,A4,A5,A6,A7](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6], m2: M[A7]) {
      
      def ~ [A8](m3: M[A8]) = new CanBuild8(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 => f(a1, a2, a3, a4, a5, a6, a7) 
      })
    }
    
    class CanBuild8 [A1,A2,A3,A4,A5,A6,A7,A8](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7], m2: M[A8]) {
      
      def ~ [A9](m3: M[A9]) = new CanBuild9(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7,A8) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 => f(a1, a2, a3, a4, a5, a6, a7, a8) 
      })
    }
    
    class CanBuild9 [A1,A2,A3,A4,A5,A6,A7,A8,A9](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8], m2: M[A9]) {
      
      def ~ [A10](m3: M[A10]) = new CanBuild10(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7,A8,A9) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9) 
      })
    }
    
    class CanBuild10 [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9], m2: M[A10]) {
      
      def ~ [A11](m3: M[A11]) = new CanBuild11(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) 
      })
    }
    
    class CanBuild11 [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10], m2: M[A11]) {
      
      def ~ [A12](m3: M[A12]) = new CanBuild12(canBuild(m1,m2), m3)
   
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) 
      })
    }
    
    class CanBuild12 [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](m1: M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11], m2: M[A12]) {
      
      def apply [B](f: (A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12) => B): M[B] =
        canBuild.map[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12, B](canBuild(m1, m2), { 
          case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) 
      })
    }
    
    
   
  }

  
  
}

/** A wrapper for two result values.
  */
case class ~[+A,+B] (_1:A, _2:B)
