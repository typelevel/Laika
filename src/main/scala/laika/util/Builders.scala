/*
 * Copyright 2013 the original author or authors.
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

/** Based on a concept outlined by Sadek Drobi in this gist: https://gist.github.com/sadache/3646092
 *  The code used here is only a subset of the demonstrated functionality.
 * 
 *  @author Sadek Drobi / Jens Halm
 */
object Builders {

  
  trait CanBuild [M[_]]{
    
    def apply [A,B](ma: M[A], mb: M[B]): M[A ~ B]
        
    def map [A,B](m: M[A], f: A => B): M[B]
            
  }
    
    
  implicit def toBuilderOps [M[_],A](a: M[A])(implicit fcb: CanBuild[M]) = new BuilderOps[M,A](a)(fcb)
  
  class BuilderOps [M[_],A](ma:M[A])(implicit fcb: CanBuild[M]) {
   
    def ~ [B](mb: M[B]): Builder[M]#CanBuild2[A,B] = { 
      val b = new Builder(fcb)
      new b.CanBuild2(ma,mb)
    }
   
  }
  
  
  case class ~[A,B](_1:A,_2:B)
  
  
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
    }
    
    // TODO - continue
   
  }

  
  
}