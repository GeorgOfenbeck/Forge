// package ch.epfl
// package lamp
// package autolifter
// package library

// /* due to ambigous implicit values */
// trait FractionalImplicits {
//   class FractionalOps[T: Fractional](lhs: T){
//     def /(rhs: T) = implicitly[Fractional[T]].div(lhs, rhs)
//   }

//   implicit def infixFractionalOps[T: Fractional](x: T): FractionalOps[T] = new FractionalOps(x)
// }
