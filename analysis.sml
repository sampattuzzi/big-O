(* Maths analysis functions *)
fun sum xs = foldl (op+) 0.0 xs;

fun mean xs = sum xs / real (length xs);

fun stdDev xs =
let 
  val l = real (length xs) - 1.0
  val m = mean xs
  val summedElements = sum (map (fn x => Math.pow (x - m, 2.0)) xs) 
in
  Math.sqrt (summedElements / l)
end;

fun linReg xs ys =
let
  val m1 = mean xs
  val m2 = mean ys
  val n = real (length xs)
  val sx = stdDev xs
  val sy = stdDev ys
  val c = sum (zipWith (op*) (map (fn x => x - m1) xs) (map (fn y => y - m2)
  ys)) / (n - 1.0)
  val r = (c / sx * sy)
  val beta = (r * sy / sx)
  val alpha = m2 - beta * m1
in
  (alpha, beta, r*r)
end;

use "matrix/MATRIX.sml";
use "matrix/RealMatrix.sml";
fun linRegMat X y =
let
  open RealMatrix;
  val (n, _) = size(y)
  val beta = inv(trans(X) * X) * trans(X) * y
  val R = y - X * beta
  val RR = norm(trans(R) * R)
in
 (beta, RR)
end;
