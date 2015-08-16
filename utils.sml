(* Utility functions *)
fun id x = x;

val reals = map real;

fun compose f g x = f (g x);

fun zip (x::xs) (y::ys) = ((x,y))::(zip xs ys)
  | zip [] [] = [];

fun zipWith f (x::xs) (y::ys) = (f (x,y))::(zipWith f xs ys)
  | zipWith _ [] [] = [];

fun max compare min xs = foldl (fn (x, y) => if compare x y then x else y) min xs
