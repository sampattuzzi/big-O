PolyML.Compiler.debug := true;
(* Test functions for different complexity classes. *)

fun addList a l =
    let
        fun f (b,c) = a+b+c
    in
        case l of
              [] => a
            | (x::y) =>
                let
                    val v = f x
                    val l' = y
                in
                    addList v l'
                end
    end;

fun fib 0 = 1
  | fib 1 = 1
  | fib n = (fib (n-1)) + (fib (n-2));

fun const_fun n = ();

fun noisy_const_fun n = 
  if ((n mod 2) = 0) 
  then 
    const_fun n 
  else 
    (
    if ((n mod 3) = 0) 
    then
      ( const_fun n 
      ; const_fun n
      ) 
    else ());

fun log_fun 0 = ()
  | log_fun n = log_fun (n div 2);

fun lin_fun 0 = ()
  | lin_fun n = lin_fun (n - 1);

fun lin_log_fun 0 = ()
  | lin_log_fun n = (log_fun n; lin_log_fun (n-1));

fun square_fun 0 = ()
  | square_fun n = (lin_fun n; square_fun (n-1));

fun cube_fun 0 = ()
  | cube_fun n = (square_fun n; cube_fun (n-1));

fun very_exp_fun 0 = ()
  | very_exp_fun n = (very_exp_fun (n-1); very_exp_fun (n-1); very_exp_fun (n-1));

fun less_exp_fun 0 = ()
  | less_exp_fun n = (less_exp_fun (n-1); less_exp_fun (n div 4));

fun rev xs =
let
  fun r [] ys = ys
    | r (x::xs) ys = r xs (x::ys)
in
  r xs []
end;
