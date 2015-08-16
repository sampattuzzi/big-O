use "tests/big-O-functions.sml";
use "tests/big-O-generators.sml";
use "big-O.sml";

(* Tests the various complexity classes. *)
val timeout = 0.05;
val const_class = getClass timeout id const_fun;
val noisy_const_class = getClass timeout id noisy_const_fun;
val log_class = getClass timeout id log_fun;
val lin_class = getClass timeout id lin_fun;
val lin_log_class = getClass timeout id lin_log_fun;
val square_class = getClass timeout id square_fun;
val cube_class = getClass timeout id cube_fun;
val fib_class = getClass timeout id fib;
val very_exp_class = getClass timeout id very_exp_fun;
(* This case is hard to distinguish at 0.05 but tends with greater timeout.*)
val less_exp_class = getClass timeout id less_exp_fun; 
