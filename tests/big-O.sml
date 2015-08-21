use "tests/big-O-functions.sml";
use "tests/big-O-generators.sml";
use "big-O.sml";

(* Tests the various complexity classes. *)
(* Should all be true. *)
val timeout = 0.05;
val const_class_test = ((getClass timeout id const_fun) = "1");
val noisy_const_class_test = ((getClass timeout id noisy_const_fun) = "1");
val log_class_test = ((getClass timeout id log_fun) = "log(n)");
val lin_class_test = ((getClass timeout id lin_fun) = "n");
val lin_log_class_test = ((getClass timeout id lin_log_fun) = "n * log(n)");
val square_class_test = ((getClass timeout id square_fun) = "n ^ 2");
val cube_class_test = ((getClass timeout id cube_fun) = "n ^ 3");
val fib_class_test = ((getClass timeout id fib) = "2 ^ n");
val very_exp_class_test = ((getClass timeout id very_exp_fun) = "2 ^ n");
(* This case is hard to distinguish at 0.05 but tends with greater timeout.*)
val less_exp_class_test = ((getClass timeout id less_exp_fun) = "n ^ 3");

(* Tricky case as starts cubic then turns linear. Get the right result by
* discarding startup effects.*)
val startup_cube_test = ((getClass timeout id startup_cube) <> "n");
val startup_cube_dropping_test = ((getClassWithoutStartup 0.5 timeout id
  startup_cube) = "n");
