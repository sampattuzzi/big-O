(* Generator functions *)

fun list_gen 0 = []
  | list_gen n = (n, n+2) :: (list_gen (n-1));


