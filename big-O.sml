use "utils.sml";
use "analysis.sml";

PolyML.Compiler.debug := false;
use "count.sml";

(* Complexity class finding functions *)

type complexityClass = { 
 name:string, 
 class_fun: RealMatrix.matrix -> RealMatrix.matrix, 
 class_inv: RealMatrix.matrix -> RealMatrix.matrix
 };

fun testComplexityClass timeout (gen: int -> 'b) (f: 'b -> 'd) (compClass:
  complexityClass) = 
  let
    open Time
    val start_time = now ()
    val time_allowed = fromReal timeout
    val class = #class_fun compClass
    val class_inv = #class_inv compClass

    fun run_to_largest d =
      let
        val time_now = now ()
        val delta = time_now - start_time
        val default = ([], [])
      in
        if delta < time_allowed
        then
          let 
            val timeout = time_allowed - delta
            val count = getFunctionCount f timeout (gen d)
          in
            if isSome count
            then
              let 
                val (ys, xs) = run_to_largest (Int.+(d, 1)) 
              in
                ((getOpt (count, 0))::ys, d::xs)
              end
            else
              default
          end
        else
          default
      end

    val (ys, xs) = run_to_largest 1
    val x = RealMatrix.trans(RealMatrix.fromList [(reals xs)])
    val y = RealMatrix.trans(RealMatrix.fromList [(reals ys)])
    val X = class x
    val y2 = class_inv y
    val (beta, r) = linRegMat X y2
    val (rank, _) = RealMatrix.size(y2)
  in
    r / (real rank)
  end;

fun ones n = RealMatrix.fromDiag(RealMatrix.identity(n))

val known_classes: complexityClass list = [
 {name = "1" 
 , class_fun = (fn x => 
   let
     open RealMatrix
     val (n, _) = size(x)
   in
     (ones n)
   end) 
 , class_inv = id
 },
 {name = "log(n)"
 , class_fun = (fn x => let
     open RealMatrix
     val (n, _) = size(x)
     val lg = map Math.ln x
   in
     appendCols(lg, ones n)
   end)
 , class_inv = id},
 {name = "n"
 , class_fun = (fn x =>
   let
     open RealMatrix
     val (n, _) = size(x)
   in
     appendCols(x, ones n)
   end)
 , class_inv = id},
 {name = "n * log(n)"
 , class_fun = (fn x =>
   let
     val (n, _) = RealMatrix.size(x)
     val lg = RealMatrix.map (fn x => x * Math.ln(x)) x
   in
     RealMatrix.appendCols(lg, ones n)
   end)
 , class_inv = id},
 {name = "n ^ 2"
 , class_fun = (fn x =>
   let
     open RealMatrix
     val (n, _) = size(x)
     val lg = map (fn x => Math.pow(x, 2.0)) x
   in
     appendCols(lg, ones n)
   end)
 , class_inv = id
 },
 {name = "n ^ 3"
 , class_fun = (fn x =>
   let
     open RealMatrix
     val (n, _) = size(x)
     val lg = map (fn x => Math.pow(x, 3.0)) x
   in
     appendCols(lg, ones n)
   end)
 , class_inv = id
 },
 {name = "2 ^ n"
 , class_fun = (fn x =>
   let
     open RealMatrix
     val (n, _) = size(x)
     fun f x = x
     val exp = map f x
   in
     appendCols(exp, ones n)
   end)
 , class_inv = (fn x =>
   let
     val (n, _) = RealMatrix.size(x)
     (* Control the strength of the exponential function so as not to suppress
     * errors too much nor use numbers too large. *)
     val magic = (0.0001 * 150.0) / (real n)
     fun f x = Math.ln ( x ) / Math.ln ( 1.0 + magic )
     open RealMatrix
     val exp = map f x
   in
     appendCols(exp, ones n)
   end)
 }
 ]

fun getClass timeout gen f =
  let
    val leastSqrResult = map (testComplexityClass timeout gen f) known_classes
    val class_names = map (#name) known_classes
    val names_results = zip class_names leastSqrResult
    fun compare x y =
      let
        val (x_name, x_val) = x
        val (y_name, y_val) = y
      in
        (* Prefer simpler classes when they are "almost the same". *)
        not (Real.isNan x_val) andalso (x_val < (y_val - Math.pow(10.0, ~1.0))) 
      end
    val (max_name,_) = max compare ("none", Real.posInf) names_results
  in
    max_name
  end
