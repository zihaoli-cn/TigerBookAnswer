type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* page-11 exercise 1 : starts here *)

fun maxargs(x) = 
  let 
    fun maxExp(x) = 
        case x
            of IdExp _ => 0
            | NumExp num => 0
            | OpExp (left, _, right) => let val left_val = maxExp(left) 
                                            val right_val = maxExp(right)
                                        in 
                                            Int.max(left_val, right_val) 
                                        end
            | EseqExp (s, e) => let val stm_result = maxargs(s)
                                    val exp_result = maxExp(e)
                                in 
                                    Int.max(stm_result, exp_result) 
                                end

    fun maxExpList(x) = 
        case x 
        of [] => 0
        | hd :: hs => let val exp_result = maxExp(hd)
                          val tail_result = maxExpList(hs)
                      in 
                        Int.max(exp_result, tail_result) 
                      end
  in 
    case x 
        of CompoundStm (left, right) => let val left_result = maxargs(left)
                                            val right_result = maxargs(right)
                                        in 
                                            Int.max(left_result, right_result) 
                                        end
         | AssignStm (_, e) => maxExp(e)
         | PrintStm l => List.length(l)
  end

(* page-11 exercise 1 : ends here *)

(* page-11 exercise 2 : starts here *)
exception NotFound;

type symbol_table = (string * int) list

fun lookup (table : symbol_table, name : string) = 
    case table
    of [] => raise NotFound
    | (k, v)::remained => if k = name 
                          then v
                          else lookup(remained, name);

fun update (table : symbol_table, name : string, data : int) = 
    (name, data)::table;

fun getBinaryOp Plus = op +
  | getBinaryOp Minus = op -
  | getBinaryOp Times = op *
  | getBinaryOp Div = op div;

fun interpStm(stmt : stm, env : symbol_table) = 
    case stmt 
    of CompoundStm (left, right) => interpStm(right, interpStm(left, env))
     | AssignStm (name, expr) => let val (new_value, new_env) = interpExp(expr, env) 
                                 in
                                    update(new_env, name, new_value)
                                 end
     | PrintStm exps => let fun expPrint(expr, env) = 
                            let val (value, new_env) = interpExp(expr, env)
                            in
                                (print (Int.toString value);
                                print " ";
                                new_env)
                            end
                      in
                          List.foldl expPrint env exps
                      end
and interpExp(expr : exp, env : symbol_table) = 
  case expr
  of IdExp identifier => (lookup(env, identifier), env)
   | NumExp x => (x, env)
   | OpExp (left, operation, right) => let val (left_val, left_env) = interpExp(left, env)
                                           val (right_val, right_env) = interpExp(right, left_env)
                                    in
                                        (((getBinaryOp operation) (left_val, right_val)), right_env)
                                    end
   | EseqExp (stmt, expr_) => interpExp(expr_, interpStm(stmt, env));


fun interp(stmt: stm) = interpStm(stmt, [])       