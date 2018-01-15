<<"../quotation.wl";

(* Modification of ../interpreter.wl to support lazy evaluation *)

(* -------------------------------------- *)
(* evalExpr is used to evaluate meta code *)

(* TODO debug assignment! *)

(* string and numbers *)
evalExpr[expr_?selfEvaluatingQ, env_] := expr;
(* symbols *)
evalExpr[symb: quoted[x_], env_] := If[MemberQ[Keys[env], symb],
                                       lookup[symb, env],
                                       x];
(* lists *)
evalExpr[list: List[xs__], env_] := thunk[#, env]& /@ list;
(* variable assignment *)
evalExpr[quoted[Set][x_, y_], env_] :=
  Block[{val = evalExpr[y, env]},
        assignVariable[x, val, env];
        val];
(* if *)
evalExpr[quoted[If][p_, c_, a_], env_] :=
  If[trueQ[actualValue[p, env]],
     evalExpr[c, env],
     evalExpr[a, env]];
(* compound expression *)
evalExpr[quoted[CompoundExpression][exprs__], env_] :=
  Block[{commands = List[exprs]},
        evalExpr[#, Append[$theGlobalEnvironment, env] ]& /@ Most[commands];
        evalExpr[Last@commands, Append[$theGlobalEnvironment, env]]];
(* anonymous functions *)
evalExpr[quoted[Function][arg_, body_], env_] :=
  Closure[arg, body, env];
(* function application *)
evalExpr[quotedFunc_[args___], env_] :=
  Block[{f = actualValue[quotedFunc, env], xs = List[args]},
        Which[
          (* primitive function *)
          primitiveWolframFunctionQ[f],
          f @@ (actualValue[#, env]& /@ xs),
          (* closure *)
          Head[f] === Closure,
          evalExpr[
            f[[2]],
            extendEnvironment[
              assignmentBindings[f[[1]], (delay[#, env]& /@ xs)],
              Append[env, f[[3]]]]],
          (* else return failed *)
          True, Failed]];
(* if nothing else, throw an error *)
evalExpr[expr_, env_] := Failed;


(* additional functions *)
(* increment *)
evalExpr[quoted[Increment][x_], env_] :=
  Block[{val = evalExpr[quote[x=x+1], env]},
        assignVariable[x, val, env];
        val];
(* list operations *)
evalExpr[quoted[Part][xs_, ref_], env_] :=
  evalExpr[xs, env][[evalExpr[ref, env]]];
evalExpr[quoted[Length][xs_], env_] := Length[xs];
(* force *)
evalExpr[quoted[force][xs_], env_] := force[evalExpr[xs, env]];


(* lazy helpers *)

delay[expr_, env_] := thunk[expr, env];

actualValue[expr_, env_] := force[evalExpr[expr, env]];

force[thunk[expr_, env_]] := actualValue[expr, env];
force[list: List[xs__]] := force /@ list;
force[x_] := x;


(* helper functions *)

assignmentBindings[vars_, vals_] :=
  If[Length[vars] != Length[vals],
     Print["Error -- assignmentBindings!"]; Failed,
     Join @@ {#[[1]] -> #[[2]]}& /@ Partition[Riffle[vars, vals], 2]];

(* just borrow the underlying bools *)
trueQ[True] := True;
trueQ[False] := False;
trueQ[x_] := x;

primitiveWolframFunctions = {
  Times,
  Plus,
  Power,
  Equal
  (* add whatever you like... *)
};

selfEvaluatingQ[expr_] := Or[StringQ[expr], NumberQ[expr]];

primitiveWolframFunctionQ[qf_] := MemberQ[primitiveWolframFunctions, First[qf]];

(* lookup a symbol in the environment map *)
lookup[expr_, env_] := env[expr];

(* variable assignment extends the environment *)
primitiveWolframFunctionQ[f_] := MemberQ[primitiveWolframFunctions, f];
selfEvaluatingQ[expr_] := Or[StringQ[expr], NumberQ[expr]];
assignmentQ[x_] := MatchQ[x, quoted[Set][_,_]];
(* assign var to val through a side effect *)
assignVariable[var_, val_, env_] :=
  ($theGlobalEnvironment = Append[env, var -> val]);
extendEnvironment[bindings_, env_] := Append[env, bindings];

(* lookup a symbol in the environment map *)
lookup[expr_, env_] := env[expr];

