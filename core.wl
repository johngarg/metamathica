<<"/Users/johngargalionis/Dropbox/mathematicaPackages/metamathica/quotation.wl";

(* -------------------------------------- *)
(* evalExpr is used to evaluate meta code *)

(* symbols *)
evalExpr[symb: quoted[x_], env_] := If[MemberQ[Keys[env], symb], env[symb], x];
(* variable assignment *)
evalExpr[quoted[Set][x_, y_], env_] :=
  Block[{val = evalExpr[y, env]},
        assignVariable[x, val, env];
        val];
(* compound expression *)
evalExpr[quoted[CompoundExpression][exprs__], env_] :=
  Block[{commands = List[exprs]},
        evalExpr[#, Append[$theGlobalEnvironment, env] ]& /@ Most[commands];
        evalExpr[Last@commands, Append[$theGlobalEnvironment, env]]];
(* anonymous functions *)
evalExpr[quoted[Function][args_, body_], env_] := Closure[args, body, env];
(* function application *)
evalExpr[quotedFunc_[args___], env_] :=
  Block[{f = evalExpr[quotedFunc, env],
         xs = evalExpr[#, env]& /@ List[args]},
        evalExpr[f[[2]],
                 extendEnvironment[
                   assignmentBindings[f[[1]], xs],
                   (* shadow args in closure *)
                   Append[env, f[[3]]]]]];
(* if nothing else, throw an error *)
evalExpr[expr_, env_] := Failed;


(* helper functions *)

assignmentBindings[vars_, vals_] :=
  If[Length[vars] != Length[vals],
     Print["Error -- assignmentBindings!"]; Failed,
     Join @@ {#[[1]] -> #[[2]]}& /@ Partition[Riffle[vars, vals], 2]];

assignmentQ[x_] := MatchQ[x, quoted[Set][_,_]];
(* assign var to val through a side effect *)
assignVariable[var_, val_, env_] :=
  ($theGlobalEnvironment = Append[env, var -> val]);
extendEnvironment[bindings_, env_] := Append[env, bindings];
