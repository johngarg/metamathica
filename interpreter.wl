(* ::Package:: *)

<<"quotation.wl";

(* -------------------------------------- *)
(* evalExpr is used to evaluate meta code *)

(* string and numbers *)
evalExpr[expr_?selfEvaluatingQ, env_] := expr;
(* symbols *)
evalExpr[symb: quoted[x_], env_] := If[MemberQ[Keys[env], symb],
                                       lookup[symb, env],
                                       x];
(* lists *)
evalExpr[list: List[xs__], env_] := evalExpr[#, env]& /@ list;
(* variable assignment *)
evalExpr[quoted[Set][x_, y_], env_] :=
  Block[{val = evalExpr[y, env]},
        assignVariable[x, val, env];
        val];
(* if *)
evalExpr[quoted[If][p_, c_, a_], env_] :=
  If[trueQ[evalExpr[p, env]],
     evalExpr[c, env],
     evalExpr[a, env]];
(* compound expression *)
evalExpr[quoted[CompoundExpression][exprs__], env_] :=
  Block[{commands = List[exprs]},
        evalExpr[#, Append[$theGlobalEnvironment, env] ]& /@ Most[commands];
        evalExpr[Last@commands, Append[$theGlobalEnvironment, env]]];
(* anonymous functions *)
evalExpr[quoted[Function][args_, body_], env_] :=
  Closure[args, body, env];
(* function application *)
evalExpr[quotedFunc_[args___], env_] :=
  Block[{f = evalExpr[quotedFunc, env], xs = evalExpr[#, env]& /@ List[args]},
        Which[
          (* primitive function *)
          primitiveWolframFunctionQ[f],
          f @@ xs,
          (* closure *)
          Head[f] === Closure,
          evalExpr[f[[2]], extendEnvironment[
            assignmentBindings[f[[1]], xs],
            (* shadow args in closure *)
            Append[env, f[[3]]]]],
          (* else return failed *)
          True, Failed]];
(* if nothing else, throw an error *)
evalExpr[expr_, env_] := Failed;


(* helper functions *)

(* assignmentBindings[vars_, vals_] := *)
(*   Block[{}, *)
(*         If[Length[vars] != Length[vals], Print["Error -- assignmentBindings!"]; Failed]; *)
(*         Join @@ {#[[1]] -> #[[2]]}& /@ Partition[Riffle[vars, vals], 2]]; *)

assignmentBindings[vars_, vals_] :=
  If[Length[vars] != Length[vals],
     Print["Error -- assignmentBindings!"]; Failed,
     Join @@ {#[[1]] -> #[[2]]}& /@ Partition[Riffle[vars, vals], 2]];

trueQ[x_] := x === True;

primitiveWolframFunctions = {
  Plus,
  Times,
  Power,
  Equal,
  Solve,
  ReplaceAll,
  Part,
  First,
  Rest
  (* add whatever you like... *)
};

primitiveWolframFunctionQ[f_] := MemberQ[primitiveWolframFunctions, f];
selfEvaluatingQ[expr_] := Or[StringQ[expr], NumberQ[expr]];
assignmentQ[x_] := MatchQ[x, quoted[Set][_,_]];
(* assign var to val through a side effect *)
assignVariable[var_, val_, env_] :=
  ($theGlobalEnvironment = Append[env, var -> val]);
extendEnvironment[bindings_, env_] := Append[env, bindings];

(* lookup a symbol in the environment map *)
lookup[expr_, env_] := env[expr];
