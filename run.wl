<<"/Users/johngargalionis/Dropbox/mathematicaPackages/metamathica/quotation.wl";
(* <<"/Users/johngargalionis/Dropbox/mathematicaPackages/metamathica/interpreter.wl"; *)
<<"/Users/johngargalionis/Dropbox/mathematicaPackages/metamathica/examples/lazy.wl";
(* <<"/Users/johngargalionis/Dropbox/mathematicaPackages/metamathica/core.wl"; *)


(* for use with the regular interpreter, in that case no thunks to be forced *)
force[x_] := x


(* the interactive Read-Eval-Print-Loop [REPL] *)

(* start with an [almost] empty global environment *)
initializeEnv[] :=
  Association[
    (* you can initialize bindings here... *)
  ];
$theGlobalEnvironment = initializeEnv[];

repl[] :=
  Do[
    input = "quote["<>InputString["Meta> "]<>"]";
    (* system commands to exit, clear or show environment *)
    If[input == "quote[exit[]]", Exit[]];
    If[input == "quote[$theGlobalEnvironment]",
       Print[$theGlobalEnvironment];
       Continue[]];
    If[input == "quote[clear[]]",
       $theGlobalEnvironment = initializeEnv[];
       Print["Environment cleared!"];
       Continue[]];

    (* evaluate string containing quoted expression *)
    quotedInput = ToExpression[input]//Quiet;
    (* Print[quotedInput]; *)

    If[Head[quotedInput] === quoted[Get],
       Block[{fileName = quotedInput[[1]]},
             quotedInput = ToExpression["quote["<>ReadString[fileName]<>"]"]//Quiet;
             Print[fileName<>" loaded!"]]];

    output = evalExpr[quotedInput, $theGlobalEnvironment]//Quiet;

    (* (\* don't display thunks to the screen *\) *)
    (* If[Head[output] === thunk, Print["thunk!"]; Continue[]]; *)

    (* Don't print env in REPL when evaluating functions *)
    (* If[Head[output] === Closure, *)
    (*    Print["<Function>"], *)
    (*    Print[output]]; *)
    Print[output//force];, 1000] (* for now, run at most 1000 times *)

repl[]

