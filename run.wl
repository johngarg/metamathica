(* ::Package:: *)

<<"quotation.wl";


(* Use only one interpreter at a time! *)
interpreterChoice = "lazy"; (* Choose one of "core", "regular", "lazy" *)

Which[
    (* As bare as possible *)
	interpreterChoice=="core",
	<<"core.wl";
	processOutput = Identity;
	Print["Running the core interpreter."],
	
	(* More features *)
	interpreterChoice=="regular",
	<<"interpreter.wl";
	processOutput = Identity; (* Convenient to leave the `force` command below *)
	Print["Running the regular interpreter."],
	
	(* The lazy interpeter example *)
	interpreterChoice=="lazy",
	<<"examples/lazy.wl";
	processOutput = force;
	Print["Running the lazy interpreter."]];


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
    Print[quotedInput];

    If[Head[quotedInput] === quoted[Get],
       Block[{fileName = quotedInput[[1]]},
             quotedInput = ToExpression["quote["<>ReadString[fileName]<>"]"]//Quiet;]];

    output = evalExpr[quotedInput, $theGlobalEnvironment]//Quiet;
    Print[processOutput[output]];, 1000] (* for now, run at most 1000 times *)

repl[]
