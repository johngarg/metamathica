(* peano arithmetic *)
(* pattern matching not implemented yet, so this won't work *)

succ[infinity] = infinity;

zeroQ[zero] = True;
zeroQ[succ[x___]] = False;

plus[x, zero_] := zero;
plus[x_, succ[y_]] := succ[plus[x, y]];

times[x, zero_] := zero;
times[x_, succ[y_]] := plus[x, times[x, y]];
