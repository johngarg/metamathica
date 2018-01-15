(* Church pairs *)
cons = Function[
  {x, y},
  Function[{m}, m[x, y]]];

car = Function[
  {z},
  z[Function[{p, q}, p]]];

cdr = Function[
  {z},
  z[Function[{p, q}, q]]];
