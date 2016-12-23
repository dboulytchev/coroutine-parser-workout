ocamlc -rectypes -o P -pp "camlp5o -I `ocamlfind -query GT.syntax.all` pa_gt.cmo -L `ocamlfind -query GT.syntax.all`" -I `ocamlfind -query GT` GT.cma  P.ml
 
