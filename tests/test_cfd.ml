open ConjugateGradient
open CFDHilbert

module CF = CFDHilbert
module Q = Params(CF)
module R = ConjugateGradient (CF)



let init = ones ()
let b = ones ()


(*let c = 
  let cc = float_of_int 64 in
   ( (cc *. cc *. cc))
*)



let p  = {Q.init=init;maxit=100;reltol=1e-6;check_gap=(20);}
let soln = R.solve p op b
