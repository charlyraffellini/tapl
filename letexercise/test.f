/* Examples for testing */

 lambda x:Bool. x;
 (lambda x:Bool->Bool. if x false then true else false) 
   (lambda x:Bool. if x then false else true); 

let x = false in true;

let x = false in (lambda y:Bool. y) x;

let x = ((lambda y:Bool. y) false) in (lambda y:Bool. if y then false else true) x;
