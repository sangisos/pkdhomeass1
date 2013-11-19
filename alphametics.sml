(* validatePuzzle((x,y))
TYPE: string list * string -> bool
PRE: x must not be [], . x and y must not be "". All characters in x and y
	must be alphabetic capitals, and there can be no more than 10 different
	letters in total.
POST: true if x and y produces a valid arithmetic puzzle, otherwise false.
EXAMPLE: (["SEND, "MORE"], "MONEY") = true


*)
fun validatePuzzle((addends,sum)) = 
    let 
	fun checkWords [] = true
	  | checkWords (word::r) = if word = "" then 
				       false 
				   else 
				       checkWords r
    in
	if checkWords(addends) orelse checkWords([sum]) then
	    true (*TODO: Will be implemented*)
	else
	    false
    end;
