(* validatePuzzle((x,y))
TYPE: string list * string -> bool
PRE: x must not be [], . x and y must not be "". All characters in x and y
	must be alphabetic capitals, and there can be no more than 10 different
	letters in total.
POST: true if x and y produces a valid arithmetic puzzle, otherwise false.
EXAMPLE: (["SEND, "MORE"], "MONEY") = true



*)

fun checkCapitalization ( [] ) = false (* should never end up here *)
	  | checkCapitalization ( [c] ) = if Char.isLower c then
					      false
					  else
					      true
	  | checkCapitalization ( c::r ) = if Char.isLower c then
					       false
					   else
					       checkCapitalization(r);
fun checkWords [] = false
	  | checkWords [word] =  if word = "" then 
				       false
				   else
				       checkCapitalization(explode(word))
	  | checkWords (word::r) = if word = "" then 
				       false
				   else
				       checkCapitalization(explode(word)) andalso checkWords r;

fun validatePuzzle((addends,sum)) = 
    checkWords(addends) andalso checkWords([sum]);
    
