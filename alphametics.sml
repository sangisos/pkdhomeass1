(* validatePuzzle((x,y))
PRE: x must not be []. x and y must not be "". 
POST:



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
    
