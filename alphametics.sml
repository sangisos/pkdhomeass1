
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
