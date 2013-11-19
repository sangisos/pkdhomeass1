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
	fun checkCapitalization [] = false
	  | checkCapitalization [c] = Char.isUpper c
	  | checkCapitalization (c::r) = Char.isUpper c andalso
				       checkCapitalization(r)
	fun checkWords [] = false
	  | checkWords [word] = checkCapitalization( explode(word) )
	  | checkWords (word::r) = checkCapitalization(explode(word))
				   andalso checkWords r
    in
	checkWords(addends) andalso checkWords([sum])
    end;

(1,validatePuzzle(["A","B"],"C"));
(2,validatePuzzle(["A"],"A"));
(3,validatePuzzle([""],"A") = false);
(4,validatePuzzle([],"A") = false);
(5,validatePuzzle(["A"],"") = false);
(6,validatePuzzle(["A"],"a") = false);
(7,validatePuzzle(["a"],"A") = false);
(8,validatePuzzle(["A","b"],"C") = false);
(9,validatePuzzle(["AAA","BBB"],"CCC"));
(10,validatePuzzle(["AAA","BbB"],"CCC") = false);
(11,validatePuzzle(["AAA","BBB"],"CcC") = false);
