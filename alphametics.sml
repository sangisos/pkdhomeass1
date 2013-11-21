(* 
validatePuzzle((x,y))
TYPE: string list * string -> bool
PRE: x must not be []. x and y must not be "". All characters in x and y
	must be alphabetic capitals, and there can be no more than 10 different
	letters in total.
POST: true if x and y produces a valid arithmetic puzzle, otherwise false.
EXAMPLE: (["SEND, "MORE"], "MONEY") = true

checkCapitalization (l)
TYPE: 'a list -> bool
PRE: 
POST:
EXAMPLE:
VARIANT:

checkWords (l)
TYPE: 
PRE:
POST:
EXAMPLE:
VARIANT:



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

(1,validatePuzzle(["A","B"],"C") = true);
(2,validatePuzzle(["A"],"A") = true);
(3,validatePuzzle([""],"A") = false);
(4,validatePuzzle([],"A") = false);
(5,validatePuzzle(["A"],"") = false);
(6,validatePuzzle(["A"],"a") = false);
(7,validatePuzzle(["a"],"A") = false);
(8,validatePuzzle(["A","b"],"C") = false);
(9,validatePuzzle(["AAA","BBB"],"CCC") = true);
(10,validatePuzzle(["AAA","BbB"],"CCC") = false);
(11,validatePuzzle(["AAA","BBB"],"CcC") = false);

(* validateSolution(solution)
TYPE: (char * int) list -> bool
PRE: 
POST:
EXAMPLE:
VARIANT: length solution
*)

fun validateSolution [] = false
  | validateSolution ((letter,digit)::solution) =
    let
	fun neitherIn ((letter,digit), []) = true
	  | neitherIn ((letter,digit), (l,d)::rest) =
	     letter<>l andalso
	     digit<>d andalso
	     neitherIn ((letter,digit), rest)
    in
	Char.isUpper letter andalso
	0 <= digit andalso digit <= 9 andalso
	neitherIn((letter,digit),solution) andalso
	if solution = [] then
	    true
	else
	    validateSolution(solution)
    end;

(1, validateSolution([]) = false);
(2,validateSolution([(#"A",1)]) = true);
(3,validateSolution([(#"a",1)]) = false);
(4,validateSolution([(#"A",10)]) = false);
(5,validateSolution([(#"A",1),(#"A",1)]) = false);
(6,validateSolution([(#"A",1),(#"B",1)]) = false);
(7,validateSolution([(#"A",1),(#"B",10)]) = false);
(8,validateSolution([(#"a",1),(#"B",2)]) = false);
(9,validateSolution([(#"A",1),(#"b",2)]) = false);
(10,validateSolution([(#"A",1),(#"A",2)]) = false);
(11,validateSolution([(#"A",1),(#"B",2)]) = true);
(12,validateSolution([(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(13, validateSolution([(#"4",8)]) = false);
(14, validateSolution([(#"#",8)]) = false);
(15, validateSolution([(#"'",8)]) = false);



(* check ((x,y), l) 
   TYPE:
   PRE: 
   POST:
   EXAMPLE:
   *)

fun checkIfFirstIsNotZero ((addends,sum), []) = true
  | checkIfFirstIsNotZero ((addends,sum), (letter,0)::solution) = 
    let
	fun ok word = hd(explode(word)) <> letter
	fun allOk [] = false
	  | allOk (word::[]) = ok(word)
	  | allOk (word::rest) = ok(word) andalso allOk(rest)
    in
	allOk(sum::addends)
    end
  | checkIfFirstIsNotZero ((addends,sum), (letter,digit)::solution) =
    checkIfFirstIsNotZero ((addends,sum), solution);

(*fun check ((addends, sum),solution) = if validatePuzzle ((addends, sum)) andalso validateSolution (solution) then
		let 


	 	fun checkMapping solution = 

	 	fun checkSum ((addends, sum), solution) =
		in
		end;


	else false*)











