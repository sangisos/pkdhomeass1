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
POST: true if all the letters in l are capital letter, else false.
EXAMPLE: checkCapitalization ["R"]
VARIANT:

checkWords (l)
TYPE: 'a list -> bool
PRE:
POST: true if all letters in l are capitalized, else false.
EXAMPLE: checkWords ["SEND", "MORE"] = true
VARIANT: length l



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

fun checkMapping ("", solution) = true
  | checkMapping (addendsSumString, solution) =
    let
	fun existsIn (letter, []) = false
	  | existsIn (letter, (l,_)::rest) =
	     letter=l orelse
	     existsIn (letter, rest)
    in
	existsIn(hd(explode(addendsSumString)),solution) andalso
	checkMapping(String.substring(addendsSumString,1,size(addendsSumString)-1), solution)
    end;

(1, checkMapping("SENDMOREMONEY",[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(2, checkMapping("SENDMOREMONEY",[(#"D",7),(#"L",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);

fun check ((addends, sum), solution) = 
    validatePuzzle ((addends, sum))
    andalso validateSolution (solution)
    andalso
    let
	fun getValue (letter, []) = 0
	  | getValue (letter, (l,digit)::rest) =
	    if l=letter then digit else getValue(letter,rest)
					
	fun getValueOfWord "" = 0
	  | getValueOfWord word = 
	    getValueOfWord(
	    String.substring( word, 0, size(word)-1 )
	    ) * 10 + getValue(
	    String.sub( word, size(word)-1 ), solution
	    )
	fun sumList [] = 0
	  | sumList (x::xs) = x + sumList(xs)
			     
	val addendsSumList = sum::addends
	fun makeString [] = ""
	  | makeString (word::rest) = word ^ makeString(rest)
	
	fun checkIfFirstIsNotZero ([]) = true
	  | checkIfFirstIsNotZero ((letter,0)::solution) = 
	    let
		fun ok word = String.sub(word,0) <> letter
		fun allOk [] = false
		  | allOk (word::[]) = ok(word)
		  | allOk (word::rest) = ok(word) andalso allOk(rest)
	    in
		allOk(addendsSumList)
	    end
	  | checkIfFirstIsNotZero (_::rest) =
	    checkIfFirstIsNotZero (rest);
				      
      in
	  checkIfFirstIsNotZero(solution)
	  andalso
	  checkMapping ( makeString(addendsSumList), solution)
	  andalso
	  foldl op+ 0 (map getValueOfWord addends) = getValueOfWord(sum)
      end;
    
(1, check((["SEND","MORE"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(2, check((["SEND","MORE"],"MONEY"),[(#"D",3),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
(3, check((["SEND","MORe"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
