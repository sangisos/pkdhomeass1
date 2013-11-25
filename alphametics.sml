(*	validatePuzzle((x,y))
	TYPE: string list * string -> bool
	PRE: true
	POST: true if x and y produces a valid arithmetic puzzle, otherwise false.
	EXAMPLE: (["SEND, "MORE"], "MONEY") = true
	VARIANT: length x, length y

	checkCapitalization (l)
	TYPE: 'a list -> bool
	PRE: true
	POST: true if all the letters in l are capital letters, else false.
	EXAMPLE: checkCapitalization [#"S",#"E",#"N",#"D"] = true
	VARIANT: length l

	checkWords (l)
	TYPE: 'a list -> bool
	PRE: true
	POST: true if checkCapitalization returns true, else false.
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
	fun listLettersInWords [] = []
	  | listLettersInWords (word::rest) = explode(word)@listLettersInWords(rest)
	fun uniqueLetters [] = []
	  | uniqueLetters (letter::rest) = 
	    let
		val result = uniqueLetters(rest)
	    in
		if (List.exists (fn x => x = letter) result) then
		    result
		else
		    letter::result
	    end
    in
	checkWords(addends) andalso checkWords([sum]) andalso
	length(uniqueLetters(listLettersInWords(sum::addends))) <= 10
    end;

(*
(1,uniqueLetters([#"A",#"B",#"A"]) = [#"B",#"A"]);
*)

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
(12,validatePuzzle(["ABCDE","FGHIJ"],"ABCDE") = true);
(13,validatePuzzle(["ABCDE","FGHIJK"],"ABCDED") = false);
(14,validatePuzzle(["ABCDE","FGHIJ"],"KLMNO") = false);

(*	validateSolution(l)
	TYPE: (char * int) list -> bool
	PRE: true
	POST: true if l is a list of unique tuples consisting of an uppercase alphabetical character 
		  and an integer, else false.
	EXAMPLE: validateSolution([(#"A",1)]) = true
	VARIANT: length l

	neitherIn()
*)

fun validateSolution [] = false
  | validateSolution ((letter,digit)::solution) =
    let
	fun neitherIn [] = true
	  | neitherIn ((l,d)::rest) =
	     letter<>l andalso
	     digit<>d andalso
	     neitherIn (rest)
    in
	Char.isUpper letter andalso
	0 <= digit andalso digit <= 9 andalso
	neitherIn(solution) andalso
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



(*	check ((x,y), l) 
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
	fun getDigit (letter, (l,digit)::rest) =
	    if l=letter then digit else getDigit(letter,rest)
					
	fun getValueOfWord "" = 0
	  | getValueOfWord word = 
	    getValueOfWord(
	      String.substring( word, 0, size(word)-1 )
	    ) * 10 + getDigit(
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
		fun allOk [] = true
		  | allOk (word::rest) = String.sub(word,0) <> letter
					 andalso allOk(rest)
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
	  sumList (map getValueOfWord addends) = getValueOfWord(sum)
      end;
    
(1, check((["SEND","MORE"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(2, check((["SEND","MORE"],"MONEY"),[(#"D",3),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
(3, check((["SEND","MORe"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
