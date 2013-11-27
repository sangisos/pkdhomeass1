(*	validatePuzzle ((x,y))
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
	POST: true if checkCapitalization for every element in l returns true, else
		  false.
	EXAMPLE: checkWords ["SEND", "MORE"] = true
	VARIANT: length l

	listLettersInWords (l)
	TYPE: string list -> char list
	PRE: true
	POST: produces a list of uppercase alphabetical characters
	EXAMPLE: listLettersInWords (["MONEY","SEND","MORE"]) = 
	         [#"M",#"O",#"N",#"E",#"Y",#"S",#"E",#"N",#"D",#"M",#"O",#"R",#"E"]
	VARIANT: 

	uniqueLetters (l)
	TYPE: ''a list -> ''a list
	PRE: true
	POST: produces a list of uppercase alphabetical characters
	EXAMPLE: uniqueLetters ([#"M",#"O",#"N",#"E",#"Y"]) =
			 [#"Y",#"E",#"N",#"O",#"M"]
			 uniqueLetters ([#"A",#"N",#"N",#"A"]) =
			 [#"N",#"A"]
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

(*	validateSolution (l)
	TYPE: (char * int) list -> bool
	PRE: true
	POST: true if l is a list of unique tuples consisting of 
		  an uppercase alphabetical character and an integer, else false.
	EXAMPLE: validateSolution([(#"A",1)]) = true
			 validateSolution([(#"a",1)]) = false
	VARIANT: length l

	neitherIn (l)
	TYPE: (char * int) list -> bool
	PRE: true
	POST: true if all tuples in l are unique, else false.
	EXAMPLE: neitherIn([(#"A",1),(#"B",2)]) = true
			 neitherIn([(#"A",1),(#"B",1)]) = false
	VARIANT: length l
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

(*	check ((addends,sum), solution) 
	TYPE: (string list * string) * (char * int) list -> bool
	PRE: true
	POST: true if the first character in sum or addends is not mapped to 0, 
		  solution is a valid mapping and the value of addends and sum are 
		  equal by the mapping of solution.
	EXAMPLE: check ((["SEND","MORE"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),
			 (#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);

	checkMapping (addendsSumString)
	TYPE: string -> bool
	PRE: true
	POST: true if every letter in addendsSumString is present in solution.
	EXAMPLE: checkMapping ("HEJ") = true (if solution = [(#"H",1),(#"E",2),(#"J",3)])
	VARIANT: length addendsSumString

	existsIn (letter, l)
	TYPE: ''a * (''a * 'b) list -> bool 
	PRE: true
	POST: true if letter can be found in any tuple of l, else false.
	EXAMPLE: existsIn(#"H",[(#"H",1),(#"E",2),(#"J",3)]) = true
	VARIANT: length l

	getDigit (letter, l)
	TYPE: ''a * (''a * 'b) list -> 'b
	PRE: true
	POST: returns the value that letter is mapped to within l.
	EXAMPLE: getDigit (#"J",[(#"H",1),(#"E",2),(#"J",3)]) = 3
	VARIANT: length l

	getValueOfWord (word)
	TYPE: string -> int
	PRE: true
	POST: the sum of the letters in word by using getDigit.
	EXAMPLE: getValueOfWord ("HEJ") = 6 (given getDigit (#"J",[(#"H",1),(#"E",2),(#"J",3)]) = 3)
	VARIANT: length word
	*DISCLAIMER: influenced by severian vodalus presented in lab 4*

	sumList (l)
	TYPE: int list -> int
	PRE: true
	POST: the sum of all elements in l.
	EXAMPLE: sumList [1,2,3,4,6] = 16
	VARIANT: length l
	*DISCLAIMER: taken from lab 5*

	makeString (l)
	TYPE: string list -> string
	PRE: true
	POST: all of the strings in l concatinated with each other.
	EXAMPLE: makeString ["SEND","MORE"] = "SENDMORE"
	VARIANT: length l

	checkIfFirstIsNotZero (l)
	TYPE: (char * int) list -> bool
	PRE: true
	POST:
	EXAMPLE:
	VARIANT:

	firstLetterNotZero (l)
	TYPE:
	PRE:
	POST:
	EXAMPLE:
	VARIANT:


*)

fun check ((addends, sum), solution) = 
    validatePuzzle ((addends, sum))
    andalso validateSolution (solution)
    andalso
    let
	val addendsSumList = sum::addends
	fun checkMapping ("") = true
	  | checkMapping (addendsSumString) =
	    let
		fun existsIn (letter, []) = false
		  | existsIn (letter, (l,_)::rest) =
		    letter=l orelse
		    existsIn (letter, rest)
	    in
		existsIn(String.sub(addendsSumString,0), solution)
		andalso
		checkMapping(
   		  String.substring(addendsSumString, 1, 
				   size(addendsSumString)-1
				  )
		)
	    end
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
	
	fun makeString [] = ""
	  | makeString (word::rest) = word ^ makeString(rest)
	
	fun checkIfFirstIsNotZero ([]) = true
	  | checkIfFirstIsNotZero ((letter,0)::rest) = 
	    let
		fun firstLetterNotZero [] = true
		  | firstLetterNotZero (word::words) = 
		    String.sub(word,0) <> letter
		    andalso firstLetterNotZero(words)
	    in
		firstLetterNotZero(addendsSumList)
	    end
	  | checkIfFirstIsNotZero (_::rest) =
	    checkIfFirstIsNotZero (rest);
      in
	  checkIfFirstIsNotZero(solution)
	  andalso
	  checkMapping ( makeString(addendsSumList))
	  andalso
	  sumList (map getValueOfWord addends) = getValueOfWord(sum)
      end;

