(*
Training test cases and auxiliary functions for PKD homework
assignment 1 (alphametic puzzles), 2013/2014

by Johannes Åman Pohjola (johannes.aman-pohjola@it.uu.se)
last updated 15/11 2013
no copyright, no licenses, omnia sunt communia

This file contains the functions makePuzzle, printPuzzle and
printSolution described in the assignment instructions.  It also
contains four simple test cases for alphametic puzzles that your
solution is expected to handle correctly.  To test your code with
these test cases, start Poly/ML and enter

  > use "alphametics.sml";
  > use "assignment1_tests.sml";
  > test ();

Optionally (if you have implemented solve), uncomment the last part of
this file and also enter

  > test_solve ();

If this file does not compile, it is either because you forgot to load
the hand-in you want to test, or because your hand-in doesn't follow
the specification.  Be warned that if your hand-in cannot successfully
run these tests, it might not be considered a serious attempt (and
thus be worth 0 assignment points).

Note that these tests are not meant to be exhaustive: correctly
executing these tests doesn't guarantee that your program is correct.

Good luck with the assignment! If you have any questions regarding the
assignment or these tests, please feel free to ask me by e-mail.

/Johannes
*)

type puzzle = string list * string;
type solution = (char * int) list;

exception Error of string;

(* makePuzzle summands
   TYPE: int list -> puzzle
   PRE:  summands is a non-empty list of positive integers
   POST: an alphametic puzzle whose solution is summands
   EXAMPLE: makePuzzle [10,2] = (["BA","C"], "BC")
 *)
fun makePuzzle summands =
  if null summands then
    raise Error "makePuzzle: list of summands must be non-empty"
  else if List.exists (fn i => i <= 0) summands then
    raise Error "makePuzzle: list of summands may only contain positive integers"
  else
    let
      fun string_of_int i =
        String.map (fn c => chr (ord #"A" + ord c - ord #"0")) (Int.toString i)
      val sum = foldl op+ 0 summands
    in
      (map string_of_int summands, string_of_int sum)
    end;

(* printPuzzle p
   TYPE: puzzle -> unit
   PRE:  p is well-formed
   POST: ()
   SIDE-EFFECTS: pretty-prints p
   EXAMPLE: printPuzzle (["SEND","MORE"],"MONEY") prints
              S E N D
            + M O R E
            ---------
            M O N E Y
 *)
fun printPuzzle (summands, sum) =
  let
    val width = 2 * foldl Int.max (size sum) (map (fn x => size x + 1) summands) - 1
    val summands' = List.take (summands, length summands - 1) @ ["+" ^ List.last summands]
    val spaced = String.concatWith " " o map str o explode
    fun println s =
      print (s ^ "\n")
  in
    List.app println
      (map (StringCvt.padLeft #" " width o spaced) summands');
    println (StringCvt.padLeft #"-" width "");
    println (StringCvt.padLeft #" " width (spaced sum))
  end handle _ => raise Error "printPuzzle: ill-formed puzzle";

(* printSolution (p, s)
   TYPE: puzzle * solution -> unit
   PRE:  s is a well-formed (partial) solution to the well-formed puzzle p
   POST: ()
   SIDE-EFFECTS: pretty-prints s
   EXAMPLE: printSolution ((["SEND","MORE"],"MONEY"), [(#"D", 7), (#"E", 5), (#"M", 1), (#"N", 6), (#"O", 0),(#"R", 8), (#"S", 9), (#"Y", 2)])
            prints
              9 5 6 7
            + 1 0 8 5
            ---------
            1 0 6 5 2
 *)
fun printSolution ((summands, sum), solution) =
  let
    fun solutionOf c =
      (Int.toString o #2 o valOf o List.find (fn (c', _) => c' = c)) solution
        handle _ => str c
    val sseq = String.concat o map solutionOf o explode
  in
    printPuzzle (map sseq summands, sseq sum)
  end handle _ => raise Error "printSolution: ill-formed puzzle";


(******************************************************************************)
(* To run the code below, the functions validatePuzzle, validateSolution and  *)
(* check must be declared and have the correct type.                          *)
(******************************************************************************)

(* let's make sure these functions are in fact declared and have the correct type *)
validatePuzzle: puzzle -> bool;
validateSolution: solution -> bool;
check: puzzle * solution -> bool;

(* result (n, f)
   TYPE: int * (int -> bool) -> unit
   PRE:  f is well-defined (i.e., terminates without error) for the call f n
   POST: ()
   SIDE-EFFECTS: Prints a report stating whether test n was successful or not
                 (where f n = true iff the test was successful)
*)
fun result (n, f) =
  print ("Test #" ^ Int.toString n ^ 
    ((if f n then " successful." else " FAILED!")
      handle _ =>
        " raised an (unwanted) exception!") ^ "\n");

(* test ()
   TYPE: unit -> unit
   PRE:  true
   POST: ()
   SIDE-EFFECTS: Prints a report, stating whether each test case performed as
                 expected.
 *)
fun test () =
  let
    (* test n
       TYPE: int -> bool
       PRE:  1<=n<=4
       POST: true iff test n executes correctly
     *)
    fun test 1 =
      let
        val john = [(#"E",1),(#"L",2),(#"V",3),(#"I",4),(#"N",5)]
        val jimmy = [(#"M",1),(#"C",2),(#"C",3),(#"O",4),(#"Y",5)]
        val pharoah = [(#"R",1),(#"A",2),(#"S",3),(#"H",4),(#"I",5),(#"E",5),(#"D",6)]
      in
        validateSolution john = true andalso
        validateSolution jimmy = false andalso
        validateSolution pharoah = false
      end
      | test 2 =
      let
        val JulyZerg = (["SLAYERSBOXER"],"GARIMTO")
        val Jaedong = (["NALRA","NADA"],"FLASH")
      in
        validatePuzzle JulyZerg = false andalso
	validatePuzzle Jaedong = true
      end
      | test 3 = 
      let
        val cersei = (["SEND","MORE"],"MONEY")
        val jaime = [(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]
        val robert = [(#"D",5),(#"E",7),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]
      in
        check (cersei, jaime) = true andalso
        check (cersei, robert) = false
      end
      | test 4 =
      let
        val isaac = (["A"],"A")
        val emmy = [(#"A",0)]
      in
          validatePuzzle isaac = true andalso
          validateSolution emmy = true andalso
          check (isaac, emmy) = false
      end
      | test _ = raise Domain
  in
    List.app result [(1, test), (2, test), (3, test), (4, test)]
  end;


(******************************************************************************)
(* This part is commented out because implementing solve is optional. If you  *)
(* have in fact implemented solve, feel free to uncomment the code below to   *)
(* test your solve function. Again, these tests are not meant to be           *)
(* exhaustive.                                                                *)
(******************************************************************************)



(******************************************************************************)
(* To run the code below, the function solve must be declared and have the    *)
(* correct type.                                                              *)
(******************************************************************************)

(* let's make sure solve is in fact declared and has the correct type *)
solve: puzzle -> solution option;

(* test_solve ()
   TYPE: unit -> unit
   PRE:  true
   POST: ()
   SIDE-EFFECTS: Prints a report, stating whether each test case performed as
                 expected.
 *)
fun test_solve () =
  let
    (* quickSort rel xs
       TYPE: ('a * 'a -> bool) -> 'a list -> 'a list
       PRE:  'a is (strictly or non-strictly) totally ordered under rel
       POST: xs sorted in increasing order by rel
       VARIANT: length xs
     *)
    fun quickSort op< (x::xs) =
      let
        val (ls, rs) = List.foldr (fn (e, (ls,rs)) => if e < x then (e::ls, rs) else (ls, e::rs)) ([],[]) xs
      in
        (quickSort op< ls) @ (x::quickSort op< rs)
      end
      | quickSort _ [] = []
    val sortByFirst = quickSort (fn ((a,_), (b,_)) => a < b)
    (* test n
       TYPE: int -> bool
       PRE:  1<=n<=2
       POST: true iff test n executes correctly
     *)
    fun test 1 =
      let
        (* confield is a sorted version of the solution, since order of
           elements in a solution is irrelevant. *)
        val confield = Option.map sortByFirst (solve (["A","B"], "CC"))
        val VIScosePoise = [(#"A",2),(#"B",9),(#"C",1)]
        val cfern = [(#"A",3),(#"B",8),(#"C",1)]
        val penExpers = [(#"A",4),(#"B",7),(#"C",1)]
        val simGishel = [(#"A",5),(#"B",6),(#"C",1)]
        val parhelicTriangle = [(#"A",6),(#"B",5),(#"C",1)]
        val bine = [(#"A",7),(#"B",4),(#"C",1)]
        val eideticCasein = [(#"A",8),(#"B",3),(#"C",1)]
        val uviol = [(#"A",9),(#"B",2),(#"C",1)]
      in
        confield = SOME VIScosePoise orelse
        confield = SOME cfern orelse
        confield = SOME penExpers orelse
        confield = SOME simGishel orelse
        confield = SOME parhelicTriangle orelse
        confield = SOME bine orelse
        confield = SOME eideticCasein orelse
        confield = SOME uviol
      end
      | test 2 =
        solve (["C","C"], "CC") = NONE
      | test _ = raise Domain
  in
    List.app result [(1, test), (2, test)]
  end;


