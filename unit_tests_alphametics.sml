use "alphametics";

(*
(1,uniqueLetters([#"A",#"B",#"A"]) = [#"B",#"A"]);
(2,uniqueLetters ([#"M",#"O",#"N",#"E",#"Y"]) = [#"Y",#"E",#"N",#"O",#"M"]);
(3,uniqueLetters ([#"A",#"N",#"N",#"A"]) = [#"N",#"A"]);
*)
print("\nTesting validatePuzzle.\n");
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

print("\nTesting validateSolution.\n");
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

(*
(1, checkMapping("SENDMOREMONEY",[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(2, checkMapping("SENDMOREMONEY",[(#"D",7),(#"L",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
*)

print("\nTesting check.\n");
(1, check((["SEND","MORE"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = true);
(2, check((["SEND","MORE"],"MONEY"),[(#"D",3),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
(3, check((["SEND","MORe"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
(4, check((["SEND","MORR"],"MONEY"),[(#"D",7),(#"E",5),(#"M",1),(#"N",6),(#"O",0),(#"R",8),(#"S",9),(#"Y",2)]) = false);
