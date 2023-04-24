(*
    This file is used for testing the different parser used in this project.
    For this file to run, please comment out line 15 in the project file (the line including the Parser.fsi file)
    and make sure line 60 is run in Program.fs ('ExpressionTesting.testAllParsers ()').
*)
module ExpressionTesting
    open FParsecLight.TextParser
    open Parser

    let testaExpParser () =
        printfn "\tTesting aExp Parser"
        run AexpParse "4"                      |> printfn "%A"
        run AexpParse "x2"                     |> printfn "%A"
        run AexpParse "5 + y * 3"              |> printfn "%A"
        run AexpParse "(5 - y) * -3"           |> printfn "%A"
        run AexpParse "pointValue (x % 4) / 0" |> printfn "%A"
        run AexpParse "-1 + 3"                 |> printfn "%A"
        run AexpParse "charToInt ('c')"        |> printfn "%A"
        run AexpParse "charToInt (' ')"        |> printfn "%A"
        run AexpParse "1 + 3 - 5"              |> printfn "%A"
        printfn ""
    
    let testcExpParser () =
        printfn "\tTesting cExp Parser"
        run CexpParse "'x'"                                    |> printfn "%A"
        run CexpParse "toLower (toUpper( 'x'))"                |> printfn "%A"
        run CexpParse "toLower (toUpper 'x')"                  |> printfn "%A"
        run CexpParse "intToChar (4)"                          |> printfn "%A"
        run CexpParse "intToChar (charToInt (' '))"            |> printfn "%A"
        run AexpParse "charToInt (charValue (pointValue (5)))" |> printfn "%A"
        printfn ""

    let testbExpParser () =
        printfn "\tTesting bExp Parser"
        run BexpParse "true"                        |> printfn "%A"
        run BexpParse "false"                       |> printfn "%A"
        run BexpParse "5 > 4 \/ 3 >= 7"             |> printfn "%A"
        run BexpParse "~false"                      |> printfn "%A"
        run BexpParse "(5 < 4 /\ 6 <= 3) \/ ~false" |> printfn "%A"
        run BexpParse "(5 < 4 \/ 6 <= 3) \/ ~true"  |> printfn "%A"
        printfn ""
    
    let teststmntExpParser () =
        printfn "\tTesting stmntExp Parser"
        run stmntParse "x := 5" |> printfn "%A"
        run stmntParse "declare myVar" |> printfn "%A"
        run stmntParse "declaremyVar" |> printfn "%A"
        run stmntParse "declare x; x := 5" |> printfn "%A"
        run stmntParse "declare x; x := 5  ;y:=7" |> printfn "%A"
        run stmntParse "if (x < y) then { x := 5 } else { y := 7 }" |> printfn "%A"
        run stmntParse "if (x < y) then { x := 5 }" |> printfn "%A"
        run stmntParse "while (true) do {x5 := 0} " |> printfn "%A"
        printfn ""
    
    let testAllParsers () =
        testaExpParser ()
        testcExpParser ()
        testbExpParser ()
        teststmntExpParser ()