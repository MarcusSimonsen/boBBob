(*
    This file is used for testing the different parser used in this project.
    For this file to run, please comment out line 15 in the project file (the line including the Parser.fsi file)
    and make sure line 60 is run in Program.fs ('ExpressionTesting.testAllParsers ()').
*)
module ExpressionTesting
    open FParsecLight.TextParser
    open Parser
    open Eval
    open StateMonad

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
    
    let testSquareBoardFun () =
        let toString =
            function
            | Success (Some x) -> string x
            | Success None -> "#"
            | Failure err -> failwith (sprintf "Error: %A" err)

        let abs v result = ITE (v .<. N 0, Ass (result, v .*. N -1), Ass (result, v))

        let checkSquare f v els = ITE (f "xabs" "yabs", Ass ("_result_", N v), els)
        let insideCheck x y = ((V x .<. N 8) .&&. (V y .<. N 8))

        let twsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 7)) .||.
                            ((V x .=. N 7) .&&. ((V y .=. N 7) .||. (V y .=. N 0)))
        let dwsCheck x y = (V x .=. V y) .&&. (V x .<. N 7) .&&. (V x .>. N 2)
        let tlsCheck x y = ((V x .=. N 6) .&&. (V y .=. N 2)) .||.
                            ((V x .=. N 2) .&&. ((V y .=. N 2) .||. (V y .=. N 6)))
        let dlsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 4)) .||.
                            ((V x .=. N 1) .&&. ((V y .=. N 1) .||. (V y .=. N 5)))
                        .||.
                            ((V x .=. N 4) .&&. ((V y .=. N 0) .||. (V y .=. N 7)))
                        .||.
                            ((V x .=. N 5) .&&. (V y .=. N 1)) .||.
                            ((V x .=. N 7) .&&. (V y .=. N 4))

        let standardBoard =
            Seq (Declare "xabs",
                Seq (Declare "yabs",
                    Seq (abs (V "_x_") "xabs",
                        Seq (abs (V "_y_") "yabs",
                            checkSquare twsCheck 4
                                (checkSquare dwsCheck 3
                                    (checkSquare tlsCheck 2
                                        (checkSquare dlsCheck 1
                                            (checkSquare insideCheck 0
                                                (Ass ("_result_", N -1))))))))))

        let identity = [(0, 0); (1, 1); (2, 2); (3, 3); (4, 4)] |> Map.ofList

        for y in -10..10 do
            for x in -10..10 do
                printf "%s "
                    (stmntToBoardFun standardBoard identity (x, y) |> toString)
            printfn ""

    let testSquareBoardFun2 () =
        let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
        let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
        let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

        let arithDoubleWordScore = N 2 .*. V "_acc_"
        let arithTripleWordScore = N 3 .*. V "_acc_"

        let stmntSingleLetterScore = Ass ("_result_", arithSingleLetterScore)
        let stmntDoubleLetterScore = Ass ("_result_", arithDoubleLetterScore)
        let stmntTripleLetterScore = Ass ("_result_", arithTripleLetterScore)

        let stmntDoubleWordScore = Ass ("_result_", arithDoubleWordScore)
        let stmntTripleWordScore = Ass ("_result_", arithTripleWordScore)

        let singleLetterScore = stmntToSquareFun stmntSingleLetterScore
        let doubleLetterScore = stmntToSquareFun stmntDoubleLetterScore
        let tripleLetterScore = stmntToSquareFun stmntTripleLetterScore

        let doubleWordScore = stmntToSquareFun stmntDoubleWordScore
        let tripleWordScore = stmntToSquareFun stmntTripleWordScore




        let SLS  = Map.empty |> Map.add 0 stmntSingleLetterScore
        let DLS  = Map.empty |> Map.add 0 stmntDoubleLetterScore
        let TLS  = Map.empty |> Map.add 0 stmntTripleLetterScore

        let DWS  = SLS |> Map.add 1 stmntDoubleWordScore
        let TWS  = SLS |> Map.add 1 stmntTripleWordScore

        let abs v result = ITE (v .<. N 0, Ass (result, v .*. N -1), Ass (result, v))

        let twsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 7)) .||.
                            ((V x .=. N 7) .&&. ((V y .=. N 7) .||. (V y .=. N 0)))

        let dwsCheck x y = (V x .=. V y) .&&. (V x .<. N 7) .&&. (V x .>. N 2)

        let tlsCheck x y = ((V x .=. N 6) .&&. (V y .=. N 2)) .||.
                            ((V x .=. N 2) .&&. ((V y .=. N 2) .||. (V y .=. N 6)))

        let dlsCheck x y = ((V x .=. N 0) .&&. (V y .=. N 4)) .||.
                            ((V x .=. N 1) .&&. ((V y .=. N 1) .||. (V y .=. N 5))) .||.
                            ((V x .=. N 4) .&&. ((V y .=. N 0) .||. (V y .=. N 7))) .||.
                            ((V x .=. N 5) .&&. (V y .=. N 1)) .||.
                            ((V x .=. N 7) .&&. (V y .=. N 4))

        let insideCheck x y = ((V x .<. N 8) .&&. (V y .<. N 8))

        let checkSquare f v els = ITE (f "xabs" "yabs", Ass ("_result_", N v), els)
        
        let standardBoardFun =
            Seq (Declare "xabs",
                Seq (Declare "yabs",
                    Seq (abs (V "_x_") "xabs",
                        Seq (abs (V "_y_") "yabs",
                                checkSquare twsCheck 4 
                                    (checkSquare dwsCheck 3 
                                        (checkSquare tlsCheck 2 
                                            (checkSquare dlsCheck 1
                                                (checkSquare insideCheck 0
                                                    (Ass ("_result_", N -1))))))))))

        let toString =
            function
            | Success (Some x) -> string x
            | Success None    -> "#"
            | Failure err     -> failwith (sprintf "Error: %A" err)

        printfn "Testing stmntToBoardFun"
        (* Expceted output:
            # # # # # # # # # # # # # # # # # # # # # 
            # # # # # # # # # # # # # # # # # # # # # 
            # # # # # # # # # # # # # # # # # # # # # 
            # # # 4 0 0 1 0 0 0 4 0 0 0 1 0 0 4 # # # 
            # # # 0 3 0 0 0 2 0 0 0 2 0 0 0 3 0 # # # 
            # # # 0 0 3 0 0 0 1 0 1 0 0 0 3 0 0 # # # 
            # # # 1 0 0 3 0 0 0 1 0 0 0 3 0 0 1 # # # 
            # # # 0 0 0 0 3 0 0 0 0 0 3 0 0 0 0 # # # 
            # # # 0 2 0 0 0 2 0 0 0 2 0 0 0 2 0 # # # 
            # # # 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 # # # 
            # # # 4 0 0 1 0 0 0 0 0 0 0 1 0 0 4 # # # 
            # # # 0 0 1 0 0 0 1 0 1 0 0 0 1 0 0 # # # 
            # # # 0 2 0 0 0 2 0 0 0 2 0 0 0 2 0 # # # 
            # # # 0 0 0 0 3 0 0 0 0 0 3 0 0 0 0 # # # 
            # # # 1 0 0 3 0 0 0 1 0 0 0 3 0 0 1 # # # 
            # # # 0 0 3 0 0 0 1 0 1 0 0 0 3 0 0 # # # 
            # # # 0 3 0 0 0 2 0 0 0 2 0 0 0 3 0 # # # 
            # # # 4 0 0 1 0 0 0 4 0 0 0 1 0 0 4 # # # 
            # # # # # # # # # # # # # # # # # # # # # 
            # # # # # # # # # # # # # # # # # # # # # 
            # # # # # # # # # # # # # # # # # # # # #   
        *)
        let ids = [(0, SLS); (1, DLS); (2, TLS); (3, DWS); (4, TWS)] |> Map.ofList
        let boardTest = [(0, 0); (1, 1); (2, 2); (3, 3); (4, 4)] |> Map.ofList
        
        for y in -10..10 do
            for x in -10..10 do
                printf "%s "
                    (stmntToBoardFun standardBoardFun boardTest (x, y) |> toString)
            printfn ""

        
        printfn ""


        // printfn "Testing mkBoard"
        // let standardBoard = 
        //     mkBoard (0, 0) 0 standardBoardFun ids

        
        // let evalSquare w pos acc =
        //     function
        //     | Success (Some m) -> 
        //         match (Map.find 0 m) w pos acc with
        //         | Success res -> Success (Some res)
        //         | Failure e   -> Failure e
        //     | Success None -> Success None
        //     | Failure e        -> Failure e


        // for y in -10..10 do
        //     for x in -10..10 do
        //         printf "%s " (standardBoard.squares (x, y) |> evalSquare hello 1 3 |> toString)
        //     printfn ""
