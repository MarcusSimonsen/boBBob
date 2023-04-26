// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad
    open System.Text.RegularExpressions

    let add a b = (
        a >>= fun x ->
        b >>= fun y ->
        ret ((+) x y))
    let div a b = (
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0
        then ret ((/) x y)
        else fail DivisionByZero)
    let sub a b = (
        a >>= fun x ->
        b >>= fun y ->
        ret ((-) x y))
    let mul a b = (
        a >>= fun x ->
        b >>= fun y ->
        ret (x * y))
    let md_ a b = (
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0
        then ret ((%) x y)
        else fail DivisionByZero)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq  of aExp * aExp (* numeric equality *)
       | ANEq of aExp * aExp (* numeric inequality *)
       | ALt  of aExp * aExp (* numeric less than *)
       | ALte of aExp * aExp (* numeric less than or equal to *)
       | AGt  of aExp * aExp (* numeric greater than *)
       | AGte of aExp * aExp (* numeric greather than or equal to *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)
       | Disj of bExp * bExp  (* boolean disjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with
        | N i -> ret i
        | V v -> lookup v
        | WL -> wordLength
        | PV a -> arithEval a >>= pointValue

        | Add (a, b) -> add (arithEval a) (arithEval b)
        | Sub (a, b) -> sub (arithEval a) (arithEval b)
        | Mul (a, b) -> mul (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mod (a, b) -> md_ (arithEval a) (arithEval b)

        | CharToInt c -> charEval c >>= (int >> ret)

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= characterValue
        | ToUpper c -> charEval c >>= (System.Char.ToUpper >> ret)
        | ToLower c -> charEval c >>= (System.Char.ToLower >> ret)
        | IntToChar a -> arithEval a >>= (char >> ret)


    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
 
        | AEq (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x = y)
        | ANEq (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x <> y)
        | ALt (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x < y)
        | ALte (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x <= y)
        | AGt (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x > y)
        | AGte (a, b) ->
            arithEval a >>= fun x ->
            arithEval b >>= fun y ->
            ret (x >= y)

        | Not b -> boolEval b >>= (not >> ret)
        | Conj (a, b) ->
            boolEval a >>= fun x ->
            boolEval b >>= fun y ->
            ret (x && y)
        | Disj (a, b) ->
            boolEval a >>= fun x ->
            boolEval b >>= fun y ->
            ret (x || y)
        
        | IsVowel c -> 
            let rx = Regex (@"[aeiouy]", RegexOptions.Compiled)
            charEval c >>= fun c' ->
                ret (rx.IsMatch(string (System.Char.ToLower c')))
        | IsConsonant c ->
            boolEval ((~~) (IsVowel c)) >>= fun x ->
            ret x
        | IsLetter c ->
            boolEval ((.||.) (IsVowel c) (IsConsonant c)) >>= fun x ->
            ret x
        | IsDigit c ->
            charEval c >>= fun c' ->
                ret (System.Char.IsDigit c')

    type stmnt =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare var -> declare var
        | Ass (var, value) -> arithEval value >>= update var
        | Skip -> ret ()
        | Seq (stmnt1, stmnt2) -> stmntEval stmnt1 >>>= stmntEval stmnt2
        | ITE (bExp, stmnt1, stmnt2) ->
            boolEval bExp >>= fun condition ->
            push
            >>>= stmntEval (
                if condition
                then stmnt1
                else stmnt2)
            >>>= pop
        | While (bExp, stmnt) as wh ->
            boolEval bExp >>= fun condition ->
            if condition
            then push >>>= stmntEval stmnt >>>= pop >>>= stmntEval wh
            else stmntEval Skip

(* Part 3 (Optional) *)
//TODO: Maybe change to StateBuilder system
    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

(* Part 4 *)
    //TODO: 6.12
    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun : stmnt -> squareFun = fun stm ->
        fun w pos acc -> 
            stmntEval stm >>>=
            lookup "_result_" |>
            evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_";"_result_"])

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntsToSquare : Map<int, stmnt> -> Map<int, squareFun> = Map.map (fun _ value -> stmntToSquareFun value)

    let stmntToBoardFun : stmnt -> Map<int, 'a> -> coord -> Result<'a option, Error> = fun stm t (x, y) ->
        stmntEval stm >>>=
        lookup "_result_"
        |> evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"])
        |> (fun id ->
            match id with
            | Success id -> Success (Map.tryFind id t)
            | Failure err -> Failure err)

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"