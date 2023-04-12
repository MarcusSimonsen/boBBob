// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = S (fun s ->
        match s.vars with
        | [] -> failwith "empty stack..."
        | _ :: ms -> Success ((), { s with vars = ms }))

    let wordLength : SM<int> =
        S (fun s -> Success(s.word.Length, s))

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            match evalSM s wordLength with
            | Success wl when pos >= wl || pos < 0 -> Failure (IndexOutOfBounds pos)
            | _ -> Success (fst s.word.[pos] , s))

    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            match evalSM s wordLength with
            | Success wl when pos >= wl || pos < 0 -> Failure (IndexOutOfBounds pos)
            | _ -> Success (snd s.word.[pos] , s)) 

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let legalVarName (var : string) : SM<unit> =
        S (fun s ->
            if Set.contains var s.reserved
            then Failure (ReservedName var)
            else Success ((), s))
    let declare (var : string) : SM<unit> =
            var
            |> legalVarName
            >>>= S (fun s ->
                    match Map.tryFind var s.vars.Head with
                    | Some _ -> Failure (VarExists var)
                    | None -> Success ((), {s with vars = Map.add var 0 s.vars.Head :: s.vars.Tail}))

    let update (var : string) (value : int) : SM<unit> =
        let rec aux =
            function
            | [] -> []
            | m :: ms ->
                match Map.tryFind var m with
                | None -> m :: aux ms
                | Some _ -> Map.add var value m :: ms
        
        S (fun s ->
            match evalSM s (lookup var) with
            | Success _ -> Success ((), {s with vars = aux s.vars})
            | Failure err -> Failure err)