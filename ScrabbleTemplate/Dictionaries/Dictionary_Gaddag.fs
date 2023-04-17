module Dictionary
    type Val =
        | C of char
        | N
    type Dict =
        | Leaf of bool
        | Node of bool * Map<Val, Dict>

    let empty () = Leaf false

    let c2v : char -> Val = fun (c : char) ->
        match c with
        | c when c = char 0 -> N
        | c -> C c

    let rec ins (s : string) (dict : Dict) =
        match dict with
        | Leaf _ when String.length s = 0 -> Leaf true
        | Leaf isWord -> Node (isWord, Map [(c2v s.[0], ins s.[1..] (Leaf false))])
        | Node (_, children) when s.Length = 0 -> Node(true, children)
        | Node (isWord,children) -> 
            match children.TryFind (c2v s.[0]) with
            | None -> Node (isWord, children.Add (c2v s.[0], ins s.[1..] (Leaf false)))
            | Some child -> 
                Node(isWord, children.Add(c2v s.[0], ins s.[1..] child))

    let reverseS : string -> string =
        Seq.rev
        >> Seq.toArray
        >> System.String

    let insert (s : string) (dict : Dict) =
        List.fold (fun (acc, i) _ -> (ins ((reverseS s.[0..i]) + string (char 0) + (s.Substring (i+1))) acc, i+1)) (dict, 0) (Seq.toList s)
        |> fst

    let step (c : char) (dict : Dict) =
        match dict with
        | Leaf _ -> None
        | Node (_, children) -> 
            match children.TryFind (c2v c) with
            | None -> None
            | Some child -> 
                match child with
                | Leaf isWord -> Some (isWord, child)
                | Node (isWord, _) -> Some (isWord, child)

    let reverse (dict : Dict) =
        match dict with
        | Node (_, map) ->
            match Map.containsKey (char 0 |> c2v) map with
            | true -> Some ((
                match map.[char 0 |> c2v] with
                | Node (b, _) -> b
                | Leaf b -> b)
                , map.[char 0 |> c2v])
            | false -> None
        | Leaf _ -> None
    
    let debugKeys (dict : Dict) =
        match dict with
        | Node (_, map) -> Map.toList map |> List.fold (fun acc e -> acc + (string e)) ""
        | Leaf _ -> "Dict is Leaf, therefore no keys"