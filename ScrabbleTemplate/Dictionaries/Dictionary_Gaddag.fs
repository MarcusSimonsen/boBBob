module Dictionary
    type Val =
        | C of char
        | N
    type Dictionary =
        | Leaf of bool
        | Node of bool * Map<Val, Dictionary>

    let empty () = Leaf false

    let c2v : char -> Val = fun c ->
        match c with
        | c when c = char 0 -> N
        | c -> C c

    let rec ins s dict =
        match dict with
        // End of string
        | Leaf _        when s = "" -> Leaf true
        | Node (_, map) when s = "" -> Node (true, map)

        // 1 char left
        | Leaf b        when s.Substring 1 = "" ->
            Node (b, Map [c2v s.[0], ins (s.Substring 1) (empty ())])
        | Node (b, map) when s.Substring 1 = "" ->
            Node (b, map.Add (c2v s.[0], ins (s.Substring 1) (
                match map.ContainsKey (c2v s.[0]) with
                | true -> Map.find (c2v s.[0]) map
                | false -> empty ())))

        // >1 chars left
        | Leaf b ->
            Node (b, Map [c2v s.[0], ins (s.Substring 1) (empty ())])
        | Node (b, map) ->
            Node (b, map.Add (c2v s.[0], ins (s.Substring 1) (
                match map.ContainsKey (c2v s.[0]) with
                | true -> Map.find (c2v s.[0]) map
                | false -> empty ())))

    let reverseS : string -> string =
        Seq.rev
        >> Seq.toArray
        >> System.String

    let insert (s : string) (dict : Dictionary) =
        List.fold (fun (acc, i) _ -> (ins ((reverseS s.[0..i]) + string (char 0) + (s.Substring 1)) acc, i+1)) (dict, 1) (Seq.toList s)
        |> fst

    let step (c : char) (dict : Dictionary) =
        match dict with
        | Node (_, map) when Map.containsKey (c2v c) map ->
            match map.[c2v c] with
            | Node (b, map) -> Some (b, Node (b, map))
            | Leaf _ -> None
        | Node _
        | Leaf _ -> None
        
    let reverse (dict : Dictionary) =
        match dict with
        | Node (_, map) ->
            match Map.containsKey (c2v (char 0)) map with
            | true ->
                match map.[c2v (char 0)] with
                | Node (b, _) -> Some (b, map.[c2v (char 0)])
                | Leaf _ -> None
            | false -> None
        | Leaf _ -> None