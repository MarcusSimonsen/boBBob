module Dictionary
    type Dictionary =
        | Leaf of bool
        | Node of bool * Map<char, Dictionary>

    let empty () = Leaf false

    let rec insert s dict =
        match dict with
        // End of string
        | Leaf _        when s = "" -> Leaf true
        | Node (_, map) when s = "" -> Node (true, map)

        // 1 char left
        | Leaf b        when s.Substring 1 = "" ->
            Node (b, Map [s.[0], insert (s.Substring 1) (empty ())])
        | Node (b, map) when s.Substring 1 = "" ->
            Node (b, map.Add (s.[0], insert (s.Substring 1) (
                match map.ContainsKey s.[0] with
                | true -> Map.find s.[0] map
                | false -> empty ())))

        // >1 chars left
        | Leaf b ->
            Node (b, Map [s.[0], insert (s.Substring 1) (empty ())])
        | Node (b, map) ->
            Node (b, map.Add (s.[0], insert (s.Substring 1) (
                match map.ContainsKey s.[0] with
                | true -> Map.find s.[0] map
                | false -> empty ())))
        
    let rec lookup s dict =
        match dict with
        | Leaf b when s = "" -> b
        | Leaf _ -> false
    
        | Node (b, _) when s = "" -> b
        | Node (_, map) ->
            match map.ContainsKey s.[0] with
            | true -> lookup (s.Substring 1) (Map.find (s.[0]) map)
            | false -> false
            
    let step c dict =
        match dict with
        | Node (_, map) when map.ContainsKey c ->
            Some (lookup "" map.[c], map.[c])
        | Node (_, map) ->
            Some (false, map.[c])
        
        | Leaf (b) ->
            None

    let reverse _ = failwith "Trie does not have reverse"