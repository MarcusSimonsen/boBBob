module Dictionary
    type Dict = 
        | Leaf of bool
        | Node of bool * Map<char,Dict>

    let empty () = Leaf false

    let rec insert s dict =
        match dict with
        | Leaf _ when String.length s = 0 -> Leaf true
        | Leaf isWord -> Node (isWord, Map [(s.[0], insert s.[1..] (Leaf false))])
        | Node (_, children) when s.Length = 0 -> Node(true, children)
        | Node (isWord,children) -> 
            match children.TryFind s.[0] with
            | None -> Node (isWord, children.Add (s.[0], insert s.[1..] (Leaf false)))
            | Some child -> 
                Node(isWord, children.Add(s.[0], insert s.[1..] child))

    let rec lookup s dict = 
        match dict with
        | Leaf isWord when String.length s = 0 -> isWord
        | Leaf _ -> false
        | Node (isWord, _) when String.length s = 0 -> isWord
        | Node (_, children) -> 
            match children.TryFind s.[0] with
            | None -> false
            | Some child -> lookup s.[1..] child

    let step c dict = 
        match dict with
        | Leaf _ -> None
        | Node (_, children) -> 
            match children.TryFind c with
            | None -> None
            | Some child -> 
                match child with
                | Leaf isWord -> Some (isWord, child)
                | Node (isWord, _) -> Some (isWord, child)

    let reverse _ = failwith "Trie does not have reverse"