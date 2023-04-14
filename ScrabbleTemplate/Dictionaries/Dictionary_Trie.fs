
module Dictionary
    type Dict = 
        | Leaf of bool
        | Node of bool * Map * Dict * Dict

    let empty = Leaf false

    let rec insert s dict = match dict with
        | Leaf _ when s.length = 1 -> Leaf true
        | Node (_, l, r) when string.Length s = 1 -> Node true dict l r
        | Node (dict, l r) -> Map.find dict s[0]



    let rec insert x =
        function
        | Leaf _ when x = 0u -> Leaf true
        | Node (_, l, r) when x = 0u -> Node(true, l, r)
        | Leaf b when x % 2u = 0u -> Node(b, insert (x / 2u) empty, empty)
        | Leaf b -> Node(b, empty, insert (x / 2u) empty)
        | Node (b, l, r) when x % 2u = 0u -> Node(b, insert (x / 2u) l, r)
        | Node (b, l, r) -> Node(b, l, insert (x / 2u) r)