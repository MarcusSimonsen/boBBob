module internal MultiSet
    type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>

    let empty : MultiSet<'a> = MS Map.empty

    let numItems : 'a -> MultiSet<'a> -> uint32 = fun a (MS s) ->
        match s.TryFind a with
        | None -> 0u
        | Some i -> i
    let isEmpty : MultiSet<'a> -> bool = fun (MS s) -> s.IsEmpty
    let size : MultiSet<'a> -> uint32 = fun (MS s) -> List.fold (fun acc key -> acc + s.[key]) 0u (s.Keys |> List.ofSeq)
    let contains : 'a -> MultiSet<'a> -> bool = fun a (MS s) -> Map.containsKey a s
    let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun a n (MS s) ->
        match n with
        | 0u -> MS s
        | n when n < 0u -> failwith "Can't insert negative amount"
        | n -> MS (s.Add (a, n + numItems a (MS s)))
    let addSingle a ms = add a 1u ms
    let remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun a n (MS s) ->
        match (numItems a (MS s)) with
        | x when n >= x -> MS (s.Remove a)
        | x -> MS (s.Add (a, x - n))
    let removeSingle a ms = remove a 1u ms
    let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun f acc (MS s) -> Map.fold f acc s
    let foldBack f (MS s) acc = Map.foldBack f s acc
    let ofList lst = List.foldBack addSingle lst empty
    let toList ms = foldBack (fun e x acc -> (List.replicate (int x) e) @ acc) ms []
    let map f ms = foldBack (fun e x acc -> add (f e) x acc) ms empty
    let union s1 s2 =
        List.fold (fun acc e -> e :: acc) (toList s1) (toList s2)
        |> List.distinct
        |> ofList
        |> fold (fun acc e _ -> add e (max (numItems e s1) (numItems e s2)) acc) empty
    let sum s1 s2 = foldBack add s1 s2 // No, 'let sum = foldBack add' doesn't work cause reasons - idfk ask Microsoft...
    let subtract s1 s2 = foldBack remove s2 s1
    let intersection s1 s2 = 
        List.fold (fun acc e -> e :: acc) (toList s1) (toList s2)
        |> List.distinct
        |> ofList
        |> fold (fun acc e _ -> add e (min (numItems e s1) (numItems e s2)) acc) empty