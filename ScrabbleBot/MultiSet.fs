// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet
   type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>

    let empty : MultiSet<'a> = MS Map.empty
    let numItems : 'a -> MultiSet<'a> -> uint32 = fun a (MS s) ->
        match s.TryFind a with
        | None -> 0u
        | Some i -> i
    let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun a n (MS s) ->
        match n with
        | 0u -> MS s
        | n when n < 0u -> failwith "Can't insert negative amount"
        | n -> MS (s.Add (a, n + numItems a (MS s)))
    let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun f acc (MS s) -> Map.fold f acc s
