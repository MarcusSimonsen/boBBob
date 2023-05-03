namespace boBBob

open System
open System.Diagnostics
open System.Net
open System.Xml.Xsl
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        tiles         : Map<coord, char>
    }

    let mkState b d pn h = {board = b; dict = d;  playerNumber = pn; hand = h; tiles = Map.empty }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let tiles st         = st.tiles

    let removeTiles : (coord * (uint32 * (char * int))) list -> state -> state = fun tilesToRemove st ->
        {st with hand = List.fold (fun acc (_, (cid, _)) -> MultiSet.removeSingle cid acc) st.hand tilesToRemove}
    
    let addTiles : (uint32 * uint32) list -> state -> state = fun newTiles st ->
        {st with hand = List.fold (fun acc (cid, amount) -> MultiSet.add cid amount acc) st.hand newTiles}

    let placeTiles : (coord * (uint32 * (char * int))) list -> state -> state = fun newTiles st ->
        {st with tiles = List.fold (fun acc (cd, (_, (ch, _))) -> Map.add cd ch acc) st.tiles newTiles}
module internal ScrabblePlays =
    type Direction =
        | UP
        | DOWN
        | RIGHT
        | LEFT
    let idToChar : uint32 -> Map<uint32, tile> -> char = fun id pieces ->
        Map.find id pieces
        |> (fun s -> Set.minElement s)
        |> fst
    let idToPoints : uint32 -> Map<uint32, tile> -> int = fun id pieces ->
        Map.find id pieces
        |> (fun s -> Set.minElement s)
        |> snd
    let completesWord : Dictionary.Dict -> bool = fun dict ->
        match Dictionary.reverse dict with
            | Some (b, _) -> b
            | None -> false
    let rec findFirstWord : MultiSet.MultiSet<uint32> -> Map<uint32, tile> -> Dictionary.Dict -> (uint32 list) option = fun hand pieces dict ->
        if completesWord dict
        then Some []
        else MultiSet.fold (fun acc c _ ->
            match acc with
            | Some x -> Some x
            | None ->
                match Dictionary.step (idToChar c pieces) dict with
                    | Some (_, dict') ->
                        match findFirstWord (MultiSet.removeSingle c hand) pieces dict' with
                        | Some xs -> Some (c :: xs)
                        | None -> None
                    | None -> None
                    ) None hand
    
    let placeFirstMove : uint32 list -> Map<uint32, tile> -> (coord * (uint32 * (char * int))) list = fun chars pieces ->
        List.mapi (fun i ch -> ((-i, 0), (ch, (idToChar ch pieces, idToPoints ch pieces)))) chars
    
    let coordPlusDir : coord -> Direction -> coord = fun cd dir ->
        match dir with
        | Direction.UP ->    cd |> (fun (x, y) -> (x,  y-1))
        | Direction.DOWN ->  cd |> (fun (x, y) -> (x,  y+1))
        | Direction.LEFT ->  cd |> (fun (x, y) -> (x-1, y ))
        | Direction.RIGHT -> cd |> (fun (x, y) -> (x+1, y ))
    let oppositeDir : Direction -> Direction = fun dir ->
        match dir with
        | Direction.UP -> Direction.DOWN
        | Direction.LEFT -> Direction.RIGHT
        | Direction.DOWN -> Direction.UP
        | Direction.RIGHT -> Direction.LEFT
    
    let findMove : MultiSet.MultiSet<uint32> -> Map<uint32, tile> -> Dictionary.Dict -> coord -> Map<coord, char> -> ((coord * (uint32 * (char * int ))) list) list =
            fun hand pieces dict originalCoordinate tiles ->
        let rec aux : MultiSet.MultiSet<uint32> -> Dictionary.Dict -> coord -> Direction -> Boolean -> ((coord * (uint32 * (char * int ))) list) option =
            fun hand dict cd dir hasPlaced ->
            if Map.containsKey (coordPlusDir cd dir) tiles
            then None // Handle check for other words
            else
                match Dictionary.reverse dict with
                | Some (b, dict') ->
                    if b && hasPlaced
                    then Some []
                    else
                        match aux hand dict' originalCoordinate (oppositeDir dir) hasPlaced with
                        | Some xs -> Some xs
                        | None ->
                            MultiSet.fold (fun acc ch _ ->
                                match acc with
                                | Some x -> Some x
                                | None ->
                                    match Dictionary.step (idToChar ch pieces) dict' with
                                    | Some (b, dict') ->
                                        if b
                                        then Some [coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))]
                                        else
                                            match aux (MultiSet.removeSingle ch hand) dict' (coordPlusDir cd dir) dir true with
                                            | Some xs -> Some ((coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))) :: xs)
                                            | None -> acc
                                    | None -> acc
                            ) None hand
                | None ->
                    MultiSet.fold (fun acc ch _ ->
                        match acc with
                        | Some x -> Some x
                        | None ->
                            match Dictionary.step (idToChar ch pieces) dict with
                            | Some (b, dict') ->
                                if b
                                then Some [coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))]
                                else
                                    match aux (MultiSet.removeSingle ch hand) dict' (coordPlusDir cd dir) dir true with
                                    | Some xs -> Some ((coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))) :: xs)
                                    | None -> acc
                            | None -> acc
                        ) None hand
                    
                                
        debugPrint "Find move\n"
        [aux hand dict originalCoordinate Direction.LEFT false; aux hand dict originalCoordinate Direction.UP false;]
        |> List.fold (fun acc word ->
            match word with
            | Some w -> w :: acc
            | None -> acc
            ) []
module Scrabble =
    open System.Threading
    open ScrabblePlays

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            // let move = RegEx.parseMove input

            // Search for move
            if Map.containsKey st.board.center st.tiles
            then
                debugPrint "Find not first move\n"
                Map.fold (fun acc cd _ ->
                    findMove (State.hand st) pieces (State.dict st) cd (State.tiles st) @ acc
                    ) [] (State.tiles st)
                |> (fun ls ->
                    debugPrint (sprintf "Moves found:\n%A\n" ls)
                    if List.length ls = 0
                    then SMPass
                    else SMPlay ls[0])
                |> send cstream
            else
                debugPrint "Find first move\n"
                findFirstWord (State.hand st) pieces (State.dict st)
                |> (fun word ->
                    match word with
                    | Some chars -> placeFirstMove chars pieces |> SMPlay 
                    | None -> debugPrint "boBBob couldn't find a word\n" |> ignore; SMPass)
                |> send cstream

            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' =
                    st
                    |> State.removeTiles ms     // Remove used tiles from hand
                    |> State.addTiles newPieces // Add new tiles to hand
                    |> State.placeTiles ms               // Update board
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                // Update board
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play by given player. Update your state (multiplayer - who's turn is it) *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newPieces) ->
                (* You succesfully changed pieces. Update your state *)
                let st' = st |> State.addTiles newPieces // Add new tiles to hand
                aux st'
            | RCM (CMPassed pid) ->
                (* Given player passed. Update your state (three passes in a row from all players end the game) *)
                let st' = st
                aux st'
            | RCM (CMForfeit pid) ->
                (* Given player forfeited. Update your state (in multiplayer keep track of who is still in the game) *)
                let st' = st
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Cause Gaddag
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet)
        