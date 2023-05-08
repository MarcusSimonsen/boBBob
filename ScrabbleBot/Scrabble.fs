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

    let printHand pieces hand pid =
        forcePrint (sprintf "\n-------- PLAYER %d'S TURN --------\n" pid )
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
        numOfPlayers  : uint32              // Num of players at the beginning of the game
        playerTurn    : uint32
        forfeited     : uint32 list         // Players no longer in the game
        hand          : MultiSet.MultiSet<uint32>
        tiles         : Map<coord, char>    // Tiles placed on boards
        canChange     : bool                // Is there enough tiles left to change
    }

    let mkState b d pn np pt h = {board = b; dict = d;  playerNumber = pn; numOfPlayers = np; playerTurn = pt; forfeited = []; hand = h; tiles = Map.empty; canChange = true;}

    let board st          = st.board
    let dict st           = st.dict
    let playerNumber st   = st.playerNumber
    let numOfPlayers st   = st.numOfPlayers
    let playerTurn st     = st.playerTurn
    let forfeited st      = st.forfeited
    let hand st           = st.hand
    let tiles st          = st.tiles
    let canChange st      = st.canChange


    let removeTiles : (coord * (uint32 * (char * int))) list -> state -> state = fun tilesToRemove st ->
        {st with hand = List.fold (fun acc (_, (cid, _)) -> MultiSet.removeSingle cid acc) st.hand tilesToRemove}
        
    let removeChangedTiles : uint32 list -> state -> state = fun tilesToRemove st ->
        {st with hand = List.fold (fun acc cid -> MultiSet.removeSingle cid acc) st.hand tilesToRemove}
    
    let addTiles : (uint32 * uint32) list -> state -> state = fun newTiles st ->
        {st with hand = List.fold (fun acc (cid, amount) -> MultiSet.add cid amount acc) st.hand newTiles}

    let placeTiles : (coord * (uint32 * (char * int))) list -> state -> state = fun newTiles st ->
        {st with tiles = List.fold (fun acc (cd, (_, (ch, _))) -> Map.add cd ch acc) st.tiles newTiles}
        
    let changePlayerTurn : state -> state = fun st ->
        let rec findNextPlayer pid = // Find the next player who has not forfeited
            if List.contains pid st.forfeited  
            then findNextPlayer (pid % st.numOfPlayers + 1u)
            else pid
        {st with playerTurn = (findNextPlayer (st.playerTurn % st.numOfPlayers + 1u))}
        
    let removePlayer : uint32 -> state -> state = fun pid st ->
        {st with forfeited = (pid :: st.forfeited)}
        
    let changeOrPass : state -> ServerMessage = (fun st ->
        debugPrint "boBBob couldn't find a word. Will try to change or pass\n" |> ignore
        if (st.canChange && ((MultiSet.size st.hand) > 6u))              // Check if it is possible to change tiles and if you have a full hand
        then SMChange(List.take 3 (MultiSet.toList st.hand))             // Possible to change - Changes the three first tiles on hand 
        else SMPass)                                                     // Not possible to change - Passes  
    
    let changeCanChange : state -> state = fun st ->
        {st with canChange = false}
        
module internal ScrabblePlays =
    type Direction =
        | UP
        | DOWN
        | RIGHT
        | LEFT
    
    // Returns char for given tile's id
    // if wild card, returns the letter A 
    let idToChar : uint32 -> Map<uint32, tile> -> char = fun id pieces ->
        Map.find id pieces
        |> (fun s -> Set.minElement s)
        |> fst
    
    // Returns point value for given tile's id
    let idToPoints : uint32 -> Map<uint32, tile> -> int = fun id pieces ->
        Map.find id pieces
        |> (fun s -> Set.minElement s)
        |> snd
        
    let completesWord : Dictionary.Dict -> bool = fun dict ->
        match Dictionary.reverse dict with
            | Some (b, _) -> b
            | None -> false
    
    // Used to find the first move of the game
    // Returns word with the highest score
    let rec findFirstWord : MultiSet.MultiSet<uint32> -> Map<uint32, tile> -> Dictionary.Dict -> int -> uint32 list -> int -> uint32 list -> (int * uint32 list) =
        fun hand pieces dict currentScore currentWord bestScore bestWord ->
        MultiSet.fold (fun (bscore, bword) c _ ->
            match Dictionary.step (idToChar c pieces) dict with
            | Some (_, dict') ->
                if completesWord dict' // End of a word
                then
                    if currentScore + (idToPoints c pieces) > bscore
                    then findFirstWord (MultiSet.removeSingle c hand) pieces dict' (currentScore + (idToPoints c pieces)) (c :: currentWord) (currentScore + (idToPoints c pieces)) (c :: currentWord)  // Update bestScore and bestWord 
                    else findFirstWord (MultiSet.removeSingle c hand) pieces dict' (currentScore + (idToPoints c pieces)) (c :: currentWord) bscore bword
                else 
                    findFirstWord (MultiSet.removeSingle c hand) pieces dict' (currentScore + (idToPoints c pieces)) (c :: currentWord) bscore bword
            | None -> (bscore, bword)
        ) (bestScore, bestWord) hand
    
    // Places first move on board
    let placeFirstMove : uint32 list -> Map<uint32, tile> -> (coord * (uint32 * (char * int))) list = fun chars pieces ->
        List.mapi (fun i ch -> ((i, 0), (ch, (idToChar ch pieces, idToPoints ch pieces)))) chars
    
    // Moves coord one place in the given direction
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
    let perpendicularDir : Direction -> Direction = fun dir ->
        match dir with
        | Direction.UP -> Direction.LEFT
        | Direction.DOWN -> Direction.LEFT
        | Direction.LEFT -> Direction.UP
        | Direction.RIGHT -> Direction.UP

    let rec checkDirectionForward : Map<coord, char> -> Dictionary.Dict  -> coord -> Direction -> Boolean =
        fun tiles dict currentCoordinate dir ->
            if Map.containsKey currentCoordinate tiles
            then
                match Dictionary.step tiles[currentCoordinate] dict with
                | Some (b, dict') ->
                    if Map.containsKey (coordPlusDir currentCoordinate dir) tiles
                    then checkDirectionForward tiles dict' (coordPlusDir currentCoordinate dir) dir
                    else b
                | None -> false
            else false

    let rec checkDirectionBack : Map<coord, char> -> Dictionary.Dict -> coord -> coord -> Direction -> Boolean =
        fun tiles dict originalCoordinate currentCoordinate dir ->
            if Map.containsKey currentCoordinate tiles
            then
                match Dictionary.step tiles[currentCoordinate] dict with
                | Some (_, dict') -> checkDirectionBack tiles dict' originalCoordinate (coordPlusDir currentCoordinate dir) dir
                | None -> false
            else
                match Dictionary.reverse dict with
                | Some (_, dict') -> checkDirectionForward tiles dict' (coordPlusDir originalCoordinate (oppositeDir dir)) (oppositeDir dir)
                | None -> false
    
    let checkDirections : Map<coord, char> -> Dictionary.Dict -> char -> coord -> Direction -> Boolean =
        fun tiles dict c coordinate dir ->
            if Map.containsKey (coordPlusDir coordinate (perpendicularDir dir)) tiles || Map.containsKey (coordPlusDir coordinate (oppositeDir (perpendicularDir dir))) tiles
            then
                match Dictionary.step c dict with
                | Some (_, dict') -> checkDirectionBack tiles dict' (coordPlusDir coordinate (perpendicularDir dir)) coordinate (perpendicularDir dir)
                | None -> false // Should never happen
            else true
    
    // Used to find a valid move (except the first move of the game)
    // Right now returns the first valid word it finds
    let findMoves : MultiSet.MultiSet<uint32> -> Map<uint32, tile> -> Dictionary.Dict -> coord -> Map<coord, char> -> ((coord * (uint32 * (char * int ))) list) list =
            fun hand pieces orgDict originalCoordinate tiles ->
        let rec aux : MultiSet.MultiSet<uint32> -> Dictionary.Dict -> coord -> Direction -> Boolean -> ((coord * (uint32 * (char * int ))) list) option =
            fun hand dict cd dir hasPlaced ->
            if Map.containsKey (coordPlusDir cd dir) tiles 
            then  // Tile is already placed on this coordinate - Handle check for expanding words
                match Dictionary.step tiles[coordPlusDir cd dir] dict with
                | Some (_, dict') -> aux hand dict' (coordPlusDir cd dir) dir hasPlaced
                | None -> None
            else  // Empty tile
                match Dictionary.reverse dict with
                | Some (b, dict') ->  // Reached end of word
                    if b && hasPlaced // Valid word is found using at least one tile from the hand -> return 
                    then Some []
                    else aux hand dict' originalCoordinate (oppositeDir dir) hasPlaced
                | None -> // Not reached end of word
                    MultiSet.fold (fun acc ch _ -> // Fold through tiles on hand, checking if they can be used to form a valid word
                        match acc with
                        | Some x -> Some x
                        | None ->
                            match Dictionary.step (idToChar ch pieces) dict with
                            | Some (b, dict') ->
                                if b && checkDirections tiles orgDict (idToChar ch pieces) (coordPlusDir cd dir) dir
                                then Some [coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))] // Place a new tile
                                else
                                    match aux (MultiSet.removeSingle ch hand) dict' (coordPlusDir cd dir) dir true with // Place a new tile
                                    | Some xs when checkDirections tiles orgDict (idToChar ch pieces) (coordPlusDir cd dir) dir -> Some ((coordPlusDir cd dir, (ch, (idToChar ch pieces, idToPoints ch pieces))) :: xs)
                                    | Some _ -> None
                                    | None -> acc
                            | None -> acc
                        ) None hand


        match Dictionary.step tiles[originalCoordinate] orgDict with // Finds starting tile in dict
        | Some (_, dict') -> 
            [aux hand dict' originalCoordinate Direction.LEFT false; aux hand dict' originalCoordinate Direction.UP false;] // Tries to find one valid word in direction left and one valid word in direction up
        | None -> []
        |> List.fold (fun acc word -> // Removes occurences of "None" in list
            match word with
            | Some w -> w :: acc
            | None -> acc
            ) []
       
module Scrabble =
    open System.Threading
    open ScrabblePlays

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            // Thread.Sleep(1000)  // Wait 1 second to finish prints in multiplayer
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            // let input =  System.Console.ReadLine()
            // let move = RegEx.parseMove input

            // If it is my turn 
            if (st.playerNumber = st.playerTurn) then
                Print.printHand pieces (State.hand st) (State.playerNumber st)
                // Search for move
                if Map.containsKey st.board.center st.tiles
                then
                    Map.fold (fun acc cd _ ->
                        findMoves (State.hand st) pieces (State.dict st) cd (State.tiles st) @ acc
                        ) [] (State.tiles st)
                    |> (fun ls ->
                        if List.length ls = 0 // If no moves are found
                        then st |> State.changeOrPass
                        else SMPlay ls[0]) 
                    |> send cstream
                else // If it is the first move
                    sprintf "%A\n" (findFirstWord (State.hand st) pieces (State.dict st) 0 [] 0 []) |> debugPrint
                    findFirstWord (State.hand st) pieces (State.dict st) 0 [] 0 []
                    |> (fun (_, word) ->
                        match word with
                        | [] -> st |> State.changeOrPass
                        | word -> 
                            word
                            |> placeFirstMove <| pieces
                            |> SMPlay
                            )
                    |> send cstream
            // Wait for your turn
            else () 

            // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream
            // debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you.*)
                let st' =
                    st
                    |> State.removeTiles ms     // Remove used tiles from hand
                    |> State.addTiles newPieces // Add new tiles to hand
                    |> State.placeTiles ms      // Update board
                    |> State.changePlayerTurn            // Change turn
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player.*)
                let st' =
                    st
                    |> State.placeTiles ms      // Update board
                    |> State.changePlayerTurn            // Change turn
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play by you or another player.*)
                let st' =
                    st
                    |> State.changePlayerTurn            // Change turn
                aux st'
            | RCM (CMChangeSuccess newPieces) ->
                (* You successfully changed pieces.*)
                let st' =
                     st
                     |> State.removeChangedTiles (List.take 3 (MultiSet.toList st.hand))// Remove old tiles - Always changes the three first tiles on hand
                     |> State.addTiles newPieces// Add new tiles to hand
                     |> State.changePlayerTurn           // Change turn
                aux st'
            | RCM (CMChange (pid, nt)) ->
                (* Another player changed tiles.*)
                let st' =
                    st
                    |> State.changePlayerTurn            // Change turn
                aux st'           
            | RCM (CMPassed pid) ->
                (* You or another player passed. (three passes in a row from all players ends the game) *)
                let st' =
                    st
                    |> State.changePlayerTurn           // Change turn
                aux st'
            | RCM (CMForfeit pid) ->
                (* Given player forfeited.*)
                let st' =
                    st
                    |> State.removePlayer pid  // Remove forfeited player
                    |> State.changePlayerTurn           // Change turn
                aux st'
            | RCM (CMTimeout pid) ->
                (* Given player timed out  - counts as passing *)
                let st' =
                    st
                    |> State.changePlayerTurn           // Change turn
                aux st'           
            | RCM (CMGameOver _) -> ()
            | RGPE errLst -> // Error handling
                match errLst[0] with
                | GPENotEnoughPieces _  -> let st' =                       // Not enough tiles left to change. Counts as pass instead
                                               st
                                               |> State.changeCanChange    // Make it not possible to change tiles anymore
                                               |> State.changePlayerTurn            // Change turn
                                           aux st'
                | _                     -> let st' =                       // Errors counts as passing so the players dont get stuck
                                               st
                                               |> State.changePlayerTurn   // Change turn
                                           aux st'
                //printfn "Gameplay Error:\n%A" errLst
        
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
                      Number of players = %d
                      Player id = %d
                      Player turn = %d
                      Hand =  %A
                      Timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        let dict = dictf true // Cause Gaddag
        let board = Parser.mkBoard boardP

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet)
        