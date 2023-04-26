// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt = pstring "charToInt"
    let pToUpper   = pstring "toUpper"
    let pToLower   = pstring "toLower"
    let pCharValue = pstring "charValue"

    let pTrue     = pstring "true"
    let pFalse    = pstring "false"
    let pIsDigit  = pstring "isDigit"
    let pIsLetter = pstring "isLetter"
    let pIsVowel  = pstring "isVowel"

    let pif      = pstring "if"
    let pthen    = pstring "then"
    let pelse    = pstring "else"
    let pwhile   = pstring "while"
    let pdo      = pstring "do"
    let pdeclare = pstring "declare"
    let passign  = pstring ":="

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    // Boolean symbol parsing
    let pand    = pstring "/\\"
    let por     = pstring "\\/"
    let peq     = pstring "="
    let pneq    = pstring "<>"
    let plt     = pstring "<"
    let plte    = pstring "<="
    let pgt     = pstring ">"
    let pgte    = pstring ">="
    let pnot    = pstring "~"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "spaces1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let bracketise p = pchar '{' >*>. p .>*> pchar '}'
    let apostrophize p = pchar ''' >>. p .>> pchar '''
    let pvar = (many1 palphanumeric) |>> List.toArray |>> System.String

    let pid =
        pchar '_' <|> pletter
        .>>. many (palphanumeric <|> pchar '_')
        |>> (fun (a, b) ->
            a :: b
            |> List.toArray
            |> System.String)
        <?> "identifier"
    
    let stringTobExp (s : string) =
        match s with
        | "true" -> TT
        | "false" -> FF
        | _ -> failwith "Unknown input error"
    
    let unop op a    = op >*>. a
    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let GateParse, gref = createParserForwardedToRef<bExp>()
    let EquaParse, eref = createParserForwardedToRef<bExp>()
    let BoolParse, bref = createParserForwardedToRef<bExp>()
    let CondParse, dref = createParserForwardedToRef<stmnt>()
    let StmnParse, sref = createParserForwardedToRef<stmnt>()

    // Arith parser level 1 (Term)
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref.Value <- choice [AddParse; SubParse; ProdParse]

    // Arith parser level 2 (Prod)
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]

    // Arith parser level 3 (Atom)
    let VParse   = pid |>> V <?> "Variable"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (.*.) (N -1) x) <?> "Neg"
    let PVParse  = unop pPointValue ParParse |>> PV <?> "PointValue"
    let CharToIntParse = unop pCharToInt CharParse |>> CharToInt <?> "CharToInt"
    do aref.Value <- choice [CharToIntParse; NegParse; ParParse; PVParse; VParse; NParse;]

    // Char parsing (CharParse)
    let CParParse       = parenthesise CharParse
    let CParse          = apostrophize (pletter <|> whitespaceChar) |>> C <?> "C"
    let CVParse         = unop pCharValue AtomParse |>> CV          <?> "CV"
    let ToUpperParse    = unop pToUpper CharParse   |>> ToUpper     <?> "ToUpper"
    let ToLowerParse    = unop pToLower CharParse   |>> ToLower     <?> "ToLower"
    let IntToCharParse  = unop pIntToChar TermParse |>> IntToChar   <?> "IntToChar"
    do cref.Value <- choice [CParParse; IntToCharParse; ToUpperParse; ToLowerParse; CVParse; CParse;]

    // Logic Gates Parsing (Gate)
    let AndGParse   = binop pand EquaParse GateParse |>> Conj <?> "Conj"
    let OrGParse    = binop por  EquaParse GateParse |>> Disj <?> "Disj"
    do gref.Value <- choice [AndGParse; OrGParse; EquaParse;]

    // Comparison(equality) Parsing (Equa)
    let EqParse  = binop peq  TermParse TermParse |>> AEq  <?> "AEq"
    let NEqParse = binop pneq TermParse TermParse |>> ANEq <?> "ANEq"
    let LTParse  = binop plt  TermParse TermParse |>> ALt  <?> "ALt"
    let LTEParse = binop plte TermParse TermParse |>> ALte <?> "ALte"
    let GTParse  = binop pgt  TermParse TermParse |>> AGt  <?> "AGt"
    let GTEParse = binop pgte TermParse TermParse |>> AGte <?> "AGte"
    do eref.Value <- choice [NEqParse; LTEParse; GTEParse; EqParse; LTParse; GTParse; BoolParse;]

    // Boolean Parsing (Bool)
    let BParParse       = parenthesise GateParse
    let BNParse         = unop pnot GateParse       |>> Not      <?> "Not"
    let IsLetterParse   = unop pIsLetter CharParse  |>> IsLetter <?> "IsLetter"
    let IsVowelParse    = unop pIsVowel  CharParse  |>> IsVowel  <?> "IsVowel"
    let IsDigitParse    = unop pIsDigit  CharParse  |>> IsDigit  <?> "IsDigit"
    let TrueParse       = pTrue  |>> stringTobExp <?> "TT"
    let FalseParse      = pFalse |>> stringTobExp <?> "FF"
    do bref.Value <- choice [BParParse; BNParse; IsLetterParse; IsVowelParse; IsDigitParse; TrueParse; FalseParse;]

    let SeqParse    = binop (pchar ';') StmnParse CondParse |>> Seq <?> "Seq"
    let ITEParse =
                     pif    >*>. GateParse
                .>*> pthen .>*>. StmnParse
                .>*> pelse .>*>. StmnParse 
                    |>> (fun (d, c) -> d |> (fun (a, b) -> a, b, c))
                    |>> ITE <?> "ITE"
    let ITParse =
                     pif    >*>. GateParse
                .>*> pthen .>*>. StmnParse
                    |>> (fun (a, b) -> (a, b, Skip))
                    |>> ITE <?> "ITE"
    let WhileParse  = pwhile >*>. GateParse .>*> pdo   .>*>. StmnParse |>> While <?> "While"
    do dref.Value <- choice [SeqParse; ITEParse; ITParse; WhileParse; StmnParse;]

    let BracketParse = bracketise StmnParse
    let DeclareParse = unop (pdeclare .>>. spaces1) pvar |>> Declare <?> "Declare"
    let AssignParse = binop passign pvar TermParse |>> Ass <?> "Ass"
    do sref.Value <- choice [BracketParse; DeclareParse; AssignParse;]

    let AexpParse = TermParse 
    let CexpParse = CharParse
    let BexpParse = GateParse
    let stmntParse = CondParse


    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }


    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
