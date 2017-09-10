module SqlParser where

main =
    let tokens = reverse $ lexer Start "SELECT * test1, 'SELECT' FROM Table" []
    in
        mapM_ (putStrLn . show) $ tokens

data Token = Whitespace
    | Literal String
    | Identifier String
    | QuotedString String deriving (Show)

data State = Start | EOF | WS | Char | StartQuote | EndQuote Char | Error String deriving (Show)

type StateFn = String -> [Char] -> (Token, String, State)

quoteFn :: StateFn
quoteFn s cs = case s of
    []        -> (QuotedString (reverse cs), [], (Error "Stream ended inside Quotes"))
    ('\'':y:ys) -> (QuotedString (reverse cs), ys, EndQuote y)
    (x:xs)    -> quoteFn xs (x:cs)

charFn :: StateFn
charFn s cs = case s of
    []        -> (lookupId (reverse cs), [], EOF)
    (' ':xs)  -> (lookupId (reverse cs), xs, WS)
    (x:xs)    -> charFn xs (x:cs)

wsFn :: StateFn
wsFn s cs = case s of
    []       -> (Whitespace, [], EOF)
    ('\'':xs) -> (Whitespace, xs, StartQuote)
    (' ':xs) -> wsFn s []
    (x:xs)   -> (Whitespace, s, Char)


identifiers :: [String]
identifiers = [
     "*"
    ,"DELETE"
    ,"FROM"
    ,"SELECT"
    ,"UPDATE"
    ,"VALUES"]

lookupId :: String -> Token
lookupId s = case s `elem` identifiers of
    True  -> Identifier s
    False -> Literal s

stateMap :: State -> StateFn
stateMap Start        = charFn
stateMap WS           = wsFn
stateMap Char         = charFn
stateMap StartQuote   = quoteFn
stateMap (EndQuote c) = case c of
    ' ' -> stateMap WS
    _   -> stateMap Char

lexer :: State -> String -> [Token] -> [Token]
lexer state s ts = case (stateMap state) s [] of
    (token, _, EOF) -> token:ts
    (token, remainingString, newState) -> lexer newState remainingString (token:ts)
