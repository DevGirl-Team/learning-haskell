{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- EXERCICE 1

stringIsNumber :: String -> Bool
stringIsNumber str = 
    case str of 
        "" -> False
        (x:[]) -> isNumber x
        (x:xs) -> isNumber x && stringIsNumber xs
    where isNumber s = s `elem` ['0'..'9']

parseMessage :: String -> LogMessage
parseMessage str = 
    case w of 
        "E":s:t:rest
            | isError s t -> LogMessage (Error (read s)) (read t) (unwords rest)
        "I":t:rest
            | isTimestamp t -> LogMessage Info (read t) (unwords rest)
        "W":t:rest 
            | isTimestamp t -> LogMessage Warning (read t) (unwords rest)
        _ -> Unknown (unwords w)
    where w = words str
          isError severity timestamp = stringIsNumber severity && stringIsNumber timestamp
          isTimestamp timestamp = stringIsNumber timestamp

parse :: String -> [LogMessage]
parse str = parseList l
    where l = lines str
          parseList xs = 
            case xs of 
                [] -> []
                line:rest -> (parseMessage line):(parseList rest)

-- EXERCICE 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message@(LogMessage _ timestamp _) tree = 
    case tree of 
        Leaf -> Node Leaf message Leaf
        Node gTree node@(LogMessage _ nodeTimestamp _) lTree
            | nodeTimestamp <= timestamp -> Node gTree node (insert message lTree)
            | nodeTimestamp > timestamp -> Node (insert message gTree) node lTree

-- EXERCICE 3

build :: [LogMessage] -> MessageTree
build messages =
    case messages of
        [] -> Leaf
        m:rest -> insert m (build rest)

-- EXERCICE 4

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
    case tree of
        Leaf -> []
        Node gTree message lTree -> inOrder gTree ++ [message] ++ inOrder lTree

-- EXERCICE 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong messages = 
    [ content | (LogMessage mType timestamp content) <- filteredMessages, isSevereError mType ]
    where 
        filteredMessages = inOrder (build messages)
        isSevereError t =
            case t of
                (Error s) -> s >= 50
                _ -> False 