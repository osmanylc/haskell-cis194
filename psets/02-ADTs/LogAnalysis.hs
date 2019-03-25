{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage s = 
    case logType of "E" -> let (n:t:m) = ws
                               nI = read n :: Int
                               tI = read t :: Int
                               mS = unwords m
                           in LogMessage (Error nI) tI mS
                    "W" -> let (t:m) = ws
                               tI = read t :: Int
                               mS = unwords m
                           in LogMessage Warning tI mS
                    "I" -> let (t:m) = ws
                               tI = read t :: Int
                               mS = unwords m
                           in LogMessage Warning tI mS
                    _ -> Unknown s
    where (logType:ws) = words s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm@(LogMessage _ ts _) tree = 
    case tree of Leaf -> Node Leaf lm Leaf

                 (Node lt nlm@(LogMessage _ nts _) rt) -> 
                    if ts > nts 
                    then (Node lt nlm (insert lm rt))
                    else (Node (insert lm lt) nlm rt)

                 _ -> tree


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (l:ls) = insert l (build ls)


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt lm rt) = inOrder lt ++ [lm] ++ inOrder rt


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map getM . filter isSevere . inOrder . build)
    where isSevere :: LogMessage -> Bool
          isSevere (LogMessage (Error sev) _ _) = sev >= 50
          isSevere _ = False

          getM :: LogMessage -> String
          getM (LogMessage _ _ m) = m
          getM _ = ""

