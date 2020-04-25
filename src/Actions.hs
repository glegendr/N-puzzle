module Actions
( Act(..)
, ActionList
, singleton
, new
, insert
, fromString
, fromList
, toSingleLetter
) where

import Data.List (intercalate)
import Data.Char (toLower)

data Act = ActLeft | ActUp | ActRight | ActDown | ActNone deriving (Eq, Ord)
instance Show Act where
    show ActLeft = "Left"
    show ActUp = "Up"
    show ActRight = "Right"
    show ActDown = "Down"
    show ActNone = "None"

data ActionList n = Leaf Int | Node n Int Bool (ActionList n) (ActionList n) (ActionList n) (ActionList n) deriving (Eq, Ord)
instance (Ord n, Show n) => Show (ActionList n) where
  show (Leaf _) = ""
  show h@(Node n d b h1 h2 h3 h4) = tabs (2 * d) ++ "{\n" ++ tabs (2 * d + 1) ++ showParam "name" (show n) d ++ showParam "depth" (show d) d ++ showParam "finish" (show b) d  ++ "\"children\": [\n" ++ showChildren h ++ "]\n" ++ tabs (2 * d) ++ "}"
{--
    Leaf Int        -> Nothing -> Depth
    Node n          -> n is Act type
    Int             -> Depth
    ActionList n    -> Left, Up, Right, Down
--}

tabs :: Int -> String
tabs 0 = ""
tabs n = '\t' : tabs (n -  1)

showParam :: String -> String -> Int -> String
showParam name "False" _ = ""
showParam name value depth = "\"" ++ name ++ "\": \"" ++ value ++ "\",\n" ++ tabs (2 * depth + 1)

showChildren :: (Ord n, Show n) => ActionList n -> String
showChildren (Leaf _) = ""
showChildren (Node a d b h1 h2 h3 h4) = intercalate (",\n" ++ tabs (2 * d + 1)) $ filter (/= "") [show h1, show h2, show h3, show h4]

singleton :: Act -> Int -> Bool -> ActionList Act
singleton a depth finish = Node a depth finish (Leaf depth) (Leaf depth) (Leaf depth) (Leaf depth)

new :: ActionList Act
new = singleton ActNone 0 False

insert :: ActionList Act -> [Act] -> Bool -> ActionList Act
insert actList [] _ = actList
insert (Node n depth _ left up right down) (act:[]) finish
    | act == ActLeft  && left == (Leaf depth)  = Node n depth finish (singleton act (depth + 1) finish) up right down
    | act == ActUp    && up == (Leaf depth)    = Node n depth finish left (singleton act (depth + 1) finish) right down
    | act == ActRight && right == (Leaf depth) = Node n depth finish left up (singleton act (depth + 1) finish) down
    | act == ActDown  && down == (Leaf depth)  = Node n depth finish left up right (singleton act (depth + 1) finish)
    | act == ActLeft  && finish == True        = Node n depth finish (singleton act (depth + 1) finish) up right down
    | act == ActUp    && finish == True        = Node n depth finish left (singleton act (depth + 1) finish) right down
    | act == ActRight && finish == True        = Node n depth finish left up (singleton act (depth + 1) finish) down
    | act == ActDown  && finish == True        = Node n depth finish left up right (singleton act (depth + 1) finish)
    | otherwise                                = Node n depth finish left up right down
insert (Node n depth _ left up right down) (act:xs) finish
    | act == ActLeft  && left /= (Leaf depth)  = Node n depth finish (insert left xs finish) up right down
    | act == ActUp    && up /= (Leaf depth)    = Node n depth finish left (insert up xs finish) right down
    | act == ActRight && right /= (Leaf depth) = Node n depth finish left up (insert right xs finish) down
    | act == ActDown  && down /= (Leaf depth)  = Node n depth finish left up right (insert down xs finish)
    | act == ActLeft                           = Node n depth finish (insert (singleton act (depth + 1) finish) xs finish) up right down
    | act == ActUp                             = Node n depth finish left (insert (singleton act (depth + 1) finish) xs finish) right down
    | act == ActRight                          = Node n depth finish left up (insert (singleton act (depth + 1) finish) xs finish) down
    | act == ActDown                           = Node n depth finish left up right (insert (singleton act (depth + 1) finish) xs finish)
    | act == ActNone                           = Node n depth finish left up right down
insert (Leaf depth) (act:xs) finish = insert (singleton act (depth + 1) finish) xs finish

fromString :: String -> Act
fromString x
    | str == "left" || str == "actleft" || str == "l" = ActLeft
    | str == "up" || str == "actup" || str == "u" = ActUp
    | str == "right" || str == "actright" || str == "r" = ActRight
    | str == "down" || str == "actdown" || str == "d" = ActDown
    | otherwise = ActNone
    where str = map toLower x

fromList :: [String] -> [Act]
fromList lst = map fromString lst

toSingleLetter :: Act -> Char
toSingleLetter x
    | x == ActLeft  = 'L'
    | x == ActUp    = 'U'
    | x == ActRight = 'R'
    | x == ActDown  = 'D'
    | otherwise     = 'N'