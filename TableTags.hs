module TableTags where
-- Tristan Berger
-- hawkid: teberger

{- Represents an HTML tag -}
data TableTag = Table | Tr | Td | Th

{- give a lower case name of the tag, e.g., Td is "td" -}
showTableTag :: TableTag -> String
showTableTag  Table = "table"
showTableTag Tr = "tr"
showTableTag Td = "td"
showTableTag Th = "th"

equalTableTags :: TableTag -> TableTag -> Bool
equalTableTags Table Table = True
equalTableTags Tr Tr = True
equalTableTags Td Td = True
equalTableTags Th Th = True
equalTableTags _ _ = False


{- no need to modify these two instances; complete the
above functions instead -}
instance Show TableTag where
  show = showTableTag

instance Eq TableTag where
  (==) = equalTableTags

{-
Takes two tags: returns True iff the second tag is allowed to be nested directly inside of the first
-}
canBeDirectElement :: TableTag -> TableTag -> Bool
canBeDirectElement Table Tr = True
canBeDirectElement Tr Td = True
canBeDirectElement Tr Th = True
canBeDirectElement Td Table = True
canBeDirectElement Th Table = True
canBeDirectElement _ _ = False


{- TableHtml

   The object

     Element tag [ sub element 0 , ... , sub element n-1 ]

   represents an HTML element with the given tag and the given sub elements.

There are two base cases for TableHtml
   n = 0
   Text s

   Text s represents unstructured textual data s 
   See Tests for example TableHtml objects -}
data TableHtml = Element TableTag [TableHtml] | Text String

toJoin :: [String] -> String
toJoin (first:rest) = first ++ (toJoin rest)
toJoin [] = ""

showTable :: TableHtml -> String
showTable (Text st)= st
showTable (Element tt []) = "<" ++ (show tt) ++ "></" ++ (show tt) ++ ">"
showTable (Element tt rest) = "<" ++ (show tt) ++ ">" ++ inner ++ "</" ++ (show tt) ++ ">" where inner = toJoin [(showTable first) | first <- rest]


{- no need to modify this instance; complete the above function instead -}
instance Show TableHtml where
  show = showTable 

{- returns true iff the TableHtml is "okay". To be okay means that
* all rows have the same length
* follows the rules about what can be directly nested in what

Simplification: technically a Td and Th can contain a Table. But you only
need to deal with one level of Table. If a Td or Th contains a table, you don't need to check it
-}
tableOk :: TableHtml -> Bool
tableOk (Element Table li) = tableHelper((Element Table li))
tableOk _ = False

tableHelper :: TableHtml -> Bool
tableHelper (Element tt []) = True
tableHelper (Text _) = True
tableHelper (Element Table li) = (tableHelper2 (Element Table li)) && (tHelp [length(ts) | (Element _ ts) <- li])
tableHelper (Element tt li) = tableHelper2 (Element tt li)

tableHelper2 :: TableHtml -> Bool
tableHelper2 (Element tt li) = and ([(tableHelper (Element tt' li')) && canBeDirectElement tt tt' | (Element tt' li') <- li])

tHelp :: [Int] -> Bool
tHelp (first:rest) = and (map (\x -> x == first) (first:rest))
tHelp [] = True
{-
Given a Table, row index, and column index, return that cell
-}
getCell :: TableHtml -> Int -> Int -> TableHtml
getCell (Element Table li) x y =  getCell(li !! x) x y
getCell (Element Tr li) x y = li !! y
getCell empty _ _ = empty

