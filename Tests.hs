module Tests where

import TableTags

row1 = Element Tr
        [ Element Td
            [Text "hi" ],
          Element Td
            [Text "bye" ]]

row2 = Element Tr
        [ Element Td
            [Text "open" ],
          Element Td
            [Text "shut" ]]

-- tableOk should accept this one
testTable :: TableHtml
testTable =
  Element Table
    [ row1 ,
      row2]

-- the Td has a Table inside it, which is actually allowed.  So this should pass tableOk
okTable :: TableHtml
okTable = Element Table [ Element Tr [ Element Td [ Element Table []]]]
  
okTable2 :: TableHtml
okTable2 = Element Table [ Element Tr [ Element Td [ Text "fun" ]]]
     
-- we will disallow tables with rows of different lengths (though browsers are ok with this).  So tableOk should return False for this one
badTable :: TableHtml
badTable = Element Table [ Element Tr [ Element Td [ Text "hi" ]],
                           Element Tr [ Element Td [ Text "there"] , Element Td [ Text "fun"]]]

anotherBad :: TableHtml
anotherBad = Element Td [ Text "missing table tag at start" ]
