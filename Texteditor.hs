import Data.List 
import System.IO
import Control.Monad
{-
data TextEditor = TextEditor {
    highlights :: String,
    contents :: String,
    clipboard :: String,
    cursor :: Int

} deriving Show

initial = TextEditor {highlights = "", contents = "hello there", clipboard = "", cursor = length (contents initial) - 1}
b = highlightLeftChar initial
showContent :: TextEditor -> String
showContent TextEditor{highlights = h, contents = c, clipboard = cb} =  c

inputWord :: String -> TextEditor -> TextEditor
inputWord userInput file =
    file { contents = contents file ++ " " ++ userInput}

inputWordInInit userInput =
    initial { contents = contents initial ++ " " ++ userInput}


update updatedText file = 
    file { contents = updatedText }


backSpace :: TextEditor -> TextEditor
backSpace a = a { contents = init(showContent a)}

-- as-pattern
moveCursorLeft (textEditor@TextEditor{cursor = cur}) = 
    textEditor { cursor = if cur == 0 then cur else cur - 1 }


moveCursorRight (textEditor@TextEditor{cursor = cur, contents = c}) = 
    textEditor { cursor = 
        if cur == length c 
            then cur 
            else cur + 1 }

highlightLeftChar (textEditor@TextEditor{cursor = cur, highlights = h, contents = con}) = 
    moveCursorLeft (textEditor { highlights = con !! cur : h})


highlightRightChar (textEditor@TextEditor{cursor = cur, highlights = h, contents = con}) = 
    moveCursorRight (textEditor { highlights = con !! cur : h})

copy (textEditor@TextEditor{highlights = h}) = 
    textEditor{clipboard = h}

paste (textEditor@TextEditor{clipboard = cb, contents = con, cursor = cur}) = 
    textEditor{contents = (take cur con) ++ cb ++ (drop cur con), highlights = ""}

loadFile = do  
    putStr "Input file name you want to load: "
    fileName <- getLine --getLine to read what the user input is
    if null fileName --Error handling to stop the user from trying to input nothing.
        then return() 
        else do
            handle <- openFile fileName ReadMode
            contents <- hGetContents handle --hGetContents is lazy, therefore it won't try to load the whole text file into memory. But only loads parts you need
            putStrLn contents           
            hClose handle


-}
data TextEditor2 = TextEditor2 {
    highlights :: String,
    leftOfCursor :: String,
    rightOfCursor :: String,
    clipboard :: String
} deriving Show

init2 = TextEditor2 {highlights = "", leftOfCursor = "Hello ", rightOfCursor = "World", clipboard = ""}
{-
b = moveCursorLeft init2
c = moveCursorLeft b
d = moveCursorLeft c
e = moveCursorLeft d
f = moveCursorLeft e
g = moveCursorLeft f
z = moveCursorLeft g
h = moveCursorRight init2
i = moveCursorRight h
j = moveCursorRight i
k = moveCursorRight j
l = moveCursorRight k
m = moveCursorRight l
-}
inputWord :: [Char] -> TextEditor2 -> TextEditor2
inputWord a (textEditor@TextEditor2{leftOfCursor = loc}) = textEditor{leftOfCursor = loc ++ a}

moveCursorLeft :: TextEditor2 -> TextEditor2
moveCursorLeft (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = textEditor{
    leftOfCursor = if length loc == 0
        then []
        else init loc,
    rightOfCursor = if length loc == 0
        then roc
        else [loc !! (length loc -1 )] ++ roc
}

moveCursorRight :: TextEditor2 -> TextEditor2
moveCursorRight (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = textEditor{
    leftOfCursor = if length roc == 0
        then loc
        else loc ++ [head roc],
    rightOfCursor = if length roc == 0
        then []
        else tail roc
}

highlightLeftChar :: TextEditor2 -> TextEditor2
highlightLeftChar (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft $ textEditor {highlights = last loc : h}

highlightRightChar :: TextEditor2 -> TextEditor2
highlightRightChar (textEditor@TextEditor2{rightOfCursor = roc, highlights = h}) = moveCursorRight $ textEditor {highlights = h ++ [head roc]}

highlightLeftSideCursor :: TextEditor2 -> TextEditor2
highlightLeftSideCursor (textEditor@TextEditor2{leftOfCursor = loc}) = textEditor{highlights = loc}

highlightRightSideCursor :: TextEditor2 -> TextEditor2
highlightRightSideCursor (textEditor@TextEditor2{rightOfCursor = roc}) = textEditor{highlights = roc}

highlightLeftWord :: TextEditor2 -> TextEditor2

highlightClear :: TextEditor2 -> TextEditor2
highlightClear clearing = clearing {highlights = ""}

copy :: TextEditor2 -> TextEditor2
copy (textEditor@TextEditor2{highlights = h}) = textEditor {clipboard = h}

paste :: TextEditor2 -> TextEditor2
paste (textEditor@TextEditor2{clipboard = c, leftOfCursor = loc}) = textEditor{leftOfCursor = loc ++ c, highlights = ""}



a = highlightRightChar init2
b = highlightRightChar a
c = copy b
d = paste c
