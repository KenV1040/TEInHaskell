import Data.List 
import System.IO

{-
    TODO: Load file
        moveCursorLeftWord
-}
data TextEditor2 = TextEditor2 {
    highlights :: String,
    leftOfCursor :: String,
    rightOfCursor :: String,
    clipboard :: String
} deriving Show

--init has some default text in the left set and the right set
init2 :: TextEditor2
init2 = TextEditor2 {highlights = "", leftOfCursor = "Lorem ipsum", rightOfCursor = " dolor sit", clipboard = ""}

--Creates an empty TextEditor
create :: TextEditor2
create = TextEditor2 {highlights = "", leftOfCursor = "", rightOfCursor = "", clipboard = ""}

inputWord :: [Char] -> TextEditor2 -> TextEditor2
inputWord a (textEditor@TextEditor2{leftOfCursor = loc}) = textEditor{leftOfCursor = loc ++ a}

showLeft :: TextEditor2 -> String
showLeft TextEditor2 {leftOfCursor = loc}= loc

showRight :: TextEditor2 -> String
showRight TextEditor2 {rightOfCursor = roc}= roc

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

moveCursorLeftWord :: TextEditor2 -> TextEditor2
moveCursorLeftWord (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = textEditor{
    leftOfCursor = if length roc == 0
        then loc
        else show(init(words loc)),
    rightOfCursor = if length roc == 0
        then []
        else last(words loc) ++ roc
}

moveCursorStart :: TextEditor2 -> TextEditor2
moveCursorStart (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = textEditor{
    leftOfCursor = "",
    rightOfCursor = loc ++ roc
}

moveCursorEnd :: TextEditor2 -> TextEditor2
moveCursorEnd (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = textEditor {
    leftOfCursor = loc ++ roc,
    rightOfCursor = ""
}

moveCursorLeftAmount :: Int -> TextEditor2 -> TextEditor2
moveCursorLeftAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = moveCursorLeft textEditor
moveCursorLeftAmount x (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = moveCursorLeftAmount (x - 1) (moveCursorLeft textEditor)

moveCursorRightAmount :: Int -> TextEditor2 -> TextEditor2
moveCursorRightAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = moveCursorRight textEditor
moveCursorRightAmount x (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = moveCursorRightAmount (x - 1) (moveCursorRight textEditor)

highlightLeftChar :: TextEditor2 -> TextEditor2
highlightLeftChar (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft $ textEditor {highlights = 
    if length loc == 0
        then h
        else last loc : h
}

highlightRightChar :: TextEditor2 -> TextEditor2
highlightRightChar (textEditor@TextEditor2{rightOfCursor = roc, highlights = h}) = moveCursorRight $ textEditor {highlights = 
    if length roc == 0    
        then h
        else h ++ [head roc]
}

highlightLeftSideCursor :: TextEditor2 -> TextEditor2
highlightLeftSideCursor (textEditor@TextEditor2{leftOfCursor = loc}) = moveCursorStart $ textEditor{highlights = loc}

highlightRightSideCursor :: TextEditor2 -> TextEditor2
highlightRightSideCursor (textEditor@TextEditor2{rightOfCursor = roc}) = moveCursorEnd $ textEditor{highlights = roc}

highlightLeftWord :: TextEditor2 -> TextEditor2
highlightLeftWord (textEditor@TextEditor2{leftOfCursor = loc}) = textEditor{highlights = last(words loc)}

highlightRightWord :: TextEditor2 -> TextEditor2
highlightRightWord (textEditor@TextEditor2{rightOfCursor = roc}) = textEditor{highlights = head(words roc)}

--Recursive highlighting characters
highlightLeftAmount :: Int -> TextEditor2 -> TextEditor2
highlightLeftAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft $ highlightLeftChar textEditor
highlightLeftAmount x (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft (highlightLeftAmount (x - 1) (highlightLeftChar textEditor))

highlightRightAmount :: Int -> TextEditor2 -> TextEditor2
highlightRightAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorRight $ highlightRightChar textEditor
highlightRightAmount x (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorRight (highlightRightAmount (x - 1) (highlightRightChar textEditor))

--Recursive highlighting words
highlightWordLeftAmount :: Int -> TextEditor2 -> TextEditor2
highlightWordLeftAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft $ highlightLeftWord textEditor
highlightWordLeftAmount x (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorLeft (highlightWordLeftAmount (x - 1) (highlightLeftWord textEditor))

highlightWordRightAmount :: Int -> TextEditor2 -> TextEditor2
highlightWordRightAmount 0 (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorRight $ highlightRightWord textEditor
highlightWordRightAmount x (textEditor@TextEditor2{leftOfCursor = loc, highlights = h}) = moveCursorRight (highlightWordRightAmount (x - 1) (highlightRightWord textEditor))
    
highlightClear :: TextEditor2 -> TextEditor2
highlightClear clearing = clearing {highlights = ""}

copy :: TextEditor2 -> TextEditor2
copy (textEditor@TextEditor2{highlights = h}) = textEditor {clipboard = h}

paste :: TextEditor2 -> TextEditor2
paste (textEditor@TextEditor2{clipboard = c, leftOfCursor = loc}) = textEditor{leftOfCursor = loc ++ c, highlights = ""}

--cut (textEditor@TextEditor2{highlights = h, leftOfCursor = loc, rightOfCursor = roc}) = textEditor{highlights = ""}
backspace :: TextEditor2 -> TextEditor2
backspace (textEditor@TextEditor2{leftOfCursor = loc}) = textEditor {leftOfCursor = init loc}

loadFile :: FilePath -> IO() --FilePath is basically just a string
loadFile fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle --Impure function. Can't get IO to String since IO is impure.

saveFile :: String -> TextEditor2 -> IO ()
saveFile fileName (textEditor@TextEditor2{leftOfCursor = loc, rightOfCursor = roc}) = do
    let x = (showLeft textEditor) ++ (showRight textEditor)
    writeFile fileName x
    putStrLn "File saved"

a = highlightRightChar init2
b = highlightRightChar a
c = copy b
d = paste c
