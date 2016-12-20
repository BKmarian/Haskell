-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
--import Test.QuickCheck



-- Exercise 9:
aux :: Picture
aux = undefined

pic1 :: Picture
pic1 =  (beside knight (invert knight))`above`
        (beside (invert knight) knight)

pic2 :: Picture
pic2 = (beside knight (invert knight))`above`
       (beside (flipV(invert knight)) (flipV knight))


-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside blackSquare(whiteSquare))

-- b)

otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside whiteSquare(blackSquare))

-- c)

middleBoard :: Picture
middleBoard = repeatV 3 (above emptyRow(otherEmptyRow))

-- d)

rookblack :: Picture
rookblack = over rook blackSquare

bishopblack :: Picture
bishopblack = over bishop blackSquare

kingblack :: Picture
kingblack = over king blackSquare

knightblack :: Picture
knightblack = over knight blackSquare

pawnblack :: Picture
pawnblack = over pawn blackSquare

queenblack :: Picture
queenblack = over queen blackSquare

rookwhite :: Picture
rookwhite = over rook whiteSquare

bishopwhite :: Picture
bishopwhite = over bishop whiteSquare

kingwhite :: Picture
kingwhite = over king whiteSquare

knightwhite :: Picture
knightwhite = over knight whiteSquare

pawnwhite :: Picture
pawnwhite = over pawn whiteSquare

queenwhite :: Picture
queenwhite = over queen whiteSquare

whiteRow :: Picture
whiteRow = beside rookwhite (beside knightblack (beside bishopwhite (beside queenblack (beside kingwhite (beside bishopblack (beside knightwhite rookblack))))))

blackRow :: Picture
blackRow = beside (invert rookblack) (beside knightblack (beside (invert bishopblack) (beside queenblack (beside (invert kingblack) (beside bishopblack (beside (invert knightblack) rookblack))))))


-- e)

populatedBoard :: Picture
populatedBoard = (above whiteRow (above middleBoard blackRow))



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = undefined


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = undefined

fourPictures :: Picture -> Picture
fourPictures x = undefined