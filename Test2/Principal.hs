
module Principal where

import Domeniu
import Prelude 

-- 5.
main :: IO (Dom Int)
main = do
    ln <- getLine
    let x = read ln :: Dom Int
    return $ optimize (normalize x)