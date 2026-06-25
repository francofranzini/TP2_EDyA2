module ListSeq where

import Seq
import Par
newtype ListSeq a = ListSeq [a]

instance Show a => Show (ListSeq a) where
    show (ListSeq xs) = show xs

instance Eq a => Eq (ListSeq a) where
    (ListSeq xs) == (ListSeq ys) = xs == ys

instance Seq ListSeq where
    emptyS                             = ListSeq []
    singletonS x                       = ListSeq [x]
    lengthS (ListSeq s)                = length s
    nthS (ListSeq s) n                 = s !! n
    tabulateS f n                      = ListSeq (map f [0..n-1])
    mapS f (ListSeq s)                 = ListSeq (map f s)
    filterS f (ListSeq s)              = ListSeq (filter f s)
    appendS (ListSeq s1) (ListSeq s2)  = ListSeq (s1 ++ s2)
    takeS (ListSeq s) n                = ListSeq (take n s)
    dropS (ListSeq s) n                = ListSeq (drop n s)
    showtS (ListSeq [])                = EMPTY
    showtS (ListSeq [x])               = ELT x
    showtS (ListSeq s)                 = NODE (ListSeq (take mitad s))
                                              (ListSeq (drop mitad s))
                                              where mitad = (length s) `div` 2
    showlS (ListSeq [])                = NIL
    showlS (ListSeq (x:xs))            = CONS x (ListSeq xs)

    joinS (ListSeq [])                 = ListSeq []
    joinS (ListSeq s)                  = ListSeq (concatMap (\(ListSeq xs) -> xs) s)
    
    reduceS f b (ListSeq [])  = b
    reduceS f b (ListSeq [x]) = x
    reduceS f b (ListSeq s)   = reduceS f b (ListSeq (reducePairs s))
        where
            reducePairs []       = []
            reducePairs [x]      = [x]
            reducePairs (x:y:zs) = f x y : reducePairs zs
    
    scanS f b (ListSeq s)              = (ListSeq prefijos, total)
        where
            acum = scanl f b s
            prefijos = init acum
            total = last acum
    fromList s                         = (ListSeq s)
    