module ListSeq where

import Seq

newtype ListSeq a = ListSeq [a]

instance Seq ListSeq where
    emptyS                             = []
    singletonS x                       = [x]
    lengthS (ListSeq s)                = length s
    nthS (ListSeq s) n                 = s !! n
    tabulateS f (ListSeq s)            = ListSeq (map f [0..n-1])
    mapS f (ListSeq s)                 = ListSeq (map f s)
    filterS f (ListSeq s)              = ListSeq (filter f s)
    appendS (ListSeq s1) (ListSeq s2)  = ListSeq (s1 ++ s2)
    takeS (ListSeq s) n                = ListSeq (take n s)
    dropS (ListSeq s) n                = ListSeq (drop n s)
    showtS (emptyS)                    = EMPTY
    showtS (singletonS x)              = ELT x
    showtS (ListSeq s)                 = NODE (ListSeq (take mitad s))
                                              (ListSeq (drop mitad s))
                                              where mitad = (length s)/2
    showlS (emptyS)                    = NIL
    showlS (singletonS x)              = CONS x NIL
    showlS (ListSeq s)                 = CONS (take 1 s) (ListSeq (drop 1 s))

    joinS (emptyS)                     = ListSeq []
    joinS (ListSeq s)                  = ListSeq (concatMap (\(ListSec xs) -> xs) s)
    
    reduceS f b (emptyS)               = b
    reduceS f b (singletonS x)         = x
    reduceS f b (ListSeq s)            = f (reduce f b (take mitad s))
                                           (reduce f b (drop mitad s)) 
                where mitad = (length s) `div` 2
    scanS f b (ListSeq s)              = (ListSeq prefijos, total)
        where
            acum = scanl f b s
            prefijos = init acum
            total = last acum
    fromList s                         = (ListSeq s)
    

