module ArrSeq where

import qualified Arr as A
import Seq
import Par

newtype ArrSeq a = ArrSeq (A.Arr a)

instance Show a => Show (ArrSeq a) where
    show (ArrSeq a) = show a

instance Eq a => Eq (ArrSeq a) where
    (ArrSeq a) == (ArrSeq b) = a == b

instance Seq ArrSeq where
    emptyS                          = ArrSeq A.empty
    singletonS a                    = ArrSeq (A.fromList [a])
    lengthS (ArrSeq a)              = A.length a
    nthS (ArrSeq a) n               = a A.! n
    tabulateS f n                  = ArrSeq (A.tabulate f n)
    mapS f (ArrSeq a)               = ArrSeq (A.tabulate f2 n)
        where
            n  = A.length a
            f2 = \i -> f (a A.! i)
    filterS f (ArrSeq a)    = ArrSeq (A.flatten (A.tabulate f2 n))
        where 
            n = A.length a
            f2 = (\i -> if f (a A.! i) then A.fromList [a A.! i] else A.empty)
    appendS (ArrSeq a) (ArrSeq b)   = ArrSeq (A.flatten (A.fromList [a, b]))
    takeS (ArrSeq a) x              = ArrSeq (A.subArray 0 x a)
    dropS (ArrSeq a) x              = ArrSeq (A.subArray  x ((A.length a)-x) a)
    showtS (ArrSeq a)
        | A.length a == 0 = EMPTY
        | A.length a == 1 = ELT (a A.! 0)
        | otherwise       = NODE (ArrSeq (A.subArray 0 mid a))
                                 (ArrSeq (A.subArray mid (n-mid) a))
        where 
            n = A.length a
            mid = (A.length a)`div`2
    showlS (ArrSeq a)
        | A.length a == 0 = NIL
        | otherwise       = CONS (a A.! 0) (ArrSeq(A.subArray 1 ((A.length a)-1) a))
    joinS (ArrSeq aa)     = ArrSeq (A.flatten(A.tabulate f n))
        where
            n = (A.length aa)
            f = (\i-> let ArrSeq cont = aa A.! i in cont) 
    reduceS f b (ArrSeq a)
        | A.length a == 0    = b
        | A.length a == 1    = (a A.! 0)
        | otherwise         = 
            let (l, r) = (reduceS f b (takeS (ArrSeq a) mid)) ||| (reduceS f b (dropS (ArrSeq a) mid))
            in f l r
        where
            mid = (A.length a)`div`2
    scanS f b (ArrSeq a) 
        | A.length a == 0       = (emptyS, b)
        | A.length a == 1       = (singletonS b, f b (a A.! 0))
        | otherwise             =
            let mid = (A.length a)`div`2
                ((prefL, totL), (prefR, totR)) = 
                    scanS f b (ArrSeq (A.subArray 0 mid a)) |||
                    scanS f b (ArrSeq (A.subArray mid ((A.length a)-mid) a))
                prefR' = mapS (f totL) prefR
            in (appendS prefL prefR', f totL totR)

    fromList xs                 = ArrSeq (A.fromList xs)
    
     
