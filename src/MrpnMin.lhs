First cut implementation of:

Ming Chen, [Matrix representation of Petri nets](http://www.techfak.uni-bielefeld.de/~mchen/BioPNML/Intro/MRPN.html)

> module MrpnMin where

> import Data.List (transpose)

> inp = [[0,0,0,0,1],[1,0,0,0,0],[0,1,0,0,0],[0,0,1,1,0]]::[[Integer]]
> outp = [[1,1,0,0,0],[0,0,1,1,0],[0,0,0,1,0],[0,0,0,0,1]]::[[Integer]]
> tvm = [[0,1,1,0],[0,1,0,1],[1,0,0,1],[1,1,1,0]]::[[Integer]]
> mvm  = [[2,1,0,0,0],[1,0,1,2,0],[0,0,1,2,1],[1,1,0,1,1]]::[[Integer]]

> main = print test
> test  = [step inp outp tv mv |tv <- tvm, mv <- mvm]

Given a starting marking [1.1.0,2,1] Fig 1

==>
[[1,0,1,2,0] Fig 2
,[0,-1,2,4,0]
,[-1,-1,2,4,1]
,[0,0,1,3,1]
,[1,1,0,0,1]
,[0,0,1,2,1] Fig 3
,[-1,-,1,2,2]
,[0,1,0,1,2]
,[3,2,-1,-1,0]
,[2,1,0,1,0]
,[1,1,0,1,1] Fig 4
,[2,2,-1,0,1]
,[2,1,1,2,-1]
,[1,0,2,4,-1]
,[0,0,2,4,0]
,[1,1,1,3,0]] Fig 5

> step inp outp transV markV = zipWith (+) markV (multStep inp outp transV)
> multStep inp outp transV = vMultM transV (trStep inp outp)
> vMultM v m = do if length m < 1 || length (head m) < 1 || length v < 1 then []
>                                                                         else (vMultV (head m) v) : vMultM v (tail m)
> vMultV v1 v2 = sum $ zipWith (*) v1 v2
> trStep inp outp  = transpose $ mMinusM outp inp
> mMinusM m1 m2 = if length m1  < 1 || length m2 < 1 then []
>                                                     else (vMinusV (head m1) (head m2)) : mMinusM (tail m1) (tail m2)
> vMinusV v1 v2 = zipWith (-) v1 v2

