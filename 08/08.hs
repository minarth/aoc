module Main where
input = [2,2,1,2,2,2,2,1,0,2,0,2,2,2,2,0,2,2,2,2,2,1,2,2,2,2,2,2,2,2,1,0,2,2,2,2,0,2,1,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,0,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,0,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,0,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,1,2,1,0,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,1,2,2,2,0,2,2,0,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,0,2,2,2,2,1,2,1,0,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,1,2,0,2,2,2,2,2,0,2,2,2,1,2,2,2,2,1,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,0,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,2,2,0,2,0,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,0,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,1,0,2,2,2,2,2,1,0,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,0,1,2,0,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,2,2,1,2,0,0,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,2,0,2,2,2,2,0,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,0,0,2,2,2,2,2,0,0,2,2,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,0,1,2,0,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,1,1,2,2,2,2,1,2,0,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,2,2,2,2,2,1,2,2,2,2,2,2,1,2,1,2,2,2,2,2,0,2,2,2,2,1,1,2,0,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,2,2,0,2,0,1,2,2,0,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,0,2,0,2,2,1,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,0,2,2,1,2,2,2,2,2,2,2,2,1,0,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,0,2,1,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,1,2,2,2,2,0,2,0,2,2,1,1,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,1,2,2,2,2,2,2,1,2,2,2,0,2,0,2,2,2,2,2,1,2,2,2,2,2,1,2,2,1,2,2,2,0,2,2,0,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,0,0,2,2,2,2,2,0,0,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,0,0,2,2,2,1,2,2,2,2,2,2,2,0,2,2,2,2,2,2,1,0,2,2,2,2,2,2,1,0,2,1,0,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,0,2,2,2,2,2,1,2,2,2,1,2,1,2,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,2,2,2,0,2,2,2,0,2,2,1,2,0,2,2,2,2,2,2,2,2,1,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,2,2,2,2,1,1,2,1,1,2,2,2,2,0,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,0,2,2,1,1,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,1,2,2,2,2,2,1,0,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,0,2,2,1,2,2,2,1,2,2,1,2,2,2,2,2,0,2,0,2,2,2,2,2,2,2,0,2,2,2,2,1,2,0,0,2,0,0,2,2,2,2,0,2,2,2,2,2,1,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,0,2,0,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,0,2,2,1,2,2,1,2,0,2,2,2,2,2,2,2,2,1,2,2,1,2,0,0,0,2,2,1,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,1,2,2,2,2,0,2,1,0,2,1,1,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,1,2,1,2,2,1,2,2,2,2,2,2,1,1,2,0,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,2,2,2,1,0,2,2,2,2,2,1,2,0,2,2,2,2,2,0,2,2,2,2,1,2,2,1,1,2,2,2,2,2,2,2,2,2,0,2,0,2,2,2,2,2,2,0,1,0,2,2,2,1,2,0,0,2,0,1,2,2,2,2,1,2,2,2,2,2,1,2,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,0,1,2,2,2,2,2,2,2,2,1,2,2,0,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,0,2,2,2,0,2,2,1,2,0,2,2,2,2,2,1,2,2,2,2,2,1,2,0,1,1,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,0,2,2,2,1,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,2,0,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,2,0,2,2,2,0,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,2,1,2,1,1,2,2,2,2,2,1,1,2,2,0,2,2,0,2,0,2,2,2,2,2,1,2,2,2,2,1,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,1,2,2,2,0,2,1,0,2,2,0,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,0,2,2,1,2,2,2,2,2,2,1,2,2,2,0,2,0,2,2,2,2,2,0,2,2,2,1,2,0,2,2,0,2,2,2,1,2,2,1,1,2,1,1,2,2,2,2,2,2,1,2,2,0,2,2,1,2,2,2,2,2,1,2,2,2,1,2,2,0,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,0,1,2,2,2,0,2,1,2,2,1,0,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,1,2,0,2,2,2,2,2,2,2,2,2,1,2,1,0,2,2,2,2,2,2,2,2,2,1,2,0,1,2,2,2,2,2,2,1,2,2,2,2,0,1,2,2,2,2,2,2,0,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,0,1,0,2,2,0,2,2,2,2,2,1,2,1,2,2,2,2,2,2,2,1,2,2,2,2,1,2,2,1,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,1,2,2,2,2,2,2,2,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,0,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,2,0,2,2,0,2,1,2,1,2,2,2,2,0,0,2,2,1,2,2,2,2,2,2,2,2,2,2,0,2,2,1,2,1,2,2,0,0,1,2,2,1,1,2,2,2,2,1,2,2,2,2,2,2,2,2,1,1,1,2,2,2,0,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,0,1,2,2,2,2,2,1,2,2,0,0,2,1,0,2,2,2,2,2,2,1,2,2,1,2,2,1,1,2,2,2,2,0,0,2,2,0,2,2,2,2,0,2,2,2,2,2,1,2,2,2,2,0,0,2,0,2,1,2,2,1,0,2,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,1,2,0,0,2,0,0,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,0,2,2,2,1,2,2,0,2,2,0,0,2,2,2,2,2,2,1,2,2,0,2,2,0,1,2,2,2,2,0,1,2,2,1,2,2,1,2,1,2,2,2,2,2,1,2,2,1,2,1,2,2,0,0,1,2,2,0,0,2,2,0,2,1,2,1,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,0,2,1,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,1,1,2,0,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,0,1,0,2,2,2,2,0,0,2,2,1,2,2,0,2,1,2,2,0,2,2,1,2,2,0,2,1,1,2,2,2,0,2,2,0,2,2,2,1,2,0,2,0,2,2,2,2,2,2,0,0,0,2,2,2,2,2,1,2,2,2,0,2,2,2,2,1,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,0,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2,2,0,2,2,2,2,2,0,1,2,1,2,2,2,0,2,2,2,1,2,0,1,2,2,2,0,2,2,0,2,2,0,2,1,0,2,2,2,2,2,2,1,2,2,0,2,2,0,2,1,2,2,1,2,2,1,2,2,2,2,0,0,2,2,1,1,2,2,1,2,2,2,1,2,0,2,1,2,2,2,2,2,0,0,0,1,2,2,2,2,2,0,0,2,1,2,2,2,2,2,0,2,2,2,2,2,1,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,1,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,0,2,1,2,2,2,2,2,0,2,2,2,2,2,1,2,2,0,2,2,2,0,2,2,0,0,2,2,0,2,2,2,1,2,2,0,2,2,2,2,1,1,2,2,2,2,2,2,0,0,2,0,2,2,1,2,0,2,2,2,2,2,1,2,2,2,2,2,0,2,0,2,1,2,2,0,2,2,2,0,2,0,2,1,2,2,2,2,2,0,0,1,1,2,2,2,0,2,1,0,2,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,0,2,1,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,1,2,2,2,2,2,2,2,2,2,1,2,1,0,2,1,2,2,2,0,2,2,0,1,2,2,0,2,2,2,2,2,2,2,2,2,1,2,0,1,1,2,2,2,2,0,0,1,2,0,2,2,2,2,0,2,2,0,2,2,1,2,2,1,2,1,1,2,0,2,0,2,2,1,1,2,2,2,2,0,2,0,2,2,2,2,2,1,2,0,1,2,2,2,1,2,0,1,2,1,0,2,2,1,2,1,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,0,2,1,1,2,1,2,2,2,2,2,2,2,1,2,1,2,2,2,2,0,1,2,2,2,2,0,2,2,0,0,2,2,2,2,2,1,2,2,1,2,2,2,2,0,2,0,0,2,2,0,2,2,1,2,1,1,2,1,1,0,2,2,0,0,2,2,0,2,2,2,0,2,2,2,2,2,0,2,1,2,2,2,2,1,2,0,1,2,2,2,2,0,1,2,0,2,2,2,2,2,0,2,2,1,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,0,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,0,2,2,1,2,2,1,2,2,2,2,0,2,2,2,2,2,0,2,0,0,2,2,2,2,2,1,2,1,2,0,2,2,1,2,1,2,2,0,2,2,1,2,2,1,2,0,1,2,2,2,1,2,2,0,1,2,2,1,2,1,2,0,2,2,2,2,1,2,0,0,2,2,2,2,0,2,0,1,2,1,1,2,1,0,2,1,2,2,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,2,1,2,2,2,0,2,1,2,2,2,2,2,1,2,2,2,0,2,1,2,2,0,2,2,2,0,2,2,0,0,2,1,0,2,2,2,0,2,2,1,2,2,2,2,2,0,0,2,2,2,2,2,2,1,2,1,2,2,2,2,1,2,0,0,2,2,0,2,2,1,2,1,0,2,2,1,1,2,2,1,0,2,2,1,2,0,2,0,2,2,2,2,1,0,2,2,0,2,2,2,0,2,0,1,2,1,0,2,0,1,2,2,2,2,2,2,2,0,2,2,0,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,1,2,0,0,2,0,2,2,2,2,2,2,0,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,2,2,0,0,2,2,2,2,2,1,2,2,1,1,2,0,1,2,2,2,0,2,2,2,2,2,1,2,0,2,0,2,2,2,2,1,1,0,2,2,2,2,2,2,0,2,2,2,2,2,1,2,2,0,2,1,1,2,1,1,0,2,2,2,2,2,2,1,2,1,2,1,2,2,2,2,0,2,1,1,0,2,2,2,1,2,2,1,2,0,2,2,2,2,2,2,2,2,2,2,2,0,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,0,0,1,2,2,0,2,2,2,2,2,2,0,2,2,2,2,2,1,2,2,2,2,2,0,2,2,2,0,2,1,0,2,1,2,2,2,1,2,2,1,0,2,0,1,2,2,2,0,0,2,2,2,2,0,2,0,0,1,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,0,2,2,0,2,2,2,2,0,1,2,0,1,1,2,2,2,0,2,2,1,2,1,2,2,2,2,2,2,0,2,1,0,1,2,2,2,2,2,1,0,2,2,1,2,2,1,2,1,2,2,2,2,2,1,2,2,1,2,2,2,1,2,2,1,2,2,0,2,2,2,2,2,1,2,0,0,0,0,2,2,2,2,2,2,2,0,0,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,0,2,0,0,2,0,2,2,2,1,2,1,0,1,2,1,2,2,2,2,1,1,2,0,2,2,0,2,1,2,1,2,2,2,2,1,1,0,2,1,2,2,1,2,1,2,0,2,2,2,0,2,2,1,2,1,1,2,2,2,0,2,2,0,0,2,2,1,2,1,2,1,2,2,2,2,0,0,2,1,1,2,2,2,0,2,0,1,2,1,1,2,1,2,2,1,2,2,2,2,2,0,2,2,1,2,2,2,1,2,2,0,2,2,2,2,2,2,2,2,1,2,1,2,1,0,2,0,2,2,2,2,2,0,0,2,2,2,0,2,2,2,2,2,2,2,0,2,2,2,2,2,1,0,2,1,2,2,2,0,2,1,1,1,2,2,0,2,2,2,2,0,2,1,2,2,2,2,0,2,2,2,2,2,2,0,1,1,2,0,2,2,2,2,1,2,2,1,2,2,0,2,2,1,2,2,1,2,0,2,0,2,2,2,2,2,2,2,2,1,2,1,2,0,2,2,0,1,2,2,2,2,2,2,0,2,2,2,2,1,1,2,1,0,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,1,2,0,1,2,2,2,1,2,2,2,2,2,2,1,2,2,2,1,2,0,2,2,2,2,2,1,2,2,2,2,2,2,1,2,1,2,2,2,2,2,1,2,0,2,0,0,2,2,2,2,2,2,0,2,2,0,2,2,2,0,2,2,2,2,1,0,2,2,0,2,2,2,2,1,2,1,0,2,2,0,2,2,0,2,2,1,2,0,2,0,2,2,1,2,2,2,1,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,2,1,2,1,0,2,2,0,2,1,1,2,0,2,2,2,2,2,1,2,2,2,2,2,2,0,2,2,0,2,2,1,0,2,2,2,2,1,0,2,0,2,2,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,1,0,2,2,2,2,2,2,2,1,0,0,2,1,2,2,2,2,1,2,2,0,2,2,2,2,2,2,2,2,2,2,2,1,1,1,2,0,2,2,1,2,0,2,1,2,2,2,2,2,2,2,2,0,2,2,2,0,0,2,2,1,2,2,2,1,2,2,2,0,2,1,2,2,0,2,0,2,0,2,2,2,2,2,2,1,2,2,0,2,2,1,2,1,2,2,2,2,2,2,2,2,1,2,2,2,1,2,2,0,2,2,0,2,2,2,2,2,2,0,1,2,1,1,2,2,2,2,2,2,2,1,0,2,2,2,0,2,0,2,2,2,2,2,2,2,2,2,2,2,1,0,2,2,2,2,2,1,2,0,1,1,2,2,2,2,1,2,1,2,2,2,2,2,0,2,0,2,1,2,2,2,2,2,0,1,2,1,2,1,1,2,2,2,2,2,2,2,1,2,2,1,2,1,2,2,1,2,2,2,2,0,1,2,2,2,2,2,2,2,2,2,2,2,0,2,1,1,2,2,2,2,2,2,1,1,2,1,0,2,1,0,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,0,2,2,1,1,2,1,2,2,2,2,2,0,1,2,2,2,1,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,0,1,2,1,2,0,2,2,0,2,2,1,2,0,2,1,2,2,2,2,2,1,2,2,1,0,2,0,1,2,2,1,0,2,2,2,2,2,1,2,0,0,2,2,1,2,2,2,0,1,2,2,0,2,2,2,0,2,1,2,2,2,0,0,1,2,2,2,2,0,2,0,2,2,1,0,2,1,2,2,0,2,2,2,2,2,1,2,2,0,2,2,2,2,2,2,1,2,2,0,0,2,2,2,2,1,2,1,0,2,1,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,0,2,1,2,2,0,2,2,2,2,2,2,2,0,2,2,0,2,2,2,1,0,2,2,2,2,2,2,1,1,1,2,2,2,2,2,2,1,2,0,0,1,2,0,0,2,1,0,2,2,0,2,2,2,2,2,1,2,1,1,2,2,2,1,1,2,2,2,2,2,2,0,2,2,2,2,1,2,1,0,0,2,2,2,1,2,0,1,2,0,2,2,1,2,2,2,2,2,0,2,2,0,2,2,0,2,2,2,0,2,2,0,2,2,2,2,2,2,2,2,1,0,1,1,2,1,2,0,2,2,2,2,2,2,1,2,2,2,0,2,0,2,2,2,2,2,1,2,2,2,1,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,2,2,0,2,2,0,2,0,2,2,2,2,1,0,1,2,2,2,2,1,0,0,2,1,0,2,2,0,0,2,0,1,2,2,2,2,2,0,2,2,0,2,2,0,0,2,2,0,0,2,2,0,2,0,2,1,2,2,2,2,0,0,1,1,1,2,2,2,0,2,1,1,2,1,2,2,0,1,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,0,0,2,2,2,2,1,0,0,0,2,2,2,1,2,2,2,2,2,1,0,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,0,2,2,2,2,0,2,2,2,2,2,0,1,1,2,1,1,2,0,2,1,0,2,1,2,2,1,2,0,2,1,2,2,2,2,0,1,2,2,2,0,1,2,0,2,2,0,1,2,2,2,2,2,0,2,0,0,2,2,2,1,2,2,2,2,2,2,1,2,0,2,0,2,0,2,2,1,1,1,1,0,2,2,2,1,2,2,2,2,1,0,2,0,2,2,1,2,2,0,2,2,0,2,2,2,2,2,2,2,2,2,0,2,2,1,0,2,2,2,2,1,2,2,2,0,2,2,1,2,2,2,2,2,2,2,2,2,2,0,2,1,2,2,2,2,2,0,2,2,2,1,2,2,2,2,1,2,2,2,2,2,2,0,1,2,2,0,2,1,2,2,1,2,2,2,2,2,2,1,2,0,2,2,2,2,0,1,1,2,1,1,2,1,0,2,2,0,1,2,2,1,2,2,0,2,1,2,2,1,2,0,2,2,2,2,2,2,1,2,1,2,1,2,0,2,2,2,0,0,2,2,2,2,2,0,2,2,0,2,2,1,2,0,0,2,2,2,2,1,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,1,2,0,0,2,0,2,1,2,2,2,2,2,2,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,0,2,2,2,2,1,2,1,0,2,2,2,1,2,0,2,0,2,2,1,2,2,0,2,2,0,2,2,2,2,2,1,1,1,2,2,1,2,2,2,2,2,2,0,2,2,2,2,2,0,2,1,2,2,0,0,1,1,2,0,0,1,2,2,2,1,1,0,2,0,2,2,1,0,0,1,1,2,2,2,0,2,0,0,2,2,2,2,2,0,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,2,2,2,1,0,2,0,2,0,2,0,2,2,2,2,2,1,0,2,2,2,2,2,1,2,2,2,2,2,0,2,2,2,2,2,2,0,0,0,2,2,2,0,2,2,2,1,2,1,0,2,2,2,0,0,2,2,2,2,0,2,1,0,2,2,2,2,2,2,0,1,2,0,1,2,2,2,1,2,2,2,2,2,2,2,2,0,2,1,0,2,0,0,1,1,2,1,1,1,2,0,2,0,2,2,2,0,2,2,1,0,1,0,2,2,2,2,1,2,1,2,2,1,0,2,0,0,2,0,2,2,1,2,2,0,2,2,2,2,2,2,0,2,2,1,2,2,1,0,2,2,2,2,0,0,0,2,1,2,2,1,2,2,2,2,2,1,0,2,2,2,1,2,1,2,2,2,2,2,1,2,2,2,1,2,2,1,2,0,2,2,2,2,2,0,0,2,2,2,1,2,1,2,0,0,2,1,2,2,0,2,0,1,0,2,2,2,2,2,1,0,2,2,1,2,2,1,2,2,2,0,2,2,0,2,2,0,2,2,1,2,2,2,0,2,2,2,2,1,2,1,2,1,1,2,2,2,2,2,2,0,0,1,1,2,2,2,1,2,2,0,2,2,1,2,0,2,2,0,2,2,1,2,2,2,2,1,0,2,2,2,1,2,2,0,2,2,2,1,2,2,2,2,0,1,0,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,0,2,2,2,2,2,0,2,2,2,2,2,0,2,1,2,2,2,2,0,2,1,1,2,2,1,1,2,1,2,0,1,2,0,2,2,0,2,2,0,2,2,2,2,2,0,2,1,2,1,1,0,2,1,2,2,1,0,2,2,2,2,2,2,2,2,0,2,1,0,1,1,2,0,1,1,2,2,2,1,1,2,2,1,2,2,0,0,0,0,2,2,2,2,2,2,2,0,2,0,0,2,2,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,2,0,1,2,2,2,2,0,1,0,1,2,2,2,0,2,2,2,2,2,0,1,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,1,2,1,1,0,0,2,2,2,0,2,1,0,0,2,1,1,2,2,2,0,2,2,2,2,2,0,2,1,0,0,2,2,2,2,0,1,1,2,0,1,0,1,2,1,2,0,1,2,2,2,2,2,2,2,1,1,2,2,0,0,0,2,1,1,1,2,2,2,2,1,0,2,1,2,2,0,2,1,1,2,2,2,2,2,2,0,1,2,1,1,2,1,0,2,0,2,2,0,2,2,0,2,0,2,2,2,2,1,2,2,0,2,2,2,1,2,2,2,2,1,0,2,0,0,2,2,1,2,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2,2,0,0,2,2,2,1,2,1,0,1,0,2,2,2,1,2,0,1,0,2,2,0,2,0,2,2,2,2,1,2,2,1,2,2,2,2,2,2,2,2,2,1,1,2,1,2,1,0,0,2,2,0,0,2,2,2,2,2,1,2,2,1,2,2,0,2,2,2,0,0,0,2,2,2,0,2,0,2,1,2,2,2,1,2,1,1,2,2,2,0,2,0,0,2,2,0,2,0,0,2,1,2,2,2,2,2,0,2,2,1,2,2,2,1,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,1,1,2,0,2,2,2,2,2,0,0,2,2,2,0,2,0,2,2,2,2,0,0,2,2,2,2,2,1,1,1,0,2,2,2,0,2,0,1,0,2,0,0,2,0,2,2,0,2,2,2,2,2,2,1,0,2,2,2,2,2,2,2,2,2,1,1,1,0,0,2,2,2,0,2,2,0,2,2,2,2,2,2,2,1,2,1,2,2,0,0,1,2,2,2,1,0,1,2,0,2,2,1,1,2,0,2,2,2,2,2,2,0,1,2,0,1,2,1,2,2,1,2,2,1,2,2,1,2,1,0,2,2,2,0,2,2,1,2,2,2,0,2,2,2,2,0,1,0,2,1,2,2,1,2,2,2,2,2,2,2,2,2,2,0,2,0,2,2,2,2,1,2,2,2,2,0,2,0,0,1,1,2,2,2,2,2,2,0,2,2,1,0,2,2,2,2,0,2,1,2,2,2,2,0,2,2,2,2,2,2,2,2,1,2,2,2,1,0,2,2,2,1,0,2,2,2,2,2,2,2,1,0,2,2,2,0,2,2,2,2,2,2,2,2,2,0,0,2,2,2,2,2,0,0,0,0,2,2,2,0,2,1,2,2,0,1,2,0,0,2,0,2,2,2,2,2,2,2,2,1,2,2,2,0,2,2,2,2,1,0,1,2,2,2,2,2,0,2,1,0,2,2,0,2,2,2,2,2,0,2,2,2,2,1,2,0,2,2,2,2,1,2,2,2,2,1,2,1,1,1,1,2,2,2,1,2,0,0,2,2,1,2,2,1,2,0,0,2,0,2,2,1,2,0,2,0,2,2,2,2,2,1,2,2,0,0,1,1,0,2,2,2,0,2,2,2,2,2,0,2,2,1,2,0,2,1,2,2,2,2,1,2,0,2,1,2,2,2,2,2,2,0,1,2,0,2,2,2,2,1,2,2,2,2,2,1,2,2,0,2,1,2,2,2,2,2,0,2,2,1,2,2,2,2,2,2,0,2,2,0,0,2,2,2,2,0,1,0,2,1,1,2,0,2,2,2,2,2,2,2,2,2,2,0,2,1,2,2,2,2,1,2,2,2,2,0,2,1,1,2,2,2,2,2,2,2,1,2,0,2,2,0,2,2,2,2,1,2,0,2,2,0,2,1,0,1,2,2,2,2,0,2,0,1,0,0,2,0,1,1,2,2,2,2,2,0,2,2,1,2,1,2,2,0,2,2,2,2,1,2,0,2,0,2,1,1,2,2,0,2,2,1,1,0,0,0,2,2,2,2,2,1,1,2,0,1,2,1,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,1,2,1,0,0,2,2,2,2,1,0,0,2,1,0,2,0,2,2,2,2,2,1,2,2,2,2,0,2,0,2,2,2,2,2,2,2,2,2,2,2,0,0,1,0,2,2,2,2,2,2,0,2,2,0,2,2,0,2,2,0,2,1,2,2,1,2,2,2,2,2,2,2,2,0,1,2,2,0,2,0,1,0,1,2,2,2,2,2,0,2,2,0,2,2,0,2,1,0,2,2,2,1,2,0,2,1,2,1,2,1,2,2,2,2,1,0,2,1,0,2,2,2,2,2,2,0,2,2,0,2,0,2,2,0,2,2,1,2,2,1,2,0,2,2,2,2,0,2,2,0,2,0,1,0,2,2,2,2,2,2,1,0,0,2,2,2,2,2,2,2,2,1,0,2,2,2,2,2,0,2,2,2,2,0,0,2,2,2,2,2,0,2,2,0,2,2,2,0,2,2,2,2,2,0,1,2,0,2,1,2,2,0,2,2,0,2,1,2,2,2,2,2,2,1,0,2,1,2,0,0,2,1,0,2,1,2,2,2,2,2,2,1,2,0,1,2,0,1,2,0,2,2,2,0,2,1,2,0,2,0,2,0,2,2,1,2,2,2,1,2,2,2,2,2,1,2,2,2,1,2,0,2,2,1,2,2,2,2,2,0,2,2,1,2,2,2,1,0,2,1,2,0,2,2,2,2,2,2,2,2,1,1,1,0,2,2,2,2,2,2,2,0,2,2,2,2,1,2,1,2,2,2,2,1,2,2,2,2,1,2,1,1,0,0,2,2,2,2,2,1,1,2,2,1,1,2,0,2,2,1,2,2,2,2,2,2,2,1,2,2,2,2,2,0,1,1,1,1,2,1,0,1,0,2,1,0,2,2,1,2,2,0,2,1,2,2,2,1,2,1,2,1,1,2,2,2,2,1,2,1,2,1,0,2,1,0,1,1,2,2,2,0,2,2,0,1,2,1,0,2,2,0,2,2,2,2,2,2,2,2,2,1,1,2,2,2,0,2,2,2,2,1,0,1,2,2,2,2,1,2,0,1,0,2,2,1,2,2,2,2,2,0,0,2,2,2,0,1,0,2,2,2,2,0,1,2,2,2,1,2,0,0,1,2,2,2,2,2,2,0,0,0,2,1,2,2,2,2,1,2,2,1,2,2,0,2,0,2,1,2,2,2,2,2,0,0,0,2,2,0,0,1,1,2,1,0,2,2,2,2,2,0,2,0,0,2,2,2,2,0,2,1,0,0,2,0,2,0,2,0,2,1,1,2,0,2,2,2,1,2,2,1,0,2,2,2,2,2,0,2,0,2,2,1,2,2,1,2,2,0,2,0,1,2,2,2,1,0,2,0,2,1,0,2,2,2,2,2,0,0,2,1,1,1,2,0,2,2,2,2,2,0,1,2,2,2,2,2,1,2,2,2,2,2,1,2,2,2,2,2,1,1,0,2,2,2,2,1,2,2,1,0,2,1,1,2,2,2,0,0,2,1,2,2,0,2,0,2,1,2,2,2,2,1,2,2,2,2,2,0,1,0,1,2,1,1,2,2,0,2,2,2,2,0,1,2,2,0,0,1,2,1,2,1,2,0,2,0,2,2,2,0,0,2,2,0,2,1,0,2,2,1,2,2,2,0,2,1,1,2,0,0,2,1,2,2,0,2,2,1,2,2,2,2,2,2,0,2,2,0,2,0,1,2,2,2,2,2,0,0,0,1,0,2,2,1,2,2,2,2,2,2,1,2,1,2,0,2,1,2,2,2,2,1,0,2,2,2,1,2,2,2,0,1,2,2,2,2,2,0,1,1,2,0,2,2,2,2,0,1,2,1,2,2,1,2,1,1,0,2,2,2,2,0,1,1,2,1,2,2,1,0,0,2,2,2,2,2,0,2,2,1,2,2,1,2,1,1,2,0,2,0,2,1,2,2,2,1,2,2,2,1,0,2,2,2,1,2,0,2,2,1,1,2,2,0,2,1,1,2,1,0,2,2,2,2,2,2,2,2,2,0,2,2,2,2,1,1,2,2,2,0,1,2,2,2,2,2,0,2,0,2,2,2,2,0,2,2,2,2,2,0,2,2,1,2,1,2,2,2,2,2,2,0,0,2,2,2,1,2,0,0,1,2,2,2,2,2,2,2,2,1,2,0,2,2,1,2,1,1,2,2,2,2,0,2,2,1,0,2,2,2,2,2,1,0,0,1,0,2,2,0,2,2,1,0,2,2,1,2,2,0,2,0,0,2,0,1,2,0,2,0,0,1,2,1,2,0,1,1,2,0,1,2,0,1,1,0,2,2,2,2,2,2,2,2,2,2,1,2,1,0,2,0,2,0,2,2,2,1,2,0,1,2,2,2,0,0,2,2,2,2,1,0,2,2,2,2,1,2,0,0,0,0,2,2,2,2,2,2,2,2,1,2,0,2,0,2,0,2,2,2,2,1,2,2,2,2,0,2,0,2,2,1,2,2,2,1,2,2,1,0,2,2,0,2,0,2,2,2,2,0,2,2,2,2,1,0,2,2,2,2,2,0,1,2,1,0,0,0,2,1,1,2,1,2,2,2,2,2,2,2,2,2,0,2,1,1,0,0,2,0,2,2,2,2,2,1,0,1,2,0,0,2,2,2,0,0,0,2,2,2,2,2,1,1,2,1,0,2,2,1,2,0,2,1,2,2,2,2,2,1,0,2,2,2,1,0,2,1,2,1,1,0,2,2,2,2,2,1,0,2,2,0,2,2,2,2,2,2,2,0,0,2,0,2,0,0,1,2,2,2,2,2,1,2,2,2,2,2,1,0,2,2,2,2,2,1,2,2,1,0,0,1,2,2,0,2,0,2,2,1,2,2,0,0,2,0,0,2,2,2,2,2,1,2,0,2,2,2,0,2,2,2,1,0,2,2,2,2,2,0,2,1,2,2,1,1,1,2,2,2,2,1,2,0,2,0,1,1,2,1,1,2,2,2,0,1,0,2,2,2,0,2,2,0,2,2,1,2,0,0,2,1,2,0,2,2,2,0,2,0,2,2,2,2,1,2,2,0,2,2,0,2,2,2,2,2,1,2,1,1,2,0,2,2,2,2,2,2,2,1,1,2,2,2,2,0,1,2,2,2,2,1,1,2,1,2,0,2,2,0,0,0,2,2,2,0,2,2,2,0,0,0,0,2,0,2,2,0,2,2,2,2,2,2,2,2,1,2,2,2,2,1,1,1,0,0,0,1,0,2,2,2,0,1,2,2,1,2,2,1,2,1,2,2,2,1,1,0,2,2,2,1,2,2,2,0,2,1,2,0,1,2,1,0,0,0,2,1,2,2,0,2,1,2,2,2,2,2,0,0,2,2,2,2,2,2,2,0,2,1,0,2,2,2,0,1,2,1,2,1,1,2,2,2,2,2,2,0,1,0,1,1,2,2,2,2,2,2,2,1,1,2,2,2,0,1,1,2,2,2,2,1,0,2,2,2,2,2,1,0,1,2,2,2,2,2,2,1,0,2,0,2,2,2,1,2,1,0,2,0,2,2,0,1,2,0,2,2,2,2,2,1,2,1,0,1,2,0,1,2,1,2,1,1,2,2,1,2,2,0,2,0,2,2,1,0,0,0,2,1,1,2,2,0,2,1,1,1,2,2,0,2,0,2,0,0,0,2,2,1,2,2,1,0,2,2,0,2,2,2,2,0,2,0,0,2,2,0,2,1,1,2,0,2,1,2,2,2,2,0,0,0,2,2,2,2,2,0,1,0,1,2,2,1,2,2,2,2,2,2,1,2,1,2,1,0,2,2,2,2,2,0,2,2,2,2,2,2,0,1,0,2,2,2,2,1,2,0,0,1,1,1,1,2,2,2,2,1,2,1,2,2,1,1,1,1,2,2,2,2,2,1,2,1,1,0,1,2,2,2,2,2,1,0,1,2,1,2,2,0,2,2,0,2,0,1,1,2,2,1,1,2,2,1,2,2,0,0,2,1,0,2,0,1,2,1,2,1,2,1,0,2,0,0,2,2,0,2,2,2,2,2,2,2,2,2,2,2,2,0,0,2,0,2,2,0,2,2,2,2,0,2,2,0,2,2,2,0,1,0,1,1,2,2,2,2,2,2,2,1,1,2,1,2,2,1,2,2,2,1,2,2,1,2,0,2,0,2,1,0,1,0,2,2,2,1,2,2,1,2,0,2,0,2,0,2,1,1,2,2,2,2,1,1,0,0,1,2,2,2,2,1,1,2,0,0,2,2,1,0,1,2,0,1,0,2,2,2,2,1,2,0,0,2,0,1,0,0,2,1,1,0,2,1,2,1,1,0,2,0,2,2,2,2,1,0,2,2,2,0,1,2,1,2,2,2,1,2,0,0,2,2,2,2,2,2,2,2,2,2,1,2,1,2,1,2,2,1,2,1,0,2,2,2,2,2,1,2,2,0,1,0,2,0,2,2,2,2,2,1,2,2,0,2,1,1,2,2,2,2,2,0,0,2,0,2,1,2,0,1,1,0,2,2,2,2,2,1,0,2,0,0,2,2,0,2,0,0,2,0,2,2,0,1,2,0,0,2,2,2,2,1,1,2,2,1,0,1,1,1,0,2,2,0,1,2,1,2,2,2,2,2,1,2,0,0,0,2,2,1,0,2,2,1,2,2,2,1,2,2,0,2,0,1,0,2,0,2,2,1,2,2,2,1,2,0,2,2,0,2,2,1,2,0,0,2,2,1,2,2,2,2,0,2,2,0,2,2,2,0,2,1,2,1,2,2,1,2,0,1,0,2,2,1,2,2,2,2,2,0,0,2,2,2,0,2,2,2,2,0,2,0,1,2,1,2,1,2,0,1,1,2,2,2,2,1,2,0,2,0,2,1,1,2,2,2,1,1,2,0,2,2,0,2,0,2,2,2,2,2,2,0,1,2,2,1,0,2,0,1,1,2,0,1,2,2,2,2,2,2,2,0,2,2,1,2,1,2,0,1,1,1,2,1,2,2,2,0,2,0,1,2,0,0,2,1,1,1,2,2,2,0,0,2,2,2,2,2,0,2,2,2,2,2,2,2,2,2,2,0,1,1,1,2,0,2,2,1,2,0,2,2,2,0,2,2,0,0,0,0,0,1,2,2,2,2,2,2,2,0,1,2,2,2,0,0,2,2,2,2,2,2,0,2,2,2,0,2,2,1,2,1,2,2,2,0,2,1,1,1,0,2,2,2,2,2,0,1,2,1,2,2,0,0,1,0,0,2,2,2,2,0,1,1,2,2,0,0,2,0,0,2,1,2,0,2,1,2,2,2,2,2,0,2,0,0,1,1,2,1,1,2,2,2,2,0,2,2,2,2,0,2,2,1,0,1,1,2,2,2,2,0,0,0,2,1,1,2,2,2,2,2,2,0,1,2,2,1,2,1,2,2,1,2,0,2,2,0,2,2,0,1,2,2,2,2,2,0,0,0,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,1,1,2,2,0,2,1,2,2,1,2,2,2,2,2,1,0,2,2,2,0,2,1,2,2,2,0,0,2,0,2,0,0,2,0,2,2,1,1,1,0,0,2,2,2,2,1,1,1,1,2,0,2,2,0,0,2,0,1,0,2,0,2,2,0,2,2,2,2,2,2,2,0,1,0,1,1,2,1,2,0,1,1,2,0,1,2,1,2,2,0,2,2,2,0,0,2,0,2,2,2,2,2,1,0,2,1,2,0,1,2,2,1,2,1,1,0,2,2,0,1,2,2,2,0,1,0,2,1,2,2,2,2,2,2,1,2,2,1,2,2,2,2,2,0,0,2,1,2,2,1,0,2,2,0,2,0,2,2,0,2,2,2,0,1,0,1,2,2,2,1,2,1,1,0,2,0,1,2,0,2,0,1,2,1,2,2,0,1,2,1,2,2,2,2,2,1,0,0,0,1,2,1,0,2,2,2,0,2,1,2,2,2,2,0,2,2,1,2,0,2,2,1,2,2,0,2,2,0,2,2,0,1,2,2,0,2,2,2,0,2,2,0,2,0,2,2,1,0,2,1,0,2,2,1,2,1,2,2,2,2,2,1,2,1,1,1,2,2,1,1,2,2,2,1,2,1,2,0,2,2,1,0,0,0,0,2,2,1,2,2,2,2,2,0,1,2,0,2,1,2,1,2,2,2,2,0,2,2,2,2,2,2,1,0,0,1,2,2,2,0,2,1,1,1,1,2,2,2,2,2,1,0,2,1,2,2,0,2,1,1,2,2,2,2,2,2,2,2,2,2,0,1,1,1,1,2,2,1,2,2,0,2,2,1,2,2,1,2,0,1,1,1,2,0,1,0,2,2,2,0,2,1,2,1,0,2,2,2,1,2,2,2,2,1,1,1,1,2,2,1,0,2,0,0,2,0,2,1,1,2,2,1,2,1,2,0,1,2,2,1,2,1,2,1,0,1,2,1,2,2,0,1,0,0,2,0,2,2,2,2,2,2,2,1,1,2,2,2,1,1,0,2,2,0,2,2,0,2,1,2,1,2,1,1,2,2,2,2,2,0,2,0,2,0,2,2,2,2,1,2,2,1,2,0,2,2,1,2,2,0,2,2,2,2,2,1,1,1,0,1,1,2,1,2,0,2,2,0,0,2,0,2,2,1,2,2,2,2,0,0,2,0,1,0,1,0,2,1,2,0,1,1,2,0,0,2,0,1,1,0,0,2,2,0,0,1,0,2,2,1,2,2,0,2,2,0,2,1,1,2,2,2,2,2,0,2,0,2,1,1,2,0,2,2,2,2,2,0,2,2,0,2,0,0,1,2,2,1,2,2,2,2,2,1,1,2,2,2,1,0,0,2,2,2,2,0,0,2,0,0,2,2,1,0,0,0,2,2,2,2,2,1,1,0,2,2,1,2,2,2,1,0,2,1,2,2,1,1,1,2,2,2,2,2,1,0,2,2,0,2,0,2,1,0,2,2,0,1,2,2,1,2,2,0,2,1,1,2,1,2,0,1,0,0,1,1,2,1,2,1,2,2,2,0,1,2,2,1,0,1,2,1,2,0,0,2,2,1,2,0,1,2,2,2,2,2,2,1,0,2,2,0,2,2,1,2,1,2,0,2,2,1,2,1,0,2,2,2,2,2,0,0,1,2,2,0,2,1,2,2,2,2,2,0,2,2,0,2,1,1,2,2,2,0,2,2,2,2,1,2,0,2,2,0,2,1,2,2,2,0,2,1,2,1,0,0,2,2,2,2,1,0,2,0,2,2,2,1,0,1,2,2,2,2,1,2,2,2,0,0,1,2,2,0,1,2,1,0,2,2,0,2,2,1,2,0,1,2,2,2,1,2,0,2,1,0,2,0,2,2,0,0,2,1,2,2,0,0,1,0,1,1,2,1,2,0,0,2,2,0,1,2,2,2,2,1,2,0,1,2,2,0,2,2,2,2,0,2,2,1,2,1,2,1,1,0,2,2,2,2,2,2,2,1,2,0,2,0,2,2,2,2,2,2,2,2,0,2,2,1,2,2,2,1,2,2,0,2,0,0,0,2,2,2,0,1,2,2,2,1,2,0,2,0,2,1,0,2,0,2,2,0,2,0,2,2,1,2,0,1,2,2,2,2,1,2,2,0,0,0,1,0,0,1,1,2,1,2,2,2,0,2,2,2,2,0,0,2,2,1,0,0,2,2,1,2,2,2,2,1,1,0,2,2,1,2,1,1,1,1,0,1,2,2,0,1,0,2,2,2,1,2,0,2,2,2,2,0,0,0,2,0,2,1,2,1,2,2,1,1,2,1,2,2,0,0,2,2,2,2,0,2,0,1,1,2,2,0,2,2,2,2,2,2,0,2,2,2,2,0,0,2,2,2,2,1,1,2,2,2,1,2,1,1,1,0,2,2,2,2,2,2,0,1,1,2,0,2,2,2,2,0,2,2,2,2,0,2,0,2,1,2,2,2,0,1,0,1,2,1,2,2,0,0,0,2,2,2,1,2,0,2,2,1,2,2,0,2,1,2,1,2,0,2,0,2,2,0,2,0,2,1,2,1,2,2,2,1,0,1,0,1,2,1,0,0,0,2,2,1,0,2,2,2,2,1,2,2,0,0,2,0,2,0,2,2,1,2,0,1,2,1,2,2,1,1,2,1,2,2,0,0,0,2,2,0,2,1,2,2,2,2,2,1,1,2,2,2,0,1,0,2,2,0,2,2,2,2,2,0,1,2,1,2,2,0,2,2,2,1,2,0,0,2,0,2,2,2,1,2,1,0,2,1,2,2,0,0,2,0,1,2,2,2,0,0,1,1,0,0,2,1,2,0,2,2,2,1,1,2,1,2,2,1,2,0,2,2,1,1,1,0,1,0,2,1,2,2,2,1,1,0,2,1,0,2,1,0,0,2,0,1,2,2,0,0,0,0,2,2,1,2,0,2,2,1,2,1,0,2,2,1,2,0,2,2,1,2,0,1,2,1,2,1,0,1,2,2,2,2,2,1,1,2,1,1,2,1,2,2,2,2,2,2,0,2,2,2,0,1,2,2,2,2,2,1,0,2,1,0,1,2,2,2,1,1,2,2,2,2,2,2,1,2,2,0,2,2,1,0,1,2,0,1,2,2,1,2,0,1,0,2,2,2,1,2,0,2,1,0,0,1,1,2,2,1,2,0,2,2,0,2,2,0,2,0,2,2,0,1,0,0,1,1,1,1,2,1,2,2,2,1,2,1,2,2,2,1,2,2,1,0,2,2,0,1,1,2,2,2,1,2,2,0,2,1,2,1,0,2,2,2,2,0,0,2,2,2,2,0,2,1,2,2,1,0,2,2,2,2,0,0,2,2,1,0,2,0,2,2,2,2,2,1,0,2,0,2,2,1,0,2,2,0,2,1,1,2,2,2,1,2,0,1,2,0,2,2,2,2,2,0,1,0,0,0,1,2,1,2,2,2,1,0,2,2,0,1,1,1,0,0,2,2,1,0,1,0,0,2,0,0,1,1,1,2,1,1,2,2,1,2,2,1,2,0,0,2,0,1,0,0,2,0,0,1,2,0,2,0,1,2,2,2,1,2,0,1,1,1,2,1,2,1,1,1,0,1,2,1,1,2,1,2,2,2,2,2,0,1,2,0,2,0,0,0,0,2,0,2,2,0,2,0,2,0,2,0,2,2,0,2,1,0,2,1,2,0,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,1,2,2,1,2,2,1,1,2,0,1,1,2,2,2,2,1,2,1,0,2,1,2,1,2,2,2,1,0,1,1,2,2,1,1,1,1,1,2,2,0,2,1,1,1,1,0,2,0,0,2,0,0,1,2,0,2,1,2,2,0,2,0,1,2,1,0,0,0,1,1,2,0,2,0,2,1,2,0,2,2,0,2,2,1,2,1,0,1,2,1,1,0,1,0,2,0,2,2,1,1,2,2,2,1,0,1,2,2,2,0,2,0,2,2,0,2,2,2,2,1,1,1,2,2,2,2,1,1,2,2,2,1,2,0,2,2,2,2,2,1,1,2,0,2,1,1,2,2,2,0,2,2,1,2,1,1,1,2,2,1,0,2,2,2,2,2,2,1,2,2,0,2,2,2,1,1,1,1,2,2,2,2,1,2,1,2,2,0,2,2,0,1,2,2,2,2,2,2,1,2,2,2,2,0,1,2,1,2,2,1,2,2,2,2,1,1,0,2,2,2,0,1,2,2,2,2,0,2,2,1,2,2,2,0,0,2,1,2,0,2,1,2,2,1,2,0,0,2,1,0,2,0,2,1,1,2,2,2,2,1,2,2,0,2,0,0,2,0,2,2,0,2,2,1,2,2,0,0,1,2,1,1,2,2,2,2,2,2,2,0,2,2,2,2,1,1,1,2,2,1,2,0,0,2,0,0,0,2,2,1,0,1,2,2,2,2,2,2,1,0,0,1,1,2,2,2,0,0,2,0,2,2,1,1,0,2,2,2,2,1,1,1,0,1,0,2,0,2,0,0,2,0,2,0,0,2,0,2,2,2,2,1,0,2,2,2,1,1,0,0,1,2,2,2,2,2,0,0,2,0,1,2,1,2,1,0,2,1,0,0,0,1,0,2,2,2,0,2,0,0,2,1,2,0,0,2,2,2,2,0,0,0,2,2,2,0,2,2,2,2,0,0,2,2,2,2,1,2,2,0,2,2,2,0,2,2,2,2,2,2,1,2,2,2,1,0,2,2,2,2,2,2,0,2,0,2,2,2,1,1,0,2,2,2,2,0,0,0,1,0,0,0,2,2,2,0,1,1,1,2,2,2,0,0,0,2,1,1,2,2,0,0,2,0,0,0,2,0,0,1,2,0,2,1,0,2,2,2,2,1,2,2,2,2,2,0,0,2,2,1,0,2,2,1,2,0,0,2,2,0,2,2,1,0,0,2,1,2,1,2,2,1,0,2,2,1,1,2,0,2,2,2,2,1,1,0,2,2,2,1,2,1,0,2,0,1,2,0,2,1,2,1,2,2,2,2,2,2,1,1,1,0,2,0,2,2,2,2,2,1,2,2,0,2,0,1,2,2,2,1,0,2,1,2,2,1,1,2,1,1,0,2,2,2,2,1,0,2,2,0,0,2,2,2,1,2,0,0,1,2,2,2,2,2,2,0,2,0,2,0,2,1,0,1,0,1,2,1,2,0,2,2,1,2,0,2,2,2,2,2,2,0,0,2,0,0,1,0,2,2,2,2,2,1,2,2,2,0,2,1,1,2,1,1,1,0,2,0,0,0,2,1,0,1,2,0,1,2,0,2,2,0,2,2,0,0,2,2,2,0,1,2,2,2,1,0,2,0,2,0,0,0,2,2,2,2,0,2,0,0,2,0,2,2,2,2,2,2,2,1,2,2,0,2,2,0,2,1,2,1,2,1,1,2,2,0,2,2,1,2,0,2,2,2,2,1,2,2,0,1,0,0,1,2,0,0,1,1,1,2,2,2,0,0,2,2,0,1,2,1,2,1,2,2,1,1,2,2,2,1,0,2,0,2,0,2,1,2,2,1,2,1,1,2,0,1,2,0,0,2,0,2,2,0,2,0,2,2,2,1,2,2,0,1,2,2,0,0,1,1,1,0,2,1,2,2,2,2,1,2,2,1,2,1,0,1,2,0,2,2,0,1,0,2,2,0,2,2,2,1,1,1,2,0,2,1,2,2,2,2,1,2,2,0,2,2,2,2,2,0,1,2,1,2,0,0,0,1,2,2,2,0,0,2,0,2,0,2,1,2,2,0,2,2,2,2,2,2,2,1,1,1,2,2,2,0,0,2,1,1,2,2,0,1,0,1,0,0,2,2,1,2,0,0,2,2,2,1,2,0,0,2,2,1,0,2,2,2,2,2,2,2,1,2,0,2,1,2,0,2,1,0,2,2,2,2,1,2,2,0,1,2,1,2,1,2,2,2,0,2,2,1,2,2,2,0,2,2,0,2,2,2,2,2,2,0,2,1,2,1,0,2,2,2,0,0,2,2,2,2,1,0,2,2,2,2,0,2,0,0,0,1,2,1,2,2,2,2,2,2,2,2,0,2,1,1,2,1,2,1,0,1,2,2,2,2,2,2,1,1,1,1,2,2,2,2,2,0,1,1,0,0,1,2,1,0,0,1,1,1,2,2,0,0,2,2,2,2,2,0,0,2,0,0,0,0,2,0,0,2,1,2,2,0,0,2,2,1,2,2,2,2,1,2,0,2,0,2,1,0,1,2,2,0,2,0,0,1,2,0,0,2,1,1,1,0,2,1,2,1,2,1,1,1,2,0,0,2,0,0,2,2,2,0,0,0,2,0,2,0,2,1,2,2,2,1,2,1,2,1,0,0,2,0,2,2,2,0,2,0,2,1,2,0,2,2,2,2,2,1,0,2,0,2,1,0,1,0,2,2,0,2,1,2,2,0,1,2,1,1,2,2,2,2,2,2,2,1,0,1,2,2,1,2,2,2,2,0,0,0,2,2,2,1,1,0,1,1,2,1,2,2,0,0,1,0,2,0,2,2,0,1,0,0,2,2,1,1,2,0,2,0,2,2,1,2,0,0,1,0,0,1,2,0,2,1,2,1,2,2,1,2,2,2,1,2,2,2,2,2,2,2,0,0,2,2,2,2,1,1,2,0,2,1,0,1,2,1,2,0,1,1,2,2,0,0,2,2,2,2,2,0,2,0,2,0,1,1,2,1,1,0,2,1,2,2,2,2,2,0,1,2,1,2,0,2,2,0,2,2,2,0,0,2,2,1,2,2,0,1,0,2,2,2,2,2,0,2,2,1,0,1,2,2,1,2,2,0,1,0,2,2,0,2,2,0,1,2,2,1,1,0,0,2,1,2,0,1,0,1,2,0,0,2,0,2,0,2,2,2,2,2,2,2,2,0,0,0,2,0,2,1,2,2,2,1,0,1,2,0,0,2,1,0,2,1,2,0,2,1,1,2,2,0,2,0,1,2,0,1,2,2,2,2,0,2,2,2,2,1,0,0,0,2,1,2,2,2,2,2,0,0,2,2,2,1,1,1,1,0,1,2,2,0,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,0,2,2,1,1,0,2,2,1,0,0,2,2,2,1,0,1,2,1,0,2,1,0,0,0,2,0,1,2,2,2,2,2,0,1,1,2,2,0,1,0,1,2,0,0,0,1,2,1,2,0,0,1,2,2,0,2,2,2,2,1,0,2,1,1,0,1,0,1,1,2,2,0,2,0,0,2,2,1,2,2,2,0,1,0,1,2,0,0,1,1,0,1,2,0,2,2,1,0,2,2,2,2,0,0,2,2,2,1,0,1,0,2,2,1,2,1,2,1,0,1,2,2,1,0,0,0,0,2,0,0,2,2,2,2,2,2,2,0,0,2,1,2,0,0,0,2,2,2,2,0,1,2,0,2,2,2,2,0,1,2,2,2,2,0,2,0,0,0,1,0,0,1,0,1,0,2,2,0,2,2,2,1,1,2,0,0,2,1,2,2,2,1,0,0,2,1,0,2,0,1,1,1,0,2,1,1,2,2,2,0,2,2,0,1,2,0,0,1,2,1,2,1,2,2,1,1,2,2,1,2,1,0,0,0,1,1,2,2,1,2,0,0,2,1,0,2,0,1,2,2,2,2,2,0,2,1,2,1,2,1,1,2,1,0,2,0,2,2,2,2,2,0,0,1,1,0,1,0,2,0,2,2,1,2,2,2,2,0,0,2,2,2,0,0,0,1,2,0,1,1,2,2,0,2,1,2,2,0,1,1,2,2,2,2,1,1,0,1,0,0,2,1,1,2,0,1,2,0,2,2,2,2,1,2,2,1,2,0,0,0,1,2,1,1,2,0,1,1,2,2,0,0,1,2,2,0,2,2,2,2,2,2,0,1,2,1,1,0,0,0,2,0,2,1,2,0,2,2,2,2,2,1,0,2,1,1,0,2,2,2,2,2,2,1,2,2,1,1,2,1,2,2,2,1,2,0,2,2,1,1,0,2,0,0,2,0,2,0,2,1,1,2,2,0,0,0,1,2,1,2,2,0,2,2,2,2,2,0,2,2,0,2,1,0,1,0,2,1,1,2,0,2,1,1,0,2,1,0,2,1,2,2,2,0,1,2,2,1,2,2,2,2,0,2,2,2,1,0,2,2,2,0,0,1,2,1,2,1,1,1,2,0,0,2,0,0,2,0,0,1,2,0,0,2,2,0,2,0,2,1,1,2,1,0,2,2,2,0,0,2,2,2,2,2,0,2,2,0,1,2,1,0,2,1,0,0,0,0,0,1,1,1,2,2,1,2,0,0,2,1,2,1,1,1,2,0,2,1,2,1,2,2,1,0,2,2,2,0,1,0,1,1,1,2,0,2,1,2,0,0,2,1,0,2,2,2,0,0,2,2,1,2,2,1,1,1,2,2,2,0,1,2,1,1,1,2,0,1,0,0,2,2,2,0,2,2,1,0,1,0,1,0,0,1,1,0,0,0,2,2,0,2,1,0,1,2,2,2,1,0,2,0,1,1,0,0,2,2,1,0,1,1,2,2,2,0,2,2,2,2,2,2,0,0,1,2,2,0,0,2,2,0,2,0,1,2,2,0,2,2,2,1,1,2,2,0,1,2,0,1,0,1,2,2,2,2,2,2,2,1,2,1,0,1,2,1,2,2,1,2,1,2,0,2,2,2,2,1,0,1,1,0,2,2,2,0,1,0,2,1,2,2,1,2,2,2,1,2,1,2,2,2,1,0,0,1,2,2,2,0,0,2,1,0,0,2,2,1,0,0,2,2,2,2,0,2,0,0,2,0,1,1,2,1,0,0,2,1,2,2,0,2,1,2,2,0,0,0,0,0,2,0,1,0,1,0,2,0,0,1,2,2,2,2,0,0,0,1,2,2,2,2,2,2,0,2,0,2,1,1,2,0,2,1,1,1,2,0,0,2,2,2,0,1,0,0,1,1,2,1,1,1,2,1,0,2,2,2,2,1,2,2,2,2,2,2,2,0,1,0,1,2,1,0,2,2,2,2,2,2,0,1,0,2,0,2,2,2,0,0,2,2,2,2,2,2,1,2,0,2,0,2,0,2,0,1,2,1,1,1,1,2,2,0,0,2,2,2,1,2,2,2,2,2,2,1,1,2,0,1,0,1,1,2,0,0,0,1,2,2,2,1,1,2,1,1,0,1,2,0,2,2,2,0,1,1,0,1,2,2,2,2,0,2,2,1,1,0,2,0,2,2,2,2,2,1,1,2,1,1,2,1,2,2,2,2,0,2,2,2,0,1,0,2,2,0,1,2,1,2,0,2,2,0,1,2,1,0,2,1,2,0,2,1,2,1,2,1,0,2,1,2,2,2,2,1,2,0,1,2,2,2,0,0,0,2,1,2,0,1,2,1,2,2,2,2,2,1,1,2,2,2,1,1,2,1,2,1,2,2,1,2,0,1,1,2,1,2,0,0,2,2,2,1,2,0,1,1,0,2,2,2,2,0,2,2,0,0,2,2,1,0,2,1,2,2,0,1,0,1,1,2,0,0,1,2,0,0,2,2,1,0,1,2,0,1,0,0,2,1,1,2,2,2,0,2,0,2,2,0,2,0,2,1,2,1,2,2,0,2,1,0,0,2,1,2,1,2,0,0,2,1,2,2,1,2,0,2,2,0,2,2,0,0,2,2,2,2,2,2,2,2,2,1,2,0,2,2,1,0,0,2,1,2,2,2,1,2,2,2,2,2,0,2,2,2,2,1,2,2,1,2,0,2,2,0,2,2,2,2,0,2,2,1,1,2,0,1,2,2,2,2,2,1,1,0,0,2,1,2,1,2,2,2,0,2,0,2,2,2,1,0,1,2,1,2,0,2,0,2,0,1,1,0,0,0,2,2,0,2,1,1,1,2,2,2,1,1,2,0,2,2,0,2,2,0,1,2,1,2,2,1,2,1,2,1,2,0,2,2,0,0,1,0,1,0,1,2,1,2,2,2,2,0,2,2,0,1,2,1,1,1,1,2,2,0,2,0,2,2,2,2,1,2,2,0,2,0,0,2,2,2,2,0,1,0,1,0,1,2,2,1,1,2,2,2,0,1,2,2,0,2,1,2,2,1,2,1,0,2,2,0,0,0,0,2,2,1,2,1,2,2,2,2,1,0,0,0,2,1,2,0,1,1,0,1,2,0,2,2,0,0,1,0,1,2,1,1,2,0,2,0,2,1,1,1,0,1,2,2,1,1,1,2,2,0,1,1,2,2,0,2,2,2,0,1,0,1,2,0,2,0,2,2,2,1,2,0,2,0,1,0,2,2,2,1,0,2,2,0,0,0,2,0,1,2,0,1,2,1,0,1,2,1,2,0,2,1,2,2,0,2,1,1,2,0,2,0,2,2,0,0,1,1,0,2,1,2,0,1,2,2,0,2,2,2,0,1,2,2,2,2,2,1,2,1,2,0,0,2,2,2,0,2,2,2,1,0,1,2,2,2,2,0,2,2,0,2,1,0,1,0,2,0,1,2,2,2,2,2,1,0,2,1,0,0,2,0,2,0,1,0,1,0,2,0,1,1,1,0,0,2,1,2,0,2,1,0,2,1,0,2,1,2,2,0,1,0,2,1,2,0,2,1,0,0,2,2,2,1,0,0,2,2,1,2,2,2,2,1,0,1,2,0,1,2,0,1,2,0,2,2,0,0,2,0,2,0,2,2,1,2,2,0,2,0,2,0,0,0,1,1,1,1,0,1,2,2,1,2,2,2,1,2,2,2,2,0,0,2,2,0,2,0,1,2,2,0,2,2,0,0,0,2,0,2,0,1,1,1,2,2,2,1,0,1,0,0,1,1,0,1,2,2,1,2,0,0,2,2,1,2,1,0,1,2,0,2,1,1,2,0,0,2,1,0,0,2,0,1,2,1,1,2,1,1,0,0,2,1,2,2,2,2,1,1,2,1,0,1,2,0,2,1,1,2,2,2,2,1,0,2,1,0,0,0,0,1,0,1,0,2,2,0,2,2,0,1,2,1,2,0,2,0,2,1,2,1,0,0,2,2,1,0,2,0,2,0,2,0,1,2,1,1,2,0,0,0,0,1,2,2,0,2,2,2,0,1,0,2,1,0,1,1,1,0,2,0,0,0,0,0,2,0,0,2,0,0,1,1,2,2,2,0,0,2,2,2,0,2,2,0,2,2,2,2,2,2,2,2,2,1,1,0,1,2,2,2,2,2,1,2,0,0,1,2,0,2,1,2,0,1,2,1,1,0,1,0,1,0,2,0,2,2,2,0,2,0,1,0,0,0,1,1,1,1,1,1,0,2,2,2,1,1,1,0,2,1,0,2,2,1,1,1,2,1,2,2,0,2,2,1,0,2,1,0,0,2,0,0,2,0,1,2,0,1,0,1,2,2,1,2,0,0,0,1,1,1,2,1,0,2,0,0,0,1,2,2,1,0,1,0,2,0,0,0,0,2,1,0,0,1,2,0,0,0,0,2,0,1,1,0,1,2,2,2,1,0,1,1,2,0,0,2,1,0,2,2,1,1,0,2,2,0,1,1,0,0,1,0,0,0,2,0,2,1,2,0,2,2,2,0]
frameL = 25*6
heightL = 25

--input = [1,2,3,4,5,6,7,8,9,0,1,2]
--frameL = 2*3
--heightL = 3

separateLayers frames lLen layeredFrames
  | null frames = layeredFrames
  | otherwise = separateLayers
    (tail frames)
    lLen
    (layeredFrames ++ [(separate (head frames) lLen lLen [] [])])

separate xs len frameLen frame frames
  | null xs = (frames ++ [frame])
  | len == 0 = separate xs frameLen frameLen [] (frames ++ [frame])
  | otherwise = separate (tail xs) (len-1) frameLen (frame ++ [head xs]) frames

picture = separateLayers (separate input frameL frameL [] []) heightL []
layers = (separate input frameL frameL [] [])

getZeroesCount [] [] c counts = (counts ++ [c])
getZeroesCount layer layers c counts
  | null layer = getZeroesCount (head layers) (tail layers) 0 (counts ++ [c])
  | (head layer) == 0 = getZeroesCount (tail layer) layers (c+1) counts
  | otherwise = getZeroesCount (tail layer) layers c counts


getElementCount [] el count = count
getElementCount (x:xs) el count = if x == el then (getElementCount xs el (count+1)) else (getElementCount xs el count)

zerosCount = getZeroesCount (head layers) (tail layers) 0 []
-- minimum zerosCount -> layer 10

minLayer = (layers !! 10)

mergePoint result n layers
  | null layers = result
  | otherwise = mergePoint (if result < 2 then result else ((head layers) !! n)) n (tail layers)

mergePoints layers n results
  | n == (length (head layers)) = results
  | otherwise = mergePoints layers (n+1) (results ++ [mergePoint 2 n layers])

merged = mergePoints layers 0 []
result = separateLayers merged heightL []

main = putStrLn "Hello World"