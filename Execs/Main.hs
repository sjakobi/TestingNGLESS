module Main
    ( main
    ) where

import Deps (runResourceT)
import Interpret

main = runResourceT $ interpret [()]

