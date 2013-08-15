module Main (main) where

import Network.UTP

main :: IO ()
main = withSocket $ \sock ->
  return ()
