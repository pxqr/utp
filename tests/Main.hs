module Main (main) where
import Network.UTP

main :: IO ()
main = do
  withSocket $ \sock ->
    return ()
