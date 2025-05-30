import Utilities ((|>))
import TypesAsSets (listOfAllElements, Finite)

f :: Integer -> Integer
f = (+1) |> (+1)

main :: IO ()
main = pure ()

g :: Finite t => [t]
g = listOfAllElements