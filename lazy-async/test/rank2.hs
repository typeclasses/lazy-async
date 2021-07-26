module Main (main) where

import LazyAsync

import Test.Counter
import Test.General

import Control.Monad             ((>>=))
import Control.Monad.Trans.Class (lift)
import Data.Function             (($), (.))
import Data.Functor              (($>))
import Data.String               (String)
import Numeric.Natural           (Natural)
import System.IO                 (IO)

import Hedgehog (Property, discover, (===))

import qualified Rank2
import qualified Rank2.TH as Rank2

data Person f =
  Person
    { name     :: f String
    , age      :: f Natural
    , location :: Location f
    }

data Location f =
  Location
    { city  :: f String
    , state :: f String
    }

$(Rank2.deriveAll ''Person)
$(Rank2.deriveAll ''Location)

main :: IO ()
main = main' $$(discover)

prop_rank2 :: Property
prop_rank2 = contExample do
    tick <- expectTicks 2

    person <- Rank2.traverse memoize
        Person
            { name = tick $> "Chris"
            , age  = tick $> 34
            , location =
                Location
                  { city  = tick $> "Ronan"
                  , state = tick $> "Montana"
                  }
            }

    lift $ name person               >>= (=== "Chris")
    lift $ (state . location) person >>= (=== "Montana")
