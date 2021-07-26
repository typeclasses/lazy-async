{- |

"Higher-kinded datatypes", for tests
involving functions like 'memoizeRank2'

-}

module Test.Person where

import Data.String     (String)
import Numeric.Natural (Natural)
import Rank2.TH        (deriveAll)

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

$(deriveAll ''Location)
$(deriveAll ''Person)
