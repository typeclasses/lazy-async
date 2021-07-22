module Test.Optics (focus, _Incomplete, _Failure, _Success, _Done) where

import Control.Monad     (Monad (return))
import Data.Function     ((.))
import Data.Maybe        (maybe)
import Hedgehog          (MonadTest, failure)
import LazyAsync         (Outcome, Status)
import Optics.AffineFold (An_AffineFold, preview)
import Optics.Optic      (Is, Optic')
import Optics.TH         (makePrisms)

$(makePrisms ''Status)
$(makePrisms ''Outcome)

focus :: (MonadTest m, Is k An_AffineFold) => Optic' k is s a -> s -> m a
focus o = maybe failure return . preview o
