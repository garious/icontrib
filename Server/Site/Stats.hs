module Site.Stats where

import qualified DB.DB                       as DB
import qualified DB.UserInfo                 as U
import Site.Utils                            ( get )
import Control.Monad                         ( msum )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , dir
                                             )


stats :: DB.Database -> ServerPart Response
stats db = msum [ dir "programDistribution.json" (get  (U.programDistribution db))
                ]

