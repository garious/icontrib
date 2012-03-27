module Site.Charity where

import qualified DB.DB                       as DB
import qualified DB.CharityInfo              as C
import qualified Data.CharityInfo            as C
import qualified DB.UserInfo                 as U
import qualified Happstack.Server            as H
import qualified Site.Login                  as SL
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Popular                as P
import qualified Log                         as Log
import qualified Data.IxSet                  as IxSet
import Data.IxSet                            ( (@*), getOne )
import Site.Utils                            ( basename, getBody', post, get )
import JSONUtil                              ( jsonUpdate, jsonEncode )
import Monad                                 ( msum )
import Happstack.Server                      ( ServerPart
                                             , Response
                                             , dir
                                             )

import SiteError

charityServices :: DB.Database -> ServerPart Response
charityServices db = msum [ 
      dir "update"       (post (runErrorT $ updateOwners db))
    , dir "get.json"     (get  (runErrorT $ getOwners db))
    , dir "popular.json" (get  (popular db))
    , (get (public db))
    ]

updateOwners :: DB.Database -> SiteErrorT ()
updateOwners db = do 
    Log.debugM "trying to update registration" 
    ident <- SL.checkUser db 
    let empty = jsonEncode (C.empty { C.owner = ident })
    body <- getBody'
    test <- jsonUpdate empty body
    owners <- C.queryByOwner db ident
    mine <-         (getOne $ owners @* [C.cid test]) 
           `justOr` (getOne $ owners @* [C.ein test])
           `justOr` (return test)
    new <- jsonUpdate mine body
    C.updateInfo db ident new
 
getOwners :: DB.Database -> SiteErrorT [C.CharityInfo]
getOwners db = SL.checkUser db >>= (liftM IxSet.toList . C.queryByOwner db)

popular :: MonadIO m => DB.Database -> m [P.Popular]
popular db = (U.popularCharities db) >>= (C.toPopular db)

public :: DB.Database -> H.ServerPartT IO C.CharityInfo
public db = (basename >>= (failErrorT . C.queryByCID db . C.CharityID . BS.unpack))

