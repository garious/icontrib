{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable #-}
module Query.DB where
import Data.DB
import Data.Acid
import Query.CharityInfo
import Query.Login
import Query.UserInfo
import Query.Paypal


$(makeAcidic ''DB [ 'addIdentityU, 'checkPasswordQ, 'addIdentityTokenU, 'tokenToIdentityQ, 'listIdentitiesQ, 'clearIdentityTokensU
                  , 'deleteCharityByEinU, 'charityInfoU, 'charityByOwnerQ, 'charityByIDQ
                  , 'userInfoU, 'userInfoByOwnerQ, 'mostInfluentialUserQ, 'usersQ, 'userCharitiesQ 
                  , 'clearValidatedPaymentU
                  ])

type Database = AcidState DB
