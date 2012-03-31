module Site.Paypal where

import qualified Happstack.Server            as H
import Control.Monad                         ( ap )
import Network.URL                           ( importURL, url_params )
import Data.Time.Clock                       ( UTCTime )
import System.Locale                         ( defaultTimeLocale )
import Data.Time.Format                      ( parseTime )
import Text.Email.Validate                   ( EmailAddress, validate )

sandbox "https://www.sandbox.paypal.com/cgi-bin/webscr" 
live = "https://www.paypal.com/cgi-bin/webscr"
current = sandbox

incomingIPN db = logError $ runErrorT $ do
    urlstr <- H.getURL
    Log.debugShow ("incomingIPN", urlstr)
    url <- importURL urlstr `justOr` throwError "couldn't parse URL"
    payment <- toIPN (url_params params)
    when ((reciever_email payment) /= myemail) $ throwError $ show ("incoming ipn had unexpected reciever_email", url, payment)
    validate_host = <- importURL current `justOr` throwError "couldn't parase validate host url"
    validate_url = foldl1 add_param validate_host $ ("cmd", "_notify-validate"):(url_params params)
    doAtomicallyWithIO $  do
        addPending payment
        rv <- Browser.post 200 validate_url
        when ((upcase rv) /= "VERIFIED") $ throwError $ "coudln't verify incomming ipn got" ++ (show rv)

toIPN params = do
    tnx_type <- "txn_type" `elookup` params
    case (tnx_type) of
        "recurring_payment" -> toPayment params
        "subscr_payment"    -> toPayment params
        "virtual_terminal"  -> toPayment params
        "web_accept"        -> toPayment params
        _                   -> throwError $ "unexpected tnx_type" ++ tnx_type

toPayment params = do
    return Payment `ap` ("receiver_email" `elookup` params >>= parse)
                   `ap` ("txn_id"         `elookup` params)
                   `ap` ("payer_email"    `elookup` params >>= parse)
                   `ap` ("payment_fee"    `elookup` params >>= parse)
                   `ap` ("payment_gross"  `elookup` params >>= parse)
                   `ap` ("payment_date"   `elookup` params >>= parse)
                   `ap` ("payment_status" `elookup` params >>= parse)

elookup str params = do
    (str `lookup` params) `justOr` (throwError $ "missing key in url data " ++ (show (str, params)))

