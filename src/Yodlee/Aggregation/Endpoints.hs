{-# LANGUAGE OverloadedStrings #-}
module Yodlee.Aggregation.Endpoints where

import           Control.Concurrent.Async
import           Control.Error
import           Control.Exception.Base     (SomeException)
import           Control.Lens.Combinators
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Monoid
import           Data.String                (fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Typeable              as Typeable
import qualified Data.Vector                as V
import           Network.Wreq               as HTTP
import           Network.Wreq.Session       as HTTPSess
import           Network.Wreq.Types
import           Yodlee.Aggregation.Types

urlBase :: String
urlBase = "https://rest.developer.yodlee.com/services/srest/restserver/v1.0"

cobrandSessionToken :: Traversal' Value T.Text
cobrandSessionToken = key "cobrandConversationCredentials" . key "sessionToken" . _String

userSessionToken :: Traversal' Value T.Text
userSessionToken = key "userContext" . key "conversationCredentials" . key "sessionToken" . _String

siteId :: Traversal' Value Integer
siteId = key "siteId" . _Integer

siteAccountId :: Traversal' Value Integer
siteAccountId = key "siteAccountId" . _Integer

-- $endpoint
-- Those functions correspond to the identically named Yodlee Aggregation REST
-- APIs. Some functions have a number following them. I don't know why.

-- | Perform an action and catch all synchronous exceptions.
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

performAPIRequestVerbose :: Bool
performAPIRequestVerbose = True

httpOptions :: Options
httpOptions = set HTTP.checkStatus (Just (\_ _ _ -> Nothing)) defaults

performAPIRequest :: (Show a, Postable a) => String -> String -> a -> Yodlee (Response Value)
performAPIRequest whence urlPart postable = do
  session <- lift ask
  let url = urlBase <> urlPart
  when performAPIRequestVerbose $ do
    liftIO $ putStrLn "********** WILL PERFORM API REQUEST"
    liftIO $ print url
    liftIO $ print postable
  mbBs <- liftIO . tryAny $ HTTPSess.postWith httpOptions session url postable
  when performAPIRequestVerbose $ do
    liftIO $ putStrLn "********** HAS PERFORMED API REQUEST"
    liftIO $ print mbBs
    liftIO $ putStrLn "********** DONE WITH API REQUEST"
  bs <- hoistEither $ fmapL (ErrorAt whence . HTTPFetchException) mbBs
  let expectedStatus = 200 -- This may become an argument in the future.
  unless (view (responseStatus . statusCode) bs == expectedStatus) $
    hoistEither $ Left $ ErrorAt whence $ UnexpectedHTTPStatus (view responseStatus bs)
  r <- hoistEither $ note (ErrorAt whence (JSONParseFailed (view responseBody bs))) $ asValue bs
  when (preview (responseBody . key "errorOccurred" . _String) r == Just "true") $ -- I don't know why Yodlee decides to return true as a string.
    hoistEither $ Left $ ErrorAt whence $ JSONErrorObject (view responseBody r)
  return r

assertOutputIsJust :: String -> Response Value -> Maybe a -> Yodlee a
assertOutputIsJust whence resp = hoistEither . note (ErrorAt whence (JSONValidationFailed (view responseBody resp)))

assertOutputBool :: String -> Response Value -> Bool -> Yodlee ()
assertOutputBool whence resp condition = unless condition . hoistEither . Left . ErrorAt whence $ JSONValidationFailed (view responseBody resp)

assertInputIsJust :: (Typeable.Typeable c) => String -> c -> Maybe a -> Yodlee a
assertInputIsJust whence arg = hoistEither . note (ErrorAt whence (ArgumentValidationFailed (Typeable.typeOf arg)))

-- | This authenticates the cobrand. Once the cobrand is authenticated a
-- 'CobrandSession' is created and the token within the 'CobrandSession' expires
-- every 100 minutes. Exceptions will be thrown on network errors, but 'Nothing'
-- will be returned if the server did not send a valid JSON response, or the
-- JSON response does not contain the expected fields.
coblogin :: CobrandCredential -> Yodlee CobrandSession
coblogin credential = do
  let whence = "coblogin"
  r <- performAPIRequest whence "/authenticate/coblogin"
    [ "cobrandLogin" := view cobrandUsername credential
    , "cobrandPassword" := view cobrandPassword credential
    ]
  assertOutputBool whence r $ has (responseBody . cobrandSessionToken) r
  assertOutputIsJust whence r $ preview (responseBody . _Value . to CobrandSession) r

-- | This accepts a consumer's details to register the consumer in the Yodlee
-- system. After registration, the user is automatically logged in.
register3 :: CobrandSession -> UserRegistrationData -> Yodlee UserSession
register3 cbSess userReg = do
  let whence = "register3"
  let regParamsReq =
        [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
        , "userCredentials.loginName" := view (userCredential . userUsername) userReg
        , "userCredentials.password" := view (userCredential . userPassword) userReg
        , "userCredentials.objectInstanceType" := ("com.yodlee.ext.login.PasswordCredentials" :: T.Text)
        , "userProfile.emailAddress" := view userEmail userReg
        ]
  let optParamMap = [ ("userProfile.firstName", userFirstName)
                          , ("userProfile.lastName", userLastName)
                          , ("userProfile.middleInitial", userMiddleInitial)
                          , ("userProfile.address1", userAddress1)
                          , ("userProfile.address2", userAddress2)
                          , ("userProfile.city", userCity)
                          , ("userProfile.country", userCountry)
                          ]
  let regParamsOpt = catMaybes ((\(fieldName, fieldLens) -> (fieldName :=) <$> preview fieldLens userReg) <$> optParamMap)
  r <- performAPIRequest whence "/jsonsdk/UserRegistration/register3" (regParamsOpt <> regParamsReq)
  checkUserSession whence r

-- | This enables the consumer to log in to the application. Once the consumer
-- logs in, a 'UserSession' is created. It contains a token that will be used in
-- subsequently API calls. The token expires every 30 minutes.
login :: CobrandSession -> UserCredential -> Yodlee UserSession
login cbSess userCred = do
  let whence = "login"
  r <- performAPIRequest whence "/authenticate/login"
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "login" := view userUsername userCred
    , "password" := view userPassword userCred
    ]
  checkUserSession whence r

checkUserSession :: String -> Response Value -> Yodlee UserSession
checkUserSession whence r = do
  assertOutputBool whence r $ has (responseBody . userSessionToken) r
  assertOutputIsJust whence r $ preview (responseBody . _Value . to UserSession) r

-- | This searches for sites. If the search string is found in the display name
-- parameter or aka parameter or keywords parameter of any 'Site' object, that
-- site will be included in this list of matching sites.
searchSite :: CobrandSession -> UserSession -> T.Text -> Yodlee [Site]
searchSite cbSess user site = do
  let whence = "searchSite"
  r <- performAPIRequest whence "/jsonsdk/SiteTraversal/searchSite"
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "userSessionToken" := view (_UserSession . userSessionToken) user
    , "siteSearchString" := site
    ]
  -- This guard checks it contains a siteId. Do not remove it! The correctness of @siteId@ depends on it.
  assertOutputBool whence r $ allOf (responseBody . _Array . traverse) (has (key "siteId" . _Integer)) r
  return $ toListOf (responseBody . _Array . traverse . to Site) r

-- | This is a 'Fold' that allows you to directly obtain a list of
-- 'SiteCredentialComponent' using 'toListOf'. Alternatively, you can also use
-- 'getSiteLoginForm' to achieve the same thing with an HTTP call in 'IO'. (But
-- why?)
siteLoginForm :: Fold Site SiteCredentialComponent
siteLoginForm = _Site . key "loginForms" . _Array . to V.indexed . traverse . to (uncurry (SiteCredentialComponent Nothing))

-- | This provides the login form associated with the requested site, given a
-- 'SiteId'. It is unknown why this needs to exist because 'searchSite' already
-- returns this information, but it's included as per recommendation from
-- Yodlee. The login form comprises of the credential fields that are required
-- for adding a member to that site. This call lets the consumers enter their
-- credentials into the login form for the site they are trying to add.
getSiteLoginForm :: CobrandSession -> Site -> Yodlee [SiteCredentialComponent]
getSiteLoginForm cbSess site = do
  let whence = "getSiteLoginForm"
  i <- assertInputIsJust whence site $ preview (_Site . siteId) site
  r <- performAPIRequest whence "/jsonsdk/SiteAccountManagement/getSiteLoginForm"
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "siteId" := show i
    ]
  -- Check that the conjunctionOp is 1, which is AND, i.e. the form fields form a product type. We don't want to deal with sum types.
  -- By the way, the second key "conjuctionOp" is misspelled.
  assertOutputBool whence r . (== Just 1) $ preview (responseBody . _Value . key "conjunctionOp" . key "conjuctionOp" . _Integer) r
  let rv = toListOf (responseBody . key "componentList" . _Array . to V.indexed . traverse . to (uncurry (SiteCredentialComponent Nothing))) r
  assertOutputBool whence r $ allOf traverse (\obj -> allOf (traverse . traverse) (`has'` obj) siteCredentialExpectedFields) rv
  return rv
  where has' l = isJust . preview l

siteCredentialExpectedFields :: [(C.ByteString, Getting (First T.Text) SiteCredentialComponent T.Text)]
siteCredentialExpectedFields =
  [ ("displayName", siteCredItemDisplayName)
  , ("fieldType.typeName", siteCredItemTypeName)
  , ("name", siteCredItemName)
  , copyPrimitive "size"
  , copyPrimitive "maxlength"
  , copyString "valueIdentifier"
  , copyString "valueMask"
  , copyString "helpText"
  , copyPrimitive "isEditable"
  , copyPrimitive "isOptional"
  , copyPrimitive "isEscaped"
  , copyPrimitive "isMFA"
  , copyPrimitive "isOptionalMFA"
  ]
  where copyString f = (fromString f, siteCredItemFormat . key (fromString f) . _String)
        copyPrimitive f = (fromString f, siteCredItemFormat . key (fromString f) . _Value . to encode . to CL.toStrict . to T.decodeUtf8)
        -- Very hacky! We basically ignore types and re-encode the component in JSON and convert it.

-- | This is a helper function that allows you to write a function to fill in a
-- list of 'SiteCredentialComponent's. The function inspects a
-- 'SiteCredentialComponent' and tries to return a 'T.Text' in response. If it
-- can fill in the field, it is expected to return a 'Just' value. Otherwise it
-- is expected to return a 'Nothing' value. This is a helper function in the
-- sense that it uses only operations already exposed in the public API.
fillInSiteCredentialComponents :: (SiteCredentialComponent -> Maybe T.Text) -> [SiteCredentialComponent] -> [SiteCredentialComponent]
fillInSiteCredentialComponents f = fmap fillInOnce
  where fillInOnce r = maybe id (set siteCredItemValue . Just) (f r) r

data KOF a = Keep a | Omit | Fail

resolveKOF :: [KOF a] -> Maybe [a]
resolveKOF = sequence . catMaybes . fmap change
  where change (Keep a) = Just (Just a)
        change Omit = Nothing
        change Fail = Just Nothing

validateSiteCreds :: [SiteCredentialComponent] -> Maybe [SiteCredentialComponent]
validateSiteCreds = resolveKOF . fmap validateOnce
  where validateOnce c
          | has (siteCredItemValue . _Just) c = Keep c
          | preview siteCredItemIsOptional c == Just True = Omit
          | otherwise = Fail

-- | This adds a member site account associated with a particular site.
-- refresh is initiated for the item. This API is expected to be called after
-- getting a login form for a particular site using 'getSiteLoginForm' or
-- @getSiteInfo@ or 'searchSite'.
addSiteAccount1 :: CobrandSession -> UserSession -> Site -> [SiteCredentialComponent] -> Yodlee SiteAccount
addSiteAccount1 cbSess user site siteCreds = do
  let whence = "addSiteAccount1"
  i <- assertInputIsJust whence site $ preview (_Site . siteId) site
  siteCredsValidated <- assertInputIsJust whence siteCreds $ validateSiteCreds siteCreds
  let transformCredPiece cred name traversal = (("credentialFields[" <> view (siteCredItemIndex . to show . to C.pack) cred <> "]." <> name) :=) <$> preview traversal cred
  let transformCred cred = uncurry (transformCredPiece cred) <$> (("value", siteCredItemValue . _Just) : siteCredentialExpectedFields)
  let transformed = concatMap transformCred siteCredsValidated
  credRequestParams <- assertInputIsJust whence siteCreds . sequence $ transformed
  let requestParams = [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
                      , "userSessionToken" := view (_UserSession . userSessionToken) user
                      , "siteId" := show i
                      , "credentialFields.enclosedType" := ("com.yodlee.common.FieldInfoSingle" :: T.Text) -- XXX
                      ] <> credRequestParams
  r <- performAPIRequest whence "/jsonsdk/SiteAccountManagement/addSiteAccount1" requestParams
  assertOutputBool whence r $ has (responseBody . siteAccountId) r
  assertOutputIsJust whence r $ preview (responseBody . _Value . to SiteAccount) r

-- | This retrieves the intermediate response for MFA enabled sites and provides
-- the MFA related field information that can be one of the following types:
-- image, security question, or token. This is a non-blocking API. The API will
-- return immediately if the intermediate response is not yet available.
getMFAResponseForSite :: CobrandSession -> UserSession -> SiteAccount -> Yodlee MFARefresh
getMFAResponseForSite cbSess user siteAcc = do
  let whence = "getMFAResponseForSite"
  siteAccId <- assertInputIsJust whence siteAcc $ preview (_SiteAccount . siteAccountId) siteAcc
  let requestParams = [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
                      , "userSessionToken" := view (_UserSession . userSessionToken) user
                      , "memSiteAccId" := show siteAccId
                      ]
  r <- performAPIRequest whence "/jsonsdk/Refresh/getMFAResponseForSite" requestParams
  assertOutputIsJust whence r $ preview (responseBody . _Value . to MFARefresh) r
