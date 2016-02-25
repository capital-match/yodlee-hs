{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides functions that interfaces with the Yodlee Aggregation
-- REST API. This is a thin wrapper around the API.
module Yodlee.Aggregation
       (
         -- * The 'Default' class
         Default(..)
         -- * Data types
       , Yodlee
       , runYodlee
       , Error(..)
       , ErrorAt(..)
         -- ** API Input types
         -- $apiin
       , CobrandCredential
       , cobrandUsername
       , cobrandPassword
       , UserCredential
       , userUsername
       , userPassword
       , UserRegistrationData
       , userCredential
       , userEmail
       , userFirstName
       , userLastName
       , userMiddleInitial
       , userAddress1
       , userAddress2
       , userCity
       , userCountry
       , SiteCredentialComponent
       , siteCredItemFormat
       , siteCredItemValue
       , siteCredItemDisplayName
       , siteCredItemTypeName
       , siteCredItemName
       , siteCredItemSize
       , siteCredItemValueIdentifier
       , siteCredItemValueMask
       , siteCredItemIsEditable
       , siteCredItemIsOptional
         -- ** JSON 'Value's from the API
         -- $value
       , CobrandSession
       , _CobrandSession
       , UserSession
       , _UserSession
       , Site
       , _Site
       , siteLoginForm
       , SiteId
       , siteId
       , SiteAccount
       , _SiteAccount
         -- * Endpoints
         -- $endpoints
       , coblogin
       , register3
       , login
       , searchSite
       , getSiteLoginForm
       , addSiteAccount1
         -- * Helper functions
       , fillInSiteCredentialComponents
  ) where

import           Control.Error
import           Control.Exception.Base   (SomeException)
import           Control.Lens.Combinators
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           Network.Wreq             as HTTP
import           Network.Wreq.Session     as HTTPSess
import           Network.Wreq.Types
import Control.Concurrent.Async

-- $apiin
-- The API input data types store inputs to the APIs. This includes, for
-- example, the 'CobrandCredential' and 'UserCredential' types, which are data
-- structures that store the relevant credentials (username and password). The
-- data constructors for all those types are purposefully not exported. You are
-- expected to construct those objects by using 'def'. You can then set the
-- fields using the provided lenses, like this:
--
-- @
-- 'set' 'cobrandUsername' "username" . 'set' 'cobrandPassword' "password" $ 'def'
-- @
--

-- | 'CobrandCredential' is a data structure that stores the credentials for a
-- Yodlee cobrand login.
--
-- According to Yodlee, the cobrand login is a process by which a developer
-- authenticates their application with the Yodlee API before registering its
-- user and performing other actions like adding accounts, getting transactions,
-- etc., on behalf of its user.
$(declareLenses [d|
  data CobrandCredential = CobrandCredential
    { cobrandUsername :: T.Text
    , cobrandPassword :: T.Text
    } deriving (Show)
  |])

-- | The default value for 'CobrandCredential' is such that both the username
-- and password are 'T.empty'.
instance Default CobrandCredential where
  def = CobrandCredential T.empty T.empty

-- | 'UserCredential' is a data structure that stores user credentials.
$(declareLenses [d|
  data UserCredential = UserCredential
    { userUsername :: T.Text
    , userPassword :: T.Text
    } deriving (Show)
  |])

-- | The default value for 'UserCredential' is such that both the username and
-- password are 'T.empty'.
instance Default UserCredential where
  def = UserCredential T.empty T.empty

-- | 'UserRegistrationData' is a data structure that contains credentials and
-- the user profile, such as email, name, address, city, etc. Currently, not all
-- are supported.
$(declareLenses [d|
  data UserRegistrationData = UserRegistrationData
    { userCredential    :: UserCredential
    , userEmail         :: T.Text
    , userFirstName     :: Maybe T.Text
    , userLastName      :: Maybe T.Text
    , userMiddleInitial :: Maybe T.Text
    , userAddress1      :: Maybe T.Text
    , userAddress2      :: Maybe T.Text
    , userCity          :: Maybe T.Text
    , userCountry       :: Maybe T.Text
    } deriving (Show)
  |])

instance Default UserRegistrationData where
  def = UserRegistrationData def T.empty Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | 'SiteCredentialComponent' is a data structure that contains the expected
-- format as well as the provided value of a piece of site credential. In Yodlee
-- terms, site account is the association of a consumer with accounts available
-- in the site, and therefore a site login uses credential types that are unique
-- to each site. For this reason, a 'Getter' called 'siteCredItemFormat' is
-- provided to get the expected format of this piece of credential. The
-- credential may then be added by using the 'Lens'' called 'siteCredItemValue'.
$(declareLenses [d|
  data SiteCredentialComponent = SiteCredentialComponent
    { siteCredItemValue :: Maybe T.Text
    , siteCredItemIndex :: Int -- not exported
    , siteCredItemFormatInternal :: Value
    } deriving (Show)
  |])

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- 'SiteCredentialComponent'. This is slightly unusual because it's a 'Getter',
-- not a 'Lens'' to prevent modifications.
siteCredItemFormat :: Getter SiteCredentialComponent Value
siteCredItemFormat = siteCredItemFormatInternal

-- | This is the 'Fold' that allows you to extract the @displayName@ property
-- inside a 'SiteCredentialComponent'. This could have been a 'Getter' (indeed,
-- it should, and using 'preview' on it will always return a 'Just' value).
siteCredItemDisplayName :: Fold SiteCredentialComponent T.Text
siteCredItemDisplayName = siteCredItemFormat . key "displayName" . _String

-- | This is the 'Fold' that allows you to extract the @fieldType.typeName@
-- property inside a 'SiteCredentialComponent'. This could have been a 'Getter'
-- (indeed, it should, and using 'preview' on it will always return a 'Just'
-- value).
siteCredItemTypeName :: Fold SiteCredentialComponent T.Text
siteCredItemTypeName = siteCredItemFormat . key "fieldType" . key "typeName" . _String

-- | This is the 'Fold' that allows you to extract the @name@ property inside a
-- 'SiteCredentialComponent'. This could have been a 'Getter' (indeed, it
-- should, and using 'preview' on it will always return a 'Just' value).
siteCredItemName :: Fold SiteCredentialComponent T.Text
siteCredItemName = siteCredItemFormat . key "name" . _String

-- | This is the 'Fold' that allows you to extract the @size@ property inside a
-- 'SiteCredentialComponent'. This could have been a 'Getter' (indeed, it
-- should, and using 'preview' on it will always return a 'Just' value).
siteCredItemSize :: Fold SiteCredentialComponent Integer
siteCredItemSize = siteCredItemFormat . key "size" . _Integer

-- | This is the 'Fold' that allows you to extract the @valueIdentifier@
-- property inside a 'SiteCredentialComponent'. This could have been a 'Getter'
-- (indeed, it should, and using 'preview' on it will always return a 'Just'
-- value).
siteCredItemValueIdentifier :: Fold SiteCredentialComponent T.Text
siteCredItemValueIdentifier = siteCredItemFormat . key "valueIdentifier" . _String

-- | This is the 'Fold' that allows you to extract the @valueMask@ property
-- inside a 'SiteCredentialComponent'. This could have been a 'Getter' (indeed,
-- it should, and using 'preview' on it will always return a 'Just' value).
siteCredItemValueMask :: Fold SiteCredentialComponent T.Text
siteCredItemValueMask = siteCredItemFormat . key "valueMask" . _String

-- | This is the 'Fold' that allows you to extract the @isEditable@ property
-- inside a 'SiteCredentialComponent'. This could have been a 'Getter' (indeed,
-- it should, and using 'preview' on it will always return a 'Just' value).
siteCredItemIsEditable :: Fold SiteCredentialComponent Bool
siteCredItemIsEditable = siteCredItemFormat . key "isEditable" . _Bool

-- | This is the 'Fold' that allows you to extract the @isOptional@ property
-- inside a 'SiteCredentialComponent'. This could have been a 'Getter' (indeed,
-- it should, and using 'preview' on it will always return a 'Just' value).
siteCredItemIsOptional :: Fold SiteCredentialComponent Bool
siteCredItemIsOptional = siteCredItemFormat . key "isOptional" . _Bool

-- $value
-- This section contains data structures such as 'CobrandSession',
-- 'UserSession', and 'Site', which are returned by the Yodlee API. They are
-- implemented by wrapping a newtype around the raw @Value@. The reason is
-- because Yodlee does not seem to document very well exactly which fields are
-- present. To avoid the risk of the Haskell version getting out-of-date with
-- the upstream structure, we will trade some type safety here.
--
-- You can access the underlying 'Value' using the corresponding 'Getter'. You
-- can extract the 'Value', but you normally cannot modify those data structures
-- without extracting the 'Value' or construct them (unless you use
-- @unsafeCoerce@ in which case you should know what you are doing).

-- | 'CobrandSession' is the JSON data structure returned by the Yodlee API
-- after a successful cobrand login.
newtype CobrandSession = CobrandSession Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- a 'CobrandSession'.
_CobrandSession :: Getter CobrandSession Value
_CobrandSession = to (\(CobrandSession a) -> a)

-- | 'UserSession' is the JSON data structure returned by the Yodlee API after a
-- successful user login.
newtype UserSession = UserSession Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- a 'UserSession'.
_UserSession :: Getter UserSession Value
_UserSession = to (\(UserSession a) -> a)

-- | 'UserSession' is the JSON data structure returned by the Yodlee API after a
-- successful site search.
newtype Site = Site Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- 'Site'.
_Site :: Getter Site Value
_Site = to (\(Site a) -> a)

-- | 'SiteId' is a newtype wrapper for the site ID. You can get a 'SiteId' by
-- using the 'siteId' 'Getter'.
newtype SiteId = SiteId Integer deriving (Show)

-- | This is the 'Getter' that allows you to get a 'SiteId' from a 'Site'.
siteId :: Getter Site SiteId
siteId = to (fromJust <$> preview (_Site . key "siteId" . _Integer . to SiteId))

-- | 'SiteAccount' is the JSON data structure returned by the Yodlee API after
-- associating a customer with a site.
newtype SiteAccount = SiteAccount Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- a 'SiteAccount'.
_SiteAccount :: Getter SiteAccount Value
_SiteAccount = to (\(SiteAccount a) -> a)

urlBase :: String
urlBase = "https://rest.developer.yodlee.com/services/srest/restserver/v1.0"

cobrandSessionToken :: Traversal' Value T.Text
cobrandSessionToken = key "cobrandConversationCredentials" . key "sessionToken" . _String

userSessionToken :: Traversal' Value T.Text
userSessionToken = key "userContext" . key "conversationCredentials" . key "sessionToken" . _String

-- | The 'Yodlee' monad is a type returned by all endpoint functions. This /may/
-- become a @newtype@ in the future. The error type may also be more
-- descriptive, i.e. not just a 'Nothing' in case of error.
type Yodlee a = ExceptT ErrorAt (ReaderT HTTPSess.Session IO) a

-- | The 'runYodlee' function takes an action described by the 'Yodlee'
-- monad and executes it.
runYodlee :: Yodlee a -> IO (Either ErrorAt a)
runYodlee = HTTPSess.withSession . runReaderT . runExceptT

-- $endpoint
-- Those functions correspond to the identically named Yodlee Aggregation REST
-- APIs. Some functions have a number following them. I don't know why.

-- | The 'Error' type encapsulates every kind of error that can result from the
-- anything in the 'Yodlee' monad.
data Error
  = HTTPFetchException SomeException
    -- ^ This constructor represents errors resulting from the HTTP fetch, i.e.
    -- network errors, etc.
  | JSONParseFailed CL.ByteString
    -- ^ This constructor represents a /syntax/ error in the JSON data returned
    -- from Yodlee. The 'C.ByteString' that cannot be parsed is included.
  | JSONValidationFailed Value
    -- ^ This constructor represents a /semantic/ error in the JSON data
    -- returned from Yodlee. The JSON 'Value' that cannot be interpreted is
    -- included. This /may/ be caused by an error at the backend of Yodlee, but
    -- it can also caused by a semantic error provided as input. (Yodlee returns
    -- @200 OK@ for this kind of semantic errors.)
  | ArgumentValidationFailed
    -- ^ This constructor represents an error in one or more of the arguments
    -- passed to the function.
  deriving (Show)

-- | The 'ErrorAt' type contains an 'Error' and a 'String' describing whence the
-- error comes.
data ErrorAt = ErrorAt String Error
  deriving (Show)

-- | Perform an action and catch all synchronous exceptions.
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

performAPIRequest :: (Postable a) => String -> String -> a -> Yodlee (Response Value)
performAPIRequest whence urlPart postable = do
  session <- lift ask
  let url = urlBase <> urlPart
  mbBs <- liftIO . tryAny $ HTTPSess.post session url postable
  bs <- hoistEither $ fmapL (ErrorAt whence . HTTPFetchException) mbBs
  hoistEither $ note (ErrorAt whence (JSONParseFailed (view responseBody bs))) $ asValue bs

assertOutputIsJust :: String -> Response Value -> Maybe a -> Yodlee a
assertOutputIsJust whence resp = hoistEither . note (ErrorAt whence (JSONValidationFailed (view responseBody resp)))

assertOutputBool :: String -> Response Value -> Bool -> Yodlee ()
assertOutputBool whence resp condition = unless condition . hoistEither . Left . ErrorAt whence $ JSONValidationFailed (view responseBody resp)

assertInputIsJust :: String -> Maybe a -> Yodlee a
assertInputIsJust whence = hoistEither . note (ErrorAt whence ArgumentValidationFailed)

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
getSiteLoginForm :: CobrandSession -> SiteId -> Yodlee [SiteCredentialComponent]
getSiteLoginForm cbSess (SiteId i) = do
  let whence = "getSiteLoginForm"
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
  , ("size", siteCredItemSize . to show . to T.pack)
  , ("valueIdentifier", siteCredItemValueIdentifier)
  , ("valueMask", siteCredItemValueMask)
  , ("isEditable", siteCredItemIsEditable . to show . to T.pack . to T.toLower)
  ]

-- | This is a helper function that allows you to write a function to fill in a
-- list of 'SiteCredentialComponent's. The function inspects a
-- 'SiteCredentialComponent' and tries to return a 'T.Text' in response. If it
-- can fill in the field, it is expected to return a 'Just' value. Otherwise it
-- is expected to return a 'Nothing' value. This is a helper function in the
-- sense that it uses only operations already exposed in the public API.
fillInSiteCredentialComponents :: (SiteCredentialComponent -> Maybe T.Text) -> [SiteCredentialComponent] -> [SiteCredentialComponent]
fillInSiteCredentialComponents f rs = fillInOnce <$> rs
  where fillInOnce r = case f r of
          Nothing -> r
          Just t -> set siteCredItemValue (Just t) r

validateSiteCreds :: [SiteCredentialComponent] -> Maybe [SiteCredentialComponent]
validateSiteCreds creds = sequence . catMaybes $ go <$> creds
  where go c
          | has (siteCredItemValue . _Just) c = Just (Just c)        -- Keep
          | preview siteCredItemIsOptional c == Just True = Nothing  -- Omit
          | otherwise = Just Nothing                                 -- Fail

-- | This adds a member site account associated with a particular site.
-- refresh is initiated for the item. This API is expected to be called after
-- getting a login form for a particular site using 'getSiteLoginForm' or
-- @getSiteInfo@ or 'searchSite'.
addSiteAccount1 :: CobrandSession -> UserSession -> SiteId -> [SiteCredentialComponent] -> Yodlee SiteAccount
addSiteAccount1 cbSess user (SiteId i) siteCreds = do
  let whence = "addSiteAccount1"
  siteCredsValidated <- assertInputIsJust whence $ validateSiteCreds siteCreds
  let transformCredPiece cred name traversal = (("credentialFields[" <> view (siteCredItemIndex . to show . to C.pack) cred <> "]." <> name) :=) <$> preview traversal cred
  let transformCred cred = uncurry (transformCredPiece cred) <$> (("value", siteCredItemValue . _Just) : siteCredentialExpectedFields)
  let transformed = concatMap transformCred siteCredsValidated
  credRequestParams <- assertInputIsJust whence . sequence $ transformed
  let requestParams = [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
                      , "userSessionToken" := view (_UserSession . userSessionToken) user
                      , "siteId" := show i
                      , "credentialFields.enclosedType" := ("com.yodlee.common.FieldInfoSingle" :: T.Text) -- XXX
                      ] <> credRequestParams
  r <- performAPIRequest whence "/jsonsdk/SiteAccountManagement/addSiteAccount1" requestParams
  assertOutputIsJust whence r $ preview (responseBody . _Value . to SiteAccount) r
