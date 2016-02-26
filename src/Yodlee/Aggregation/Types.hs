{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Yodlee.Aggregation.Types where

import           Control.Error
import           Control.Exception.Base     (SomeException)
import           Control.Lens.Combinators
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Default
import qualified Data.Text                  as T
import qualified Data.Typeable              as Typeable
import           Network.Wreq.Lens          as HTTP
import           Network.Wreq.Session       as HTTPSess

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
    } deriving (Show, Typeable.Typeable)
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
    } deriving (Show, Typeable.Typeable)
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
    } deriving (Show, Typeable.Typeable)
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
    } deriving (Show, Typeable.Typeable)
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

-- | 'SiteAccount' is the JSON data structure returned by the Yodlee API after
-- associating a customer with a site.
newtype SiteAccount = SiteAccount Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- a 'SiteAccount'.
_SiteAccount :: Getter SiteAccount Value
_SiteAccount = to (\(SiteAccount a) -> a)

-- | 'SiteAccount' is the JSON data structure returned by the Yodlee API after
-- getting MFA response from a site.
newtype MFARefresh = MFARefresh Value deriving (Show)

-- | This is the 'Getter' that allows you to extract the JSON 'Value' inside
-- a 'SiteAccount'.
_MFARefresh :: Getter MFARefresh Value
_MFARefresh = to (\(MFARefresh a) -> a)


-- | The 'Yodlee' monad is a type returned by all endpoint functions. This /may/
-- become a @newtype@ in the future. The error type may also be more
-- descriptive, i.e. not just a 'Nothing' in case of error.
type Yodlee a = ExceptT ErrorAt (ReaderT HTTPSess.Session IO) a

-- | The 'runYodlee' function takes an action described by the 'Yodlee'
-- monad and executes it.
runYodlee :: Yodlee a -> IO (Either ErrorAt a)
runYodlee = HTTPSess.withSession . runReaderT . runExceptT

-- | The 'Error' type encapsulates every kind of error that can result from the
-- anything in the 'Yodlee' monad.
data Error
  = HTTPFetchException SomeException
    -- ^ This constructor represents errors resulting from the HTTP fetch, i.e.
    -- network errors, etc.
  | UnexpectedHTTPStatus HTTP.Status
    -- ^ This constructor represents errors resulting from an unexpected HTTP
    -- status code. The offending 'HTTP.Status' is returned.
  | JSONParseFailed CL.ByteString
    -- ^ This constructor represents a /syntax/ error in the JSON data returned
    -- from Yodlee. The 'C.ByteString' that cannot be parsed is included.
  | JSONErrorObject Value
    -- ^ This constructor represents Yodlee exceptions indicated in the JSON
    -- object returned from Yodlee. It is likely caused by a /semantic/ error in
    -- the input provided.
  | JSONValidationFailed Value
    -- ^ This constructor represents a /semantic/ error in the JSON data
    -- returned from Yodlee. The JSON 'Value' that cannot be interpreted is
    -- included. This /may/ be caused by an error at the backend of Yodlee, or
    -- Yodlee has changed its specification of objects.
  | ArgumentValidationFailed Typeable.TypeRep
    -- ^ This constructor represents an error in one or more of the arguments
    -- passed to the function. The type of the erred argument is provided.
  deriving (Show)

-- | The 'ErrorAt' type contains an 'Error' and a 'String' describing whence the
-- error comes.
data ErrorAt = ErrorAt String Error
  deriving (Show)
