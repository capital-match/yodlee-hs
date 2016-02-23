{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides functions that interfaces with the Yodlee Aggregation
-- REST API. This is a thin wrapper around the API.
module Yodlee.Aggregation
       (
         -- * The @'Default'@ class
         Default(..)
         -- * Data types
         -- ** Credential types
         -- $cred
       , CobrandCredential
       , cobrandUsername
       , cobrandPassword
       , UserCredential
       , userUsername
       , userPassword
         -- ** JSON @'Value'@s from the API
         -- $value
       , CobrandSession
       , _CobrandSession
       , UserSession
       , _UserSession
       , Site
       , _Site
         -- * Endpoints
       , coblogin
       , login
       , searchSite
  ) where

import           Control.Error
import           Control.Lens.Combinators
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Default
import           Data.Monoid
import qualified Data.Text              as T
import           Network.Wreq           as HTTP
import           Network.Wreq.Session   as HTTPSess

-- $cred
-- The @'CobrandCredential'@ and @'UserCredential'@ are data structures that
-- store the relevant credentials (username and password). The data constructors
-- are purposefully not exported. You are expected to construct those objects by
-- using @'def'@. You can then set the fields using the provided lenses, like
-- this:
--
-- @
-- 'set' 'cobrandUsername' "username" . 'set' 'cobrandPassword' "password" $ 'def'
-- @
--

-- | @'CobrandCredential'@ is a data structure that stores the credentials for a
-- Yodlee cobrand login.
--
-- According to Yodlee, the cobrand login is a process by
-- which a developer authenticates their application with the Yodlee API before
-- registering its user and performing other actions like adding accounts,
-- getting transactions, etc., on behalf of its user.
$(declareLenses [d|
  data CobrandCredential = CobrandCredential
    { cobrandUsername :: T.Text
    , cobrandPassword :: T.Text
    } deriving (Show)
  |])

-- | The default value for @'CobrandCredential'@ is such that both the username
-- and password are @'T.empty'@.
instance Default CobrandCredential where
  def = CobrandCredential T.empty T.empty

-- | @'UserCredential'@ is a data structure that stores user credentials.
$(declareLenses [d|
  data UserCredential = UserCredential
    { userUsername :: T.Text
    , userPassword :: T.Text
    } deriving (Show)
  |])

-- | The default value for @'UserCredential'@ is such that both the username and
-- password are @'T.empty'@.
instance Default UserCredential where
  def = UserCredential T.empty T.empty

-- $value
-- @'CobrandSession'@, @'UserSession'@, and @'Site'@ are JSON data structures
-- returned by the Yodlee API. You can access the underlying @Value@ using the
-- corresponding @Getter@. You can extract the @Value@, but you (normally)
-- cannot modify those data structures without extracting the @Value@. You also
-- cannot construct them.

-- | @'CobrandSession'@ is the JSON data structure returned by the Yodlee API
-- after a successful cobrand login.
newtype CobrandSession = CobrandSession Value deriving (Show)

-- | This is the @'Getter'@ that allows you to extract the JSON @'Value'@ inside
-- @'CobrandSession'@.
_CobrandSession :: Getter CobrandSession Value
_CobrandSession = to (\(CobrandSession a) -> a)

-- | @'UserSession'@ is the JSON data structure returned by the Yodlee API after
-- a successful user login.
newtype UserSession = UserSession Value deriving (Show)

-- | This is the @'Getter'@ that allows you to extract the JSON @'Value'@ inside
-- @'UserSession'@.
_UserSession :: Getter UserSession Value
_UserSession = to (\(UserSession a) -> a)

-- | @'UserSession'@ is the JSON data structure returned by the Yodlee API after
-- a successful site search.
newtype Site = Site Value deriving (Show)

-- | This is the @'Getter'@ that allows you to extract the JSON @'Value'@ inside
-- @'Site'@.
_Site :: Getter Site Value
_Site = to (\(Site a) -> a)

urlBase :: String
urlBase = "https://rest.developer.yodlee.com/services/srest/restserver/v1.0"

cobrandSessionToken :: Traversal' Value T.Text
cobrandSessionToken = key "cobrandConversationCredentials" . key "sessionToken" . _String

userSessionToken :: Traversal' Value T.Text
userSessionToken = key "userContext" . key "conversationCredentials" . key "sessionToken" . _String

-- | This authenticates the cobrand. Once the cobrand is authenticated a
-- @'CobrandSession'@ is created and the token within the @'CobrandSession'@
-- expires every 100 minutes. Exceptions will be thrown on network errors, but
-- @'Nothing'@ will be returned if the server did not send a valid JSON response,
-- or the JSON response does not contain the expected fields.
coblogin :: HTTPSess.Session -> CobrandCredential -> IO (Maybe CobrandSession)
coblogin session credential = runMaybeT $ do
  let url = urlBase <> "/authenticate/coblogin"
  bs <- liftIO $ HTTPSess.post session url
    [ "cobrandLogin" := view cobrandUsername credential
    , "cobrandPassword" := view cobrandPassword credential
    ]
  r <- asValue bs
  guard . isJust $ preview (responseBody . cobrandSessionToken) r
  hoistMaybe $ preview (responseBody . _Value . to CobrandSession) r

-- | This enables the consumer to log in to the application. Once the consumer
-- logs in, a @'UserSession'@ is created. It contains a token that will be used
-- in subsequently API calls. The token expires every 30 minutes.
login :: HTTPSess.Session -> CobrandSession -> UserCredential -> IO (Maybe UserSession)
login httpSess cbSess userCred = runMaybeT $ do
  let url = urlBase <> "/authenticate/login"
  bs <- liftIO $ HTTPSess.post httpSess url
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "login" := view userUsername userCred
    , "password" := view userPassword userCred
    ]
  r <- asValue bs
  guard . isJust $ preview (responseBody . userSessionToken) r
  hoistMaybe $ preview (responseBody . _Value . to UserSession) r

-- | This searches for sites. If the search string is found in the display name
-- parameter or aka parameter or keywords parameter of any @'Site'@ object, that
-- site will be included in this list of matching sites.
searchSite :: HTTPSess.Session -> CobrandSession -> UserSession -> T.Text -> IO [Site]
searchSite httpSess cbSess user site = do
  let url = urlBase <> "/jsonsdk/SiteTraversal/searchSite"
  r <- asValue =<< HTTPSess.post httpSess url
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "userSessionToken" := view (_UserSession . userSessionToken) user
    , "siteSearchString" := site
    ]
  return $ toListOf (responseBody . _Array . traverse . to Site) r
