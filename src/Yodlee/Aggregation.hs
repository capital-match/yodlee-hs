{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | This module provides functions that interfaces with the Yodlee Aggregation
-- REST API. This is a thin wrapper around this API.
module Yodlee.Aggregation
       ( Default(..)
       , CobrandCredential
       , cobrandUsername
       , cobrandPassword
       , UserCredential
       , userUsername
       , userPassword
       , CobrandSession
       , _CobrandSession
       , UserSession
       , _UserSession
       , Site
       , _Site
       , coblogin
       , login
       , searchSite
  ) where

import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Default
import           Data.Monoid
import qualified Data.Text              as T
import           Network.Wreq           as HTTP
import           Network.Wreq.Session   as HTTPSess

-- | @'CobrandCredential'@ is a data structure that stores the credentials for a
-- Yodlee cobrand login. The data constructor is purposefully not exported. You
-- are expected to construct a @'CobrandCredential'@ by using @'def'@. You can
-- then set the fields using the provided lenses, namely @'cobrandUsername'@ and
-- @'cobrandPassword'@. According to Yodlee, the cobrand login is a process by
-- which a developer authenticates their application with the Yodlee API before
-- registering its user and performing other actions like adding accounts,
-- getting transactions, etc., on behalf of its user.
$(declareLenses [d|
  data CobrandCredential = CobrandCredential
    { cobrandUsername :: T.Text
    , cobrandPassword :: T.Text
    } deriving (Show)
  |])

instance Default CobrandCredential where
  def = CobrandCredential T.empty T.empty

$(declareLenses [d|
  data UserCredential = UserCredential
    { userUsername :: T.Text
    , userPassword :: T.Text
    } deriving (Show)
  |])

instance Default UserCredential where
  def = UserCredential T.empty T.empty

newtype CobrandSession = CobrandSession Value deriving (Show)

_CobrandSession :: Getter CobrandSession Value
_CobrandSession = to (\(CobrandSession a) -> a)

newtype UserSession = UserSession Value deriving (Show)

_UserSession :: Getter UserSession Value
_UserSession = to (\(UserSession a) -> a)

newtype Site = Site Value deriving (Show)

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
  guard . isJust $ r ^? responseBody . cobrandSessionToken
  hoistMaybe $ r ^? responseBody . _Value . to CobrandSession

-- | This enables the consumer to log in to the application. Once the consumer
-- logs in, a @'UserSession'@ is created and the token within the
-- @'UserSession'@ expires every 30 minutes.
login :: HTTPSess.Session -> CobrandSession -> UserCredential -> IO (Maybe UserSession)
login httpSess cbSess userCred = runMaybeT $ do
  let url = urlBase <> "/authenticate/login"
  bs <- liftIO $ HTTPSess.post httpSess url
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "login" := view userUsername userCred
    , "password" := view userPassword userCred
    ]
  r <- asValue bs
  guard . isJust $ r ^? responseBody . userSessionToken
  hoistMaybe $ r ^? responseBody . _Value . to UserSession

-- | This searches for sites. If the search string is found in the Display Name
-- parameter or AKA parameter or Keywords parameter of any @'Site'@ object, that
-- site will be included in this array of matching sites.
searchSite :: HTTPSess.Session -> CobrandSession -> UserSession -> T.Text -> IO [Site]
searchSite httpSess cbSess user site = do
  let url = urlBase <> "/jsonsdk/SiteTraversal/searchSite"
  r <- asValue =<< HTTPSess.post httpSess url
    [ "cobSessionToken" := view (_CobrandSession . cobrandSessionToken) cbSess
    , "userSessionToken" := view (_UserSession . userSessionToken) user
    , "siteSearchString" := site
    ]
  return $ toListOf (responseBody . _Array . traverse . to Site) r
