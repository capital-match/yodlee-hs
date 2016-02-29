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
       , SiteAccount
       , _SiteAccount
       , MFARefresh
       , _MFARefresh
         -- * Endpoints
         -- $endpoints
       , coblogin
       , register3
       , login
       , searchSite
       , getSiteLoginForm
       , addSiteAccount1
       , getMFAResponseForSite
         -- * Helper functions
       , fillInSiteCredentialComponents
  ) where

import           Data.Default
import           Yodlee.Endpoints
import           Yodlee.Types


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

-- $endpoint
-- Those functions correspond to the identically named Yodlee Aggregation REST
-- APIs. Some functions have a number following them. I don't know why.
