-- | This module provides functions that interfaces with the Yodlee IAV Data
-- Service REST API. This is a thin wrapper around the API. Yodlee provides two
-- types of IAV services. In the IAV data service, Yodlee uses the credentials
-- provided by the consumer to log in to the specified bank website and returns
-- all verification information found for the accounts at that site. The Yodlee
-- customer then performs the verification by using its own rules.
module Yodlee.IAV.DataService
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
         -- * Endpoints
         -- $endpoints
       , coblogin
       , register3
       , login
       , searchSiteWithFilter
       ) where

import           Data.Default
import           Yodlee.Endpoints
import           Yodlee.Types
