{-# LANGUAGE GeneralizedNewtypeDeriving#-}
-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Data.RetryAfter where

import Imports
import Data.Aeson
import qualified Data.Schema as Schema
import qualified Data.Swagger as Swagger
import qualified Data.Swagger.Internal.Schema as SwaggerInternal
import Data.Proxy

newtype RetryAfter = RetryAfter
  {retryAfterSeconds :: Int64}
  deriving (Eq, Show, ToJSON, FromJSON)

instance Schema.ToSchema RetryAfter where schema = Schema.genericToSchema

instance Swagger.ToSchema RetryAfter where
 declareNamedSchema = SwaggerInternal.plain . Swagger.paramSchemaToSchema

instance Swagger.ToParamSchema RetryAfter where
  toParamSchema _ = Swagger.toParamSchema (Proxy @Int64)
