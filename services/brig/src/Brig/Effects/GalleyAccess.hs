{-# LANGUAGE TemplateHaskell #-}

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

module Brig.Effects.GalleyAccess where

import Data.Id
import Imports
import Polysemy
import Wire.API.Event.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Member

data GalleyAccess m a where
  GetTeamSndFactorPasswordChallenge :: TeamId -> GalleyAccess m FeatureStatus
  -- | Only works on 'BindingTeam's! The lisetBindingTeamMembersH'.
  GetTeamContacts :: UserId -> GalleyAccess m (Maybe TeamMemberList)
  -- | Calls 'Galley.API.getBindingTeamIdH'.
  GetTeamId :: UserId -> GalleyAccess m (Maybe TeamId)
  -- | Calls 'Galley.API.getTeamFeatureStatusH'.
  GetTeamLegalHoldStatus :: TeamId -> GalleyAccess m (WithStatus LegalholdConfig)
  -- | Tell Galley to remove a service bot from a conversation.
  RemoveBotMember ::
    UserId ->
    Maybe ConnId ->
    ConvId ->
    BotId ->
    GalleyAccess m (Maybe Event)

makeSem ''GalleyAccess
