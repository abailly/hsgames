{-# LANGUAGE DuplicateRecordFields #-}

module Empires where

import Data.Array.IArray (Array)
import Data.Array.Unboxed (UArray)
import Data.Text (Text)

newtype Map = Map {cells :: Array (Int, Int) Cell}

data Cell = Cell
  { x :: Int,
    y :: Int,
    terrain :: CellType
  }
  deriving (Eq, Show)

data CellType
  = Sea
  | Land Terrain
  deriving (Eq, Show)

data Terrain = Terrain
  { elevation :: Int,
    landType :: LandType
  }
  deriving (Eq, Show)

data LandType = Desert | Steppe | Agricultural
  deriving (Eq, Show)

-- | A `Community` always occupies a single cell of the map.
data Community = Community
  { -- | Identifier for this community.
    communityId :: Text,
    -- | Location of this community, simple (x,y) coordinates.
    location :: Cell,
    -- | Current so-called "ultrasociality" traits of this community.
    ultrasociality :: UltraSociality,
    -- | Current military technology level of this community.
    miltech :: MilTech,
    -- | A community initially starts as autonomous in its cell.
    -- It can later expand, thus creating a new `Polity` or become part
    -- of another `Polity` through war.
    polity :: Maybe Polity
  }
  deriving (Eq, Show)

-- | A `Polity` starts when 2 or more `Community` are merged through the effect
-- of war.
data Polity = Polity
  { -- | The identitier for this `Polity`. It must be the identifier of the
    -- leading `Community`, eg. the one that started growing this polity.
    polityId :: Text,
    -- | The list of `Community` part of this `Polity`.
    extant :: [Community]
  }
  deriving (Eq, Show)

-- | `UltraSociality` is an abstract representation of "social technology" that enable
-- communities beyond relatives. The idea is that the more ultra-social trait a community
-- has, the more sophisticated and efficient its government system, which includes stratification,
-- bureaucracy, state machinery, permanent armies, taxes... but also literacy and education,
-- roads, postal systems...
newtype UltraSociality = UltraSociality {traits :: UArray Int Bool}
  deriving (Eq, Show)

-- | `MilTech` is an abstract representation of technological advances in anything related to
-- warfare: Metallurgy, weaponry, horsemanship, tactics...
newtype MilTech = MilTech {traits :: UArray Int Bool}
  deriving (Eq, Show)
