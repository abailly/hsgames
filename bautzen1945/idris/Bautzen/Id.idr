module Bautzen.Id

import Data.String.Parser
import public Data.Vect
import Language.JSON

import JSON

%hide JSON.Parser.JSON
%default total

public export
data Id  = MkId (Vect 8 Char)

public export
defaultId : Id
defaultId = MkId $ replicate 8 '0'

export
Show Id where
  show (MkId v) = pack $ toList v

public export
makeId : String -> Either String Id
makeId s =
  fst <$> parse idParser s
  where
    idParser : Parser Id
    idParser = MkId <$> ntimes 8 alphaNum

public export
Eq Id where
  MkId v == MkId v' = v == v'

export
Ord Id where
  MkId id  `compare` MkId id' =
    id `compare` id'

public export
FromString Id where
  fromString s =
    case makeId s of
       -- TODO: this does not make much sense...
       (Left x) => defaultId
       (Right x) => x

export
Cast Id JSON where
  cast (MkId v) = JString . pack . toList $ v

export
FromJSON Id where
  fromJSON = withString "Id" $ \ s =>
     case makeId s of
         Left x => fail $ "Cannot parse Id " ++ s
         Right v => pure v
