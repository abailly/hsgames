module Bautzen.Id

import Data.String.Parser
import public Data.Vect
import Language.JSON

%default total

public export
Id : Type
Id = Vect 8 Char

public export
defaultId : Id
defaultId = replicate 8 '0'

export
[AsString] Show Id where
  show = pack . toList

export
makeId : String -> Either String Id
makeId s =
  fst <$> parse idParser s
  where
    idParser : Parser Id
    idParser = ntimes 8 alphaNum

export
FromString Id where
  fromString s =
    case makeId s of
       -- TODO: this does not make much sense...
       (Left x) => defaultId
       (Right x) => x

export
Cast Id JSON where
  cast = JString . pack . toList
