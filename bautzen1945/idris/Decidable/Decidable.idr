module Decidable.Decidable

import Data.Rel
import Data.Fun

--------------------------------------------------------------------------------
-- Interface for decidable n-ary Relations
--------------------------------------------------------------------------------

--  Note: Implementation resolution for Decidable is likely to not work in the REPL
--        at the moment.
public export
interface Decidable (ts : Vect k Type) (p : Rel ts) where
  total decide : liftRel ts p Dec

-- 'No such variable {k506}'
--decision : Decidable ts p => (ts : Vect k Type) -> (p : Rel ts) -> liftRel ts p Dec
--decision ts p = decide {ts} {p}

public export
data Given : (a : Type) -> Dec a -> Type where
  Always : (x : a) -> Given a (Yes x)
