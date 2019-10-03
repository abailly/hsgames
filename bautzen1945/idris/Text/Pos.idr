-- Simple data type to keep track of character positions
-- within a text file or other text stream.
module Text.Pos

||| A position in a specific character source
||| Packrat parsers work by memoizing results of parsing rules, pointing
||| at some position in the input stream.
public export
record Pos source where
  constructor MkPos
  posFile	: source
  posLine	: Int
  posCol	: Int

export
nextPos : Pos source -> Char -> Pos source
nextPos (MkPos source line col) c =
	if c == '\n' then MkPos source (line + 1) 1
	else if c == '\t' then MkPos source line ((div (col + 8 - 1) 8) * 8 + 1)
	else MkPos source line (col + 1)

public export
Eq (Pos String) where
	(MkPos f1 l1 c1) == (MkPos f2 l2 c2) =
		f1 == f2 && l1 == l2 && c1 == c2

public export
Ord (Pos String) where
	compare (MkPos f1 l1 c1) (MkPos f2 l2 c2) = case (compare l1 l2) of
                                                   EQ => compare c1 c2
                                                   other => other

public export
Show (Pos String) where
	show (MkPos file line col) = file ++ ":" ++ show line ++ ":" ++ show col


-- showPosRel (Pos file line col) (Pos file' line' col') =
-- 	if (file == file')
-- 	then	if (line == line')
-- 		then "column " ++ show col'
-- 		else "line " ++ show line' ++ ", column " ++ show col'
-- 	else show (Pos file' line' col')
