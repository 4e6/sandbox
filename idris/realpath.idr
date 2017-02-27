||| A prototype of unix `realpath` command. Normalize path by removing
||| intermediate self and parent (`.` and `..`) references. `PathEvent` keeps
||| track of path correctness.
module Realpath

import Data.Vect

%default total

data Path = Root | PathCons Path String

Eq Path where
    Root           == Root           = True
    Root           == (PathCons _ _) = False
    (PathCons x z) == Root           = False
    (PathCons x z) == (PathCons y w) = x == y && z == w

||| Proof of path correctness
data PathEvent : Path -> Type where
  MkRoot : PathEvent Root
  Add : (segment : String) -> (path : Path) -> PathEvent (PathCons path segment)
  Self : (action : PathEvent path) -> PathEvent path
  Parent : (action : PathEvent (PathCons parent segment)) -> PathEvent parent

data PathToken = RawElem String | RawParent | RawSelf | RawRoot

mkToken : String -> PathToken
mkToken ".." = RawParent
mkToken "."  = RawSelf
mkToken ""   = RawRoot
mkToken s    = RawElem s

||| Split input string into raw segments
tokenize : String -> (n ** Vect n PathToken)
tokenize path = (length segments ** fromList segments)
  where segments = map mkToken $ split (== '/') path

realpathRec : (acc : (path ** PathEvent path)) -> (segments : Vect n PathToken) -> Maybe (path ** PathEvent path)
realpathRec acc                    []                  = Just acc
realpathRec (path ** pf)           ((RawElem x) :: xs) = realpathRec (_ ** Add x path) xs
realpathRec (x ** pf)              (RawSelf :: xs)     = realpathRec (_ ** Self pf) xs
realpathRec (x ** pf)              (RawRoot :: xs)     = Nothing
realpathRec (Root ** pf)           (RawParent :: xs)   = Nothing
realpathRec ((PathCons _ _) ** pf) (RawParent :: xs)   = realpathRec (_ ** Parent pf) xs

realpath : (segments : Vect n PathToken) -> Maybe (path ** PathEvent path)
realpath (RawRoot :: xs) = realpathRec (_ ** MkRoot) xs
realpath _               = Nothing

||| Return normalized path and a proof
normalize : String -> Maybe (path ** PathEvent path)
normalize path = let (_ ** segments) = tokenize path
                 in realpath segments

test_1 : Bool
test_1 = map fst (normalize "/foo") == Just (PathCons Root "foo")

test_2 : Bool
test_2 = map fst (normalize "/foo/.././bar") == Just (PathCons Root "bar")

test_3 : Bool
test_3 = map fst (normalize "/../baz") == Nothing

test : Bool
test = test_1 && test_2 && test_3
