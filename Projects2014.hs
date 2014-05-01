{-# LANGUAGE GADTs #-}

data Unknown = Unknown
todo = undefined


--
-- Game DSL:
--
-- Note: Using GADTs to enable staged games
data Game s where
     Trans :: s -> [s -> s] -> (s -> Bool) -> Game s
     Stage :: Game s -> Game t -> Game (s,t)

trace :: Game s -> [s]
trace (Trans s fs p) | p s       = []
                     | otherwise = trace (Trans (foldr (.) id fs s) fs p)
trace (Stage g h) = todo

{-
Trans takes an initial state, a list of players (= state transitions), 
and a predicate to determine the end of a game.
-}



--
-- Music live coding DSL:
--
data Musicon = Motif Unknown | Rhythm Unknown | Harmony Unknown

data Music = Plain Musicon
           | Music :&: Music
           | Music :>: Music




--
-- Privacy DSL
--
type Agent = String
type Info  = String

data Target = All | Group [Agent] 

data Access = Read | ReadOnce | Send 

type Protocol = [(Access,Target)]

data Action = Post Info Protocol | Request Agent

type World = [(Agent,Action)]

data Item = I Info | K Agent Item

data Fact = Knows Agent Item

type Knowledge = [Fact]

run :: World -> Knowledge
run = todo

type Query = Knowledge -> Bool



--
-- Mutation Testing DSL
--
type Program = Unknown
type Mutant = Program
type Token = Unknown

data Mutation = Replace Token [Token]
              | Everywhere Mutation
              | DescendentOf Token Mutation 
              | UpTo Token Mutation
              | Cond (Token -> Bool) Mutation Mutation




-- 
-- "Tripper" ? testing DSL
--
type TestDescription = Unknown
type TestData = Unknown
type Res = Unknown



