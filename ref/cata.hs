
data NatF a = Zero | Succ a

instance Functor NatF where
  fmap f Zero = Zero
  fmap f (Succ a) = Succ (f a)

zeroF = Zero

oneF = Succ zeroF

twoF = Succ oneF

data NatH = ZeroH | SuccH NatH

twoH = SuccH (SuccH ZeroH)

newtype Fix f = In { unfix :: f (Fix f) }

type Nat = Fix NatF

z = In zeroF
s = In . Succ

one = s z
two = s (s z)

type Algebra f a  = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

initialAlgebra :: Algebra NatF (Fix NatF)
initialAlgebra = In

helloAlgebra :: Algebra NatF String
helloAlgebra Zero = "World"
helloAlgebra (Succ a) = "Hello " ++ a

hello :: Nat -> String
hello = cata helloAlgebra

data RecF a = Rec1 a | Rec2 a a

instance Functor RecF where
  fmap f (Rec1 a) = Rec1 (f a)
  fmap f (Rec2 a b) = Rec2 (f a) (f b)

rec1F :: RecF Integer
rec1F = Rec1 0
rec2F :: RecF (RecF Integer)
rec2F = Rec2 (Rec1 0) (Rec1 1)

type Rec = Fix RecF

sumAlgebra :: Algebra RecF Integer
sumAlgebra (Rec1 a) = a
sumAlgebra (Rec2 a b) = a + b

sum :: Rec -> Integer
sum = cata sumAlgebra

data RegexpF a = 
    Empty 
  | Character Char 
  | Disjunction a a 
  | Concatenation a a 
  | Star a
  deriving (Eq, Show, Functor)

type Regexp = Fix RegexpF

nullableAlgebra :: Algebra RegexpF Bool
nullableAlgebra Empty = True
nullableAlgebra (Character _) = False
nullableAlgebra (Disjunction r1 r2) = r1 || r2
nullableAlgebra (Concatenation r1 r2) = r1 && r2
nullableAlgebra (Star _) = True

nullable :: Regexp -> Bool
nullable = cata nullableAlgebra

empty = In Empty
char c = In (Character c)
disj r1 r2 = In (Disjunction r1 r2)
conc r1 r2 = In (Concatenation r1 r2)
star r = In (Star r)

-- examples
a = char 'a'
b = char 'b'
c = char 'c'
r = (a `conc` b) `disj` (c `conc` a) `disj` star a

nr = nullable r
na = nullable a
nab = nullable (a `conc` b)
nabc = nullable (a `conc` b `disj` c)


main :: IO ()
main = putStrLn $ hello two
