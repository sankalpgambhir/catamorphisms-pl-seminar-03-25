

// let's define natural numbers, but as a functor, which computes natural
// numbers of the next rank / height
sealed trait NatF[A]
case class Zero[A]() extends NatF[A]
case class Succ[A](n: A) extends NatF[A]

// we can see the "rank" in the type of the objects
Zero[Nothing](): NatF[Nothing]
Succ(Zero[Nothing]()): NatF[NatF[Nothing]]

// we can show that this is indeed a functor
trait Functor[F[_]]:
  extension[A] (t: F[A]) def map[B](f: A => B): F[B]

given Functor[NatF] with
  extension[A] (t: NatF[A]) def map[B](f: A => B): NatF[B] = t match
    case Zero() => Zero()
    case Succ(n) => Succ(f(n))

// note that this was the only way to define this lifting operation, and this
// could be automatically derived (deriving Functor in Haskell). We are not
// adding new information into the system here.

// let's try to define a fixed point operator
// all we know about a fixed point is that
// F ( Fix [F] ) ⊆ Fix [F]
// so we inductively define it this way
// Note: technically this is the fixed point of Fix.apply ∘ F, 
// and not F itself, but that's a limitation of the type 
// declaration.
case class Fix[F[_]](unfix: F[Fix[F]])

// and get the class of natural numbers
type Nat = Fix[NatF]

// now all the elements are in Nat, proper!
val z: Nat = Fix[NatF](Zero[Fix[NatF]]())
val s: Nat => Nat = t => Fix[NatF](Succ(t))

// an F-Algebra is a pair (A, α: F[A] -> A) such that after applying F to A,
// creating new terms, we can reduce them back to A
// below, read: an F-Algebra with carrier A is identified by the morphism α: F[A] -> A
type Algebra[F[_], A] = F[A] => A

// the definition for a catamorphism (external, for now)
def cata[F[_]: Functor, A](alg: Algebra[F, A])(t: Fix[F]): A = alg(t.unfix.map(cata(alg)))

// NatF-Algebra (Fix[NatF], Fix.apply)
// just proving that this is indeed at least an algebra
def initialAlgebra: Algebra[NatF, Nat] = Fix.apply

// a simple algebra that prints "Hello" for each Succ and "World" for Zero
def helloAlgebra: Algebra[NatF, String] = {
  case Zero() => "World"
  case Succ(n) => s"Hello ${n}"
}

val hello = cata(helloAlgebra)

// somehow, from that bit of black magic, we've created a fold
hello(z)
hello(s(z))
hello(s(s(z)))

// what happens if our type has no base cases?
sealed trait RecF[A]
case class Rec1[A](n: A) extends RecF[A]
case class Rec2[A](n: A, n2: A) extends RecF[A]

// we can still write the fixed point
type Rec = Fix[RecF]

// we can show this is a functor
given Functor[RecF] with
  extension[A] (t: RecF[A]) def map[B](f: A => B): RecF[B] = t match
    case Rec1(n) => Rec1(f(n))
    case Rec2(n, n2) => Rec2(f(n), f(n2))

// we can define a simple algebra
val reci: Rec => Int = cata[RecF, Int] {
  case Rec1(n) => n
  case Rec2(n, n2) => n + n2
}

// but we cannot construct an application of reci...
// if you cannot construct an application for a function, the domain must be empty
// Rec = Nothing, the empty set is the least fixed point of RecF


// test with a bigger language
// to see what the trees look like

sealed trait RegexpF[A]
case class Empty[A]() extends RegexpF[A]
case class Character[A](c: Char) extends RegexpF[A]
case class Disjunction[A](r1: A, r2: A) extends RegexpF[A]
case class Concatenation[A](r1: A, r2: A) extends RegexpF[A]
case class Star[A](r: A) extends RegexpF[A]

type Regexp = Fix[RegexpF]

// trivial functor instance
given Functor[RegexpF] with
  extension[A] (t: RegexpF[A]) def map[B](f: A => B): RegexpF[B] = t match
    case Empty() => Empty()
    case Character(c) => Character(c)
    case Disjunction(r1, r2) => Disjunction(f(r1), f(r2))
    case Concatenation(r1, r2) => Concatenation(f(r1), f(r2))
    case Star(r) => Star(f(r))

// some syntactic sugar
val epsilon = Fix[RegexpF](Empty[Fix[RegexpF]]())
val char = (c: Char) => Fix[RegexpF](Character[Fix[RegexpF]](c))
val disj = (r1: Regexp, r2: Regexp) => Fix[RegexpF](Disjunction(r1, r2))
val concat = (r1: Regexp, r2: Regexp) => Fix[RegexpF](Concatenation(r1, r2))
val star = (r: Regexp) => Fix[RegexpF](Star(r))

extension (r: Regexp) 
  infix def | (r2: Regexp): Regexp = disj(r, r2)
  infix def & (r2: Regexp): Regexp = concat(r, r2)
  def * : Regexp = star(r)

// let's try to check if a regex is nullable
def nullableAlgebra: Algebra[RegexpF, Boolean] = {
  case Empty() => true
  case Character(_) => false
  case Disjunction(r1, r2) => r1 || r2
  case Concatenation(r1, r2) => r1 && r2
  case Star(_) => true
}

val nullable = cata(nullableAlgebra)


val a = char('a')
val b = char('b')
val c = char('c')

val r: Regexp = (a & b) | (c & a) | (a.*)

nullable(r)
nullable(a)
nullable(a & b)
nullable(a & b | c)
