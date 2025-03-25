

type Algebra[F[_], A] = F[A] => A

sealed trait Fix[F[_]]:
  def unfix: F[Fix[F]]
case class In[F[_]](unfix: F[Fix[F]]) extends Fix[F]

sealed trait NatF[A]
case class Zero[A]() extends NatF[A]
case class Succ[A](n: A) extends NatF[A]

type Nat = Fix[NatF]

Zero[Nothing](): NatF[Nothing]
Succ(Zero[Nothing]()): NatF[NatF[Nothing]]

val z = In[NatF](Zero[Fix[NatF]]()): Nat
val s: Nat => Nat = t => In[NatF](Succ(t))

trait Functor[F[_]] {
  extension[A] (t: F[A]) def map[B](f: A => B): F[B]
}

given Functor[NatF] with
  extension[A] (t: NatF[A]) def map[B](f: A => B): NatF[B] = t match
    case Zero() => Zero()
    case Succ(n) => Succ(f(n))

def cata[F[_]: Functor, A](alg: Algebra[F, A])(t: Fix[F]): A = alg(t.unfix.map(cata(alg)))

def helloAlgebra: Algebra[NatF, String] = {
  case Zero() => "World"
  case Succ(n) => s"Hello ${n}"
}

val hello = cata(helloAlgebra)

hello(z)
hello(s(z))
hello(s(s(z)))

sealed trait RecF[A]
case class Rec1[A](n: A) extends RecF[A]
case class Rec2[A](n: A, n2: A) extends RecF[A]

type Rec = Fix[RecF]

given Functor[RecF] with
  extension[A] (t: RecF[A]) def map[B](f: A => B): RecF[B] = t match
    case Rec1(n) => Rec1(f(n))
    case Rec2(n, n2) => Rec2(f(n), f(n2))

val reci: Rec => Int = cata[RecF, Int] {
  case Rec1(n) => n
  case Rec2(n, n2) => n + n2
}
