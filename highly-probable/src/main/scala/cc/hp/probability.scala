package cc.hp

object probability {
  import scalaz._
  import Scalaz._

  //___hs___
  // --
  // -- Events
  // --
  // type Event a = a -> Bool

  type Event[A] = A => Boolean

  //___hs___
  // oneOf :: Eq a => [a] -> a -> bool
  // oneOf = flip elem

  def oneOf[A]: List[A] => Event[A] => Boolean = 
    ll => ea => ll.contains(ea)

  //___hs___
  // just :: Eq a => a -> Event a
  // just = oneOf . singleton

  // not sure why 
  def just[A]: A => A => Boolean =
    a => b => a == b
    // a => a => oneOf(List(a))

  //___hs___
  // -- Probabilities
  // --
  // newtype Probability = P ProbRep
  sealed abstract class P[A : Show](value:A) extends NewType[A]
  case class Probability(value:ProbRep) extends P[ProbRep](value)
  // case class Probability[A : Show](value: A) extends NewType[ProbRep]

  // type ProbRep = Float
  type ProbRep = Double

  //___hs___
  // precision :: Int
  // precision = 1
  val precision = 1

  //___hs___
  // showPfix :: ProbRep -> String
  // showPfix f | precision==0 = showR 3 (round (f*100))++"%"
  //            | otherwise    = showR (4+precision) (fromIntegral (round (f*100*d))/d)++"%"
  //              where d = 10^precision

  // TODO: not totally the same as hs ver
  def showPfix: ProbRep => String = {
    val d = math.pow(10, precision)
    pr => " " + math.round(pr * 100d * d)/d + "%"
  }

  //___hs___
  // -- fixed precision
  // --
  // showP :: ProbRep -> String
  // showP = showPfix

  def showP: ProbRep => String = showPfix

  //___hs___
  // instance Show Probability where
  //   show (P p) = showP p
  // import Show._

  implicit def probabilityShow: Show[Probability] = showBy(a => showPfix(a.value))

  //___hs___
  // errorMargin :: ProbRep
  // errorMargin = 0.00001
  val errorMargin: ProbRep = 0.00001

  //___hs___
  // -- Monad composition
  // --
  // --  (>@>)  binary composition
  // --  sequ   composition of a list of monadic functions
  // --
  // (>@>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
  // f >@> g = (>>= g) . f

  // both of these signature work fine:
  // def mcomp[M[_] : Monad, A, B, C]: Kleisli[M, A, B] => Kleisli[M, B, C] => A => M[C] =
  def mcomp[M[_] : Monad, A, B, C]: (A => M[B]) => (B => M[C]) => A => M[C] =
    f => g => a => kleisli(f) >=> g apply a

  // alias for mcomp
  def >@>[M[_] : Monad, A, B, C]: (A => M[B]) => (B => M[C]) => A => M[C] = mcomp

  //___hs___
  // sequ :: Monad m => [a -> m a] -> a -> m a
  // sequ = foldl (>@>) return
  // TODO



  //___hs___
  // ------------------------------------------------------------------------------
  // -- 1 DETERMINISTIC AND PROBABILISTIC VALUES
  // --
  // --   Dist     probability disribution
  // --   Spread   functions to convert a list of values into a distribution
  // ------------------------------------------------------------------------------
  //
  // --
  // -- Distributions
  // --

  //___hs___
  // newtype Dist a = D {unD :: [(a,ProbRep)]}

  //___hs___
  // type Dist[A] = List[(A, ProbRep)]
  // case class Dist[A](value:List[(A, ProbRep)])
  // class Dist[A](value: (A, ProbRep)*) extends NewType[Seq[(A, ProbRep)]]


  class Dist[A](val value:(A, ProbRep)*) {
    def events: Seq[A] = value map (a => a._1)

    def | : Event[A] => Dist[A] = 
      p => filterD(p)(this)

    def ? : Event[A] => Probability = 
      p => Probability(sumP({ for {a <- value; if p(a._1)} yield a }:_*))

    // override def toString = value
    // TODO: figure out how to mesh with ShowX..
    //   show (D []) = "Impossible"
    //   show (D xs) = concatMap (\(x,p)->showR w x++' ':showP p++"\n") (sortP (norm' xs))
    //                 where w = maximum (map (length.show.fst) xs)

    override def toString = {
      val sss = (value sortBy (_._2)) map {case (a,p) => a.toString + " => " + showPfix(p) }
      sss.mkString(" ", "\n ", "")
    }

  }

  object Dist {
    def apply[A](s: (A, ProbRep)*):Dist[A] = new Dist[A](s:_*)
    implicit def wrapDist[A](s: (A, ProbRep)*):Dist[A] = new Dist[A](s:_*)
    def unapply[A](d:Dist[A]): Seq[(A, ProbRep)] = d.value 
  }

  object Implicits {
    implicit def unD[A](d: Dist[A]): Seq[(A, ProbRep)] = d.value
  }
  import Implicits._

  import Bind._
  import Pure._
  import Monad._

  //___hs___
  // instance Monad Dist where
  //   return x = D [(x,1)]
  //   d >>= f  = D [(y,q*p) | (x,p) <- unD d, (y,q) <- unD (f x)]
  //   fail _   = D []

  implicit val distPure = new Pure[Dist] {
    def pure[A](a: => A) = Dist[A](a -> 1.0)
  }


  implicit val distBind = new Bind[Dist] {
    def bind[A, B](d: Dist[A], f: A => Dist[B]): Dist[B] =
      Dist({ for ((x,p) <- d; (y,q) <- f(x)) yield (y,q*p) }: _*)
  }

  implicit val distMonad: Monad[Dist] =
    monad[Dist](distBind, distPure)

  def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val value = a
  }

  implicit def distMA[A](d: Dist[A]) = ma[Dist, A](d)

  //___hs___
  // -- note: mzero is a zero for >>= and a unit for mplus
  // --
  // instance MonadPlus Dist where
  //   mzero      = D []
  //   mplus d d' | isZero d || isZero d' = mzero
  //              | otherwise             = unfoldD $ choose 0.5 d d'
  //
  // isZero :: Dist a -> Bool
  // isZero (D d) = null d

  import Monoid._

  // Defining Monoid rather than MonadPlus:
  def isZero: Dist[_] => Boolean =
    d => d.isEmpty

  implicit def distSemigroup[A]: Semigroup[Dist[A]] =
    semigroup({(a, b) => {
      if (isZero(a) || isZero(b))   zeroD[A]
      else                          unfoldD(choose (0.5) (a) (b))
    }})

  def zeroD[A] = Dist[A]()
  implicit def distZero[A]: Zero[Dist[A]] = zero(zeroD)
  implicit def distMonoid[A]: Monoid[Dist[A]] = monoid(distSemigroup, distZero)

  import Functor._

  //___hs___
  // instance Functor Dist where
  //   fmap f (D d) = D [(f x,p) | (x,p) <- d]
  implicit def distFunctor = new Functor[Dist] {
    def fmap[A, B](d: Dist[A], f: A => B) = 
      Dist({  for ((x,p) <- d) yield (f(x),p)  }:_*)
  }

  //___hs___
  // instance (Ord a,Eq a) => Eq (Dist a) where
  //   D xs == D ys = map fst (norm' xs)==map fst (norm' ys) && 
  //                    all (\((_,p),(_,q))->abs (p-q)<errorMargin) (zip xs ys)



  //___hs___
  // -- auxiliary functions for constructing and working with distributions
  // --
  // onD :: ([(a,ProbRep)] -> [(a,ProbRep)]) -> Dist a -> Dist a
  // onD f  = D . f . unD

  //___hs___
  // sizeD :: Dist a -> Int
  // sizeD = length . unD

  //___hs___
  // checkD :: Dist a -> Dist a
  // checkD (D d) | abs (1-sumP d) < errorMargin = D d
  //              | otherwise = error ("Illegal distribution: total probability = "++show (sumP d))

  def checkD[A]: Dist[A] => Dist[A] =
    d => {
      if (math.abs(1.0 - sumP(d:_*)) < errorMargin)
        d
      else
        sys.error("Illegal distribution: total probability = "+sumP(d:_*))
    }


  //___hs___
  // mkD :: [(a,ProbRep)] -> Dist a
  // mkD = checkD . D

  def mkD[A]: Seq[(A,ProbRep)] => Dist[A] =
    seq => checkD( Dist((seq:_*)) )

  //___hs___
  // sumP :: [(a,ProbRep)] -> ProbRep
  // sumP = sum . map snd

  def sumP[A](ps: (A, ProbRep)*): ProbRep =
    (ps map (_._2)).sum

  //___hs^
  // sortP :: [(a,ProbRep)] -> [(a,ProbRep)]
  // sortP = sortBy (\x y->compare (snd y) (snd x))


  // -- normalization = grouping
  // --
  // normBy ::  Ord a => (a -> a -> Bool) ->  Dist a -> Dist a
  // normBy f = onD $ accumBy f . sort

  // accumBy :: Num b => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
  // accumBy f ((x,p):ys@((y,q):xs)) | f x y     = accumBy f ((x,p+q):xs)
  //                                 | otherwise = (x,p):accumBy f ys
  // accumBy _ xs = xs

  // norm ::  Ord a => Dist a -> Dist a
  // norm = normBy (==)

  // norm' :: Ord a => [(a,ProbRep)] -> [(a,ProbRep)]
  // norm' = accumBy (==) . sort


  // -- pretty printing
  // --
  // instance (Ord a,Show a) => Show (Dist a) where
  //   show (D []) = "Impossible"
  //   show (D xs) = concatMap (\(x,p)->showR w x++' ':showP p++"\n") (sortP (norm' xs))
  //                 where w = maximum (map (length.show.fst) xs)
  //___hs$

  // TODO:

  implicit def DistShow[A]: Show[Dist[A]] = shows({d:Dist[A] => {
    d.value.mkString("\n", "\n", "\n")
  }})

  // --
  // -- Operations on distributions
  // --

  // -- product of independent distributions
  // --
  // joinWith :: (a -> b -> c) -> Dist a -> Dist b -> Dist c
  // joinWith f (D d) (D d') = D [ (f x y,p*q) | (x,p) <- d, (y,q) <- d']
  def joinWith[A, B, C]: (A => B => C) => Dist[A] => Dist[B] => Dist[C] =
    f => d1 => d2 => Dist({ 
      for ((x,p) <- d1; (y,q) <- d2) yield (f(x)(y), p*q) }: _*)


  // prod :: Dist a -> Dist b -> Dist (a,b)
  // prod = joinWith (,)
  def prod[A, B]: Dist[A] => Dist[B] => Dist[(A, B)] = 
    a => b => joinWith (×[A, B]) (a) (b)

  // -- distribution generators
  // --
  // type Spread a = [a] -> Dist a
  type Spread[A] = (A*) => Dist[A]

  // certainly :: Trans a
  // certainly = return
  def certainly[A]: Trans[A] = 
    a => distMonad.pure(a)

  // impossible :: Dist a
  // impossible = mzero
  def impossible[A] = zeroD[A]

  // choose :: ProbRep -> a -> a -> Dist a
  // choose p x y = enum [p,1-p] [x,y]
  def choose[A]: ProbRep => A => A => Dist[A] =
    p => x => y => enum(p, 1-p)(x, y)


  // enum :: [ProbRep] -> Spread a
  // enum ps xs = mkD $ zip xs ps
  def enum[A]: (ProbRep*) => (A*) => Dist[A] =
    ps => xs => mkD(xs.zip(ps))


  // enumPC :: [ProbRep] -> Spread a
  // enumPC ps = enum (map (/100) ps)

  // relative :: [Int] -> Spread a
  // relative ns = enum (map (\n->fromIntegral n/fromIntegral (sum ns)) ns)

  // shape :: (Float -> Float) -> Spread a
  // shape _ [] = impossible
  // shape f xs = scale (zip xs ps)
  //              where incr = 1 / fromIntegral ((length xs) - 1)
  //                    ps = map f (iterate (+incr) 0)

  import EphemeralStream._

  def shape[A]: (ProbRep => ProbRep) => (Seq[A] => Dist[A]) = 
    f => as => as match {
      case Nil => impossible[A]
      case xs  => {
        val incr = 1d / (xs.length - 1)
        lazy val ps = (iterate (0d) (_ + incr)) map f
        scale((xs zip ps):_*)
    }
  }

  // linear :: Float -> Spread a
  // linear c = shape (c*)

  // uniform :: Spread a
  // uniform = shape (const 1)
  def uniform[A]: (Seq[A] => Dist[A]) = 
    as => shape (_ => 1d) (as)

  // negexp :: Spread a
  // negexp = shape (\x -> exp (-x))

  // normal :: Spread a
  // normal = shape (normalCurve 0.5 0.5)

  // normalCurve :: Float -> Float -> Float -> Float
  // normalCurve mean stddev x = 1 / sqrt (2 * pi) * exp (-1/2 * u^2)
  // 	where u = (x - mean) / stddev


  // -- extracting and mapping the domain of a distribution
  // --
  // extract :: Dist a -> [a]
  // extract = map fst . unD

  // mapD :: (a -> b) -> Dist a -> Dist b
  // mapD = fmap


  // -- unfold a distribution of distributions into one distribution
  // --
  // unfoldD :: Dist (Dist a) -> Dist a
  // unfoldD (D d) = D [ (x,p*q) | (d',q) <- d, (x,p) <- unD d' ]
  def unfoldD[A]: Dist[Dist[A]] => Dist[A] =
    dd => Dist({ 
      for ((dp, q) <- dd; (x,p) <- dp) yield (x, p*q) }: _*)


  // -- conditional distribution
  // --
  // cond :: Dist Bool -> Dist a -> Dist a -> Dist a
  // cond b d d' = unfoldD $ choose p d d'
  //               where P p = truth b

  // truth :: Dist Bool -> Probability
  // truth (D ((b,p):_)) = P (if b then p else 1-p)


  // -- conditional probability
  // --
  // (|||) :: Dist a -> Event a -> Dist a
  // (|||) = flip filterD
  
  // Moved to method on Dist class
  // def |||[A] : Dist[A] => Event[A] => Dist[A] = d => p => filterD(p)(d)

  // -- filtering distributions
  // --
  // data Select a = Case a | Other
  //                 deriving (Eq,Ord,Show)

  // above :: Ord a => ProbRep -> Dist a -> Dist (Select a)
  // above p (D d) = D (map (\(x,q)->(Case x,q)) d1++[(Other,sumP d2)])
  //                 where (d1,d2) = span (\(_,q)->q>=p) (sortP (norm' d))

  // scale :: [(a,ProbRep)] -> Dist a
  // scale xs = D (map (\(x,p)->(x,p/q)) xs)
  //            where q = sumP xs
  def scale[A]: ((A, ProbRep)*) => Dist[A] =
    xs => Dist({ 
      xs map {case (x,p) => (x, p/sumP(xs:_*))} }:_*)



  // filterD :: (a -> Bool) -> Dist a -> Dist a
  // filterD p = scale . filter (p . fst) . unD
  def filterD[A]: Event[A] => Dist[A] => Dist[A] = 
    p => d => 
      scale({ for {a <- d; if p(a._1)} yield a }:_*)


  // -- selecting from distributions
  // --
  // selectP :: Dist a -> ProbRep -> a
  // selectP (D d) p = scanP p d

  // scanP :: ProbRep -> [(a,ProbRep)] -> a
  // scanP p ((x,q):ps) | p<=q || null ps = x
  //                    | otherwise       = scanP (p-q) ps

  // infix 8 ??

  // (??) :: Event a -> Dist a -> Probability
  // (??) p = P . sumP . filter (p . fst) . unD

  // -- (??) p d = P . sumP . filter (p . fst) . unD d

  // TODO: not really working the way I want...
  def queryD[A]: (Event[A], Dist[A]) => Probability = 
    (p, d) => 
      Probability(sumP({ for {a <- d; if p(a._1)} yield a }:_*))

  // -- TO DO: generalize Float to arbitrary Num type
  // --
  // class ToFloat a where
  //   toFloat :: a -> Float

  // instance ToFloat Float   where toFloat = id
  // instance ToFloat Int     where toFloat = fromIntegral
  // instance ToFloat Integer where toFloat = fromIntegral

  // class FromFloat a where
  //   fromFloat :: Float -> a

  // instance FromFloat Float   where fromFloat = id
  // instance FromFloat Int     where fromFloat = round
  // instance FromFloat Integer where fromFloat = round

  // -- expected :: ToFloat a => Dist a -> Float
  // -- expected = sum . map (\(x,p)->toFloat x*p) . unD

  // class Expected a where
  //   expected :: a -> Float

  // -- instance ToFloat a => Expected a where
  // --   expected = toFloat
  // instance Expected Float   where expected = id
  // instance Expected Int     where expected = toFloat
  // instance Expected Integer where expected = toFloat

  // instance Expected a => Expected [a] where
  //   expected xs = sum (map expected xs) / toFloat (length xs)

  // instance Expected a => Expected (Dist a) where
  //   expected = sum . map (\(x,p)->expected x*p) . unD

  // instance Expected a => Expected (IO a) where
  //   expected r = expected (unsafePerformIO r)


  // -- statistical analyses
  // --
  // variance :: Expected a => Dist a -> Float
  // variance d@(D ps) = sum $ map (\(x,p)->p*sqr (expected x - ex)) ps
  //    where sqr x = x * x
  //          ex    = expected d

  // stddev :: Expected a => Dist a -> Float
  // stddev = sqrt . variance



  // ------------------------------------------------------------------------------
  // -- 2 RANDOMIZED VALUES
  // --
  // --   R         random value
  // --   RDist     random distribution
  // ------------------------------------------------------------------------------


  // --
  // -- Random values
  // --
  // type R a = IO a

  // printR :: Show a => R a -> R ()
  // printR = (>>= print)

  // instance Show (IO a) where
  //   show _ = ""

  // pick :: Dist a -> R a
  // -- pick d = do {p <- Random.randomRIO (0,1); return (selectP p d)}
  // pick d = Random.randomRIO (0,1) >>= return . selectP d


  // --
  // -- Randomized distributions
  // --
  // type RDist a = R (Dist a)

  // rAbove :: Ord a => ProbRep -> RDist a -> RDist (Select a)
  // rAbove p rd = do D d <- rd
  //                  let (d1,d2) = span (\(_,q)->q>=p) (sortP (norm' d))
  //                  return (D (map (\(x,q)->(Case x,q)) d1++[(Other,sumP d2)]))



  // ------------------------------------------------------------------------------
  // -- 3 DETERMINISTIC AND PROBABILISTIC GENERATORS
  // --
  // --   Change    deterministic generator
  // --   Trans     probabilistic generator
  // --   SpreadC   functions to convert a list of changes into a transition
  // --   SpreadT   functions to convert a list of transitions into a transition
  // ------------------------------------------------------------------------------

  // --
  // -- transitions
  // --
  // type Change a = a -> a

  // type Trans a = a -> Dist a
  type Trans[A] = A => Dist[A]

  // idT :: Trans a
  // idT = certainlyT id


  // -- mapT maps a change function to the result of a transformation
  // -- (mapT is somehow a lifted form of mapD)
  // -- The restricted type of f results from the fact that the
  // -- argument to t cannot be changed to b in the result Trans type.
  // --
  // mapT :: Change a -> Trans a -> Trans a
  // mapT f t = mapD f . t


  // -- unfold a distribution of transitions into one transition
  // --
  // --   NOTE: The argument transitions must be independent
  // --
  // unfoldT :: Dist (Trans a) -> Trans a
  // unfoldT (D d) x = D [ (y,p*q) | (f,p) <- d, (y,q) <- unD (f x) ]


  // -- spreading changes into transitions
  // --
  // type SpreadC a = [Change a] -> Trans a

  // certainlyT :: Change a -> Trans a
  // certainlyT f = certainly . f
  // -- certainlyT = (certainly .)
  // -- certainlyT = maybeC 1

  // maybeT :: ProbRep -> Change a -> Trans a
  // maybeT p f = enumT [p,1-p] [f,id]

  // liftC :: Spread a -> [Change a] -> Trans a
  // liftC s cs x = s [f x | f <- cs]
  // -- liftC s cs x = s $ map ($ x) cs

  // uniformT  = liftC uniform
  // normalT   = liftC normal
  // linearT c = liftC (linear c)
  // enumT xs  = liftC (enum xs)


  // -- spreading transitions into transitions
  // --
  // type SpreadT a = [Trans a] -> Trans a

  // liftT :: Spread (Trans a) -> [Trans a] -> Trans a
  // liftT s = unfoldT . s

  // uniformTT  = liftT uniform
  // normalTT   = liftT normal
  // linearTT c = liftT (linear c)
  // enumTT xs  = liftT (enum xs)



  // ------------------------------------------------------------------------------
  // -- 4 RANDOMIZED GENERATORS
  // --
  // --   RChange   random change
  // --   RTrans    random transition
  // ------------------------------------------------------------------------------


  // --
  // -- Randomized changes
  // --
  // type RChange a = a -> R a

  // random :: Trans a -> RChange a
  // random t = pick . t
  // -- random = (pick .)


  // --
  // -- Randomized transitions
  // --
  // type RTrans a = a -> RDist a
  // type ApproxDist a = R [a]


  // -- rDist converts a list of randomly generated values into
  // --       a distribution by taking equal weights for all values
  // --
  // rDist :: Ord a => [R a] -> RDist a
  // rDist = fmap (norm . uniform) . sequence



  // ------------------------------------------------------------------------------
  // -- 5 ITERATION AND SIMULATION
  // --
  // -- Iterate   class defining *.
  // -- Sim       class defining ~.
  // ------------------------------------------------------------------------------

  // {-

  // Naming convention:

  // *   takes n :: Int and a generator and iterates the generator n times
  // .   produces a single result
  // ..  produces a trace
  // ~   takes k :: Int [and n :: Int] and a generator and simulates
  //     the [n-fold repetition of the] generator k times

  // n *.  t   iterates t and produces a distribution
  // n *.. t   iterates t and produces a trace

  // k     ~.  t   simulates t and produces a distribution
  // (k,n) ~*. t   simulates the n-fold repetition of t and produces a distribution
  // (k,n) ~.. t   simulates the n-fold repetition of t and produces a trace

  // -}

  // -- Iteration captures three iteration strategies:
  // -- iter builds an n-fold composition of a (randomized) transition
  // -- while and until implement conditional repetitions
  // --
  // -- The class Iterate allows the overloading of iteration for different
  // -- kinds of generators, namely transitions and random changes:
  // --
  // --   Trans   a = a -> Dist a    ==>   c = Dist
  // --   RChange a = a -> R a       ==>   c = R = IO
  // --
  // class Iterate c where
  //   (*.)  :: Int -> (a -> c a) -> (a -> c a)
  //   while :: (a -> Bool) -> (a -> c a) -> (a -> c a)
  //   until :: (a -> Bool) -> (a -> c a) -> (a -> c a)
  //   until p = while (not.p)

  // infix 8 *.

  // -- iteration of transitions
  // --
  // instance Iterate Dist where
  //   n *. t = head . (n *.. t)
  //   while p t x = if p x then t x >>= while p t else certainly x

  // -- iteration of random changes
  // --
  // instance Iterate IO where
  //   n *. r = (>>= return . head) . rWalk n r
  //   while p t x = do {l <- t x; if p l then while p t l else return l}



  // -- Simulation means to repeat a random chage many times and
  // -- to accumulate all results into a distribution. Therefore,
  // -- simulation can be regarded as an approximation of distributions
  // -- through randomization.
  // --
  // -- The Sim class contains two functions:
  // --
  // --   ~.   returns the final randomized transition
  // --   ~..  returns the whole trace
  // --
  // -- The Sim class allows the overloading of simulation for different
  // -- kinds of generators, namely transitions and random changes:
  // --
  // --   Trans   a = a -> Dist a   ==>   c = Dist
  // --   RChange a = a -> R a      ==>   c = R = IO
  // --
  // class Sim c where
  //   (~.)  :: Ord a => Int       -> (a -> c a) -> RTrans a
  //   (~..) :: Ord a => (Int,Int) -> (a -> c a) -> RExpand a
  //   (~*.) :: Ord a => (Int,Int) -> (a -> c a) -> RTrans a

  // infix 6 ~.
  // infix 6 ~..

  // -- simulation for transitions
  // --
  // instance Sim Dist where
  //   (~.)  x = (~.)  x . random
  //   (~..) x = (~..) x . random
  //   (~*.) x = (~*.) x . random


  // -- simulation for random changes
  // --
  // instance Sim IO where
  //   (~.)     n  t = rDist . replicate n . t
  //   (~..) (k,n) t = mergeTraces . replicate k . rWalk n t
  //   (~*.) (k,n) t = k ~. n *. t

  // infix 8 ~*.

  // --(~*.) :: (Iterate c,Sim c,Ord a) => (Int,Int) -> (a -> c a) -> RTrans a
  // --(k,n) ~*. t =


  // ------------------------------------------------------------------------------
  // -- 7 TRACING
  // --
  // --   (R)Trace
  // --   (R)Space
  // --   (R)Walk
  // --   (R)Expand
  // ------------------------------------------------------------------------------

  // type Trace a  = [a]
  // type Space a  = Trace (Dist a)
  // type Walk a   = a -> Trace a
  // type Expand a = a -> Space a


  // -- >>: composes the result of a transition with a space
  // -- (transition is composed on the left)
  // --
  // -- (a -> m a) -> (a -> [m a]) -> (a -> [m a])
  // (>>:) :: Trans a -> Expand a -> Expand a
  // f >>: g = \x -> let ds@(D d:_)=g x in
  //                     D [ (z,p*q) | (y,p) <- d, (z,q) <- unD (f y)]:ds

  // infix 6 >>:

  // -- walk is a bounded version of the predefined function iterate
  // --
  // walk :: Int -> Change a -> Walk a
  // walk n f = take n . iterate f

  // -- *.. is identical to *., but returns the list of all intermediate
  // --     distributions
  // --
  // (*..) :: Int -> Trans a -> Expand a
  // 0 *.. _ = singleton . certainly
  // 1 *.. t = singleton . t
  // n *.. t = t >>: (n-1) *.. t

  // infix 8 *..


  // type RTrace a  = R (Trace a)
  // type RSpace a  = R (Space a)
  // type RWalk a   = a -> RTrace a
  // type RExpand a = a -> RSpace a

  // --          (a -> m a) -> (a -> m [a]) -> (a -> m [a])
  // composelR :: RChange a -> RWalk a -> RWalk a
  // composelR f g x = do {rs@(r:_) <- g x; s <- f r; return (s:rs)}


  // -- rWalk computes a list of values by
  // --       randomly selecting one value from a distribution in each step.
  // --
  // rWalk :: Int -> RChange a -> RWalk a
  // rWalk 0 _ = return . singleton
  // rWalk 1 t = (>>= return . singleton) . t
  // rWalk n t = composelR t (rWalk (n-1) t)


  // -- mergeTraces converts a list of RTraces, into a list of randomized
  // --             distributions, i.e., an RSpace, by creating a randomized
  // --             distribution for each list position across all traces
  // --
  // mergeTraces :: Ord a => [RTrace a] -> RSpace a
  // mergeTraces = fmap (zipListWith (norm . uniform)) . sequence
  //               where
  //                 zipListWith :: ([a] -> b) -> [[a]] -> [b]
  //                 zipListWith f = map f . transpose



}
