package cc.hp

object tutorials {
  import scalaz._
  import Scalaz._
  import probability._

  object alarm {
    import Dist._

    // type PBool = Dist Bool
    type PBool = Dist[Boolean]

    // flp :: Float -> PBool
    // flp p = choose p True False
    def flp: ProbRep => PBool = 
      p => choose (p) (true) (false)
      // p => PBool(choose (p) (true) (false))
    
    // -- alarm network

    // -- prior burglary 1%
    // b = flp 0.01
    val b = flp (0.01)

    // -- prior earthquake 0.1%
    // e = flp 0.001
    val e = flp (0.001)
    
    // -- conditional probability of alarm given burglary and earthquake
    // a b e = case (b,e) of
    // 	 (False,False) -> flp 0.01
    //   (False,True)  -> flp 0.1
    //   (True,False)  -> flp 0.7
    //   (True,True)   -> flp 0.8

    val a:(Boolean, Boolean) => PBool = {
     	case (false, false) => flp (0.01)
      case (false, true)  => flp (0.1)
      case (true, false)  => flp (0.7)
      case (true, true)   => flp (0.8)
    }
    
    // -- conditional probability of john calling given alarm
    // j a = if a then flp 0.8 else flp 0.05
    def j(a:Boolean) = if (a) flp (0.8) else flp (0.5)
    
    // -- conditional probability of mary calling given alarm
    // m a = if a then flp 0.9 else flp 0.1
    def m(a:Boolean) = if (a) flp (0.9) else flp (0.1)
    
    // -- calculate the full joint distribution
    // data Burglary = B { 	burglary :: Bool,
    // 			earthquake :: Bool,
    // 			alarm :: Bool,
    // 			john :: Bool,
    // 			mary :: Bool }
    // 	deriving (Eq, Ord, Show)

    case class Burglary(val burglary : Boolean,
    			              val earthquake : Boolean,
    			              val alarm : Boolean,
    			              val john : Boolean,
    			              val mary : Boolean) {
      override def toString: String = this match {
        case Burglary(b, e, a, j, m) => 
          List("burglary = " + b.toString, 
               "earthquake = " + e.toString,
               "alarm = " + a.toString, 
               "john = " + j.toString, 
               "mary = " + m.toString).mkString("B{", ", ", "}")
      }
    }

    // bJoint = do b' <- b 		-- burglary
    //             e' <- e 		-- earthquake
    //             a' <- a b' e' 	-- alarm
    // 	           j' <- j a' 		-- john
    // 	           m' <- m a' 		-- mary
    // 	        return (B b' e' a' j' m')    

    val bJoint = for {b0 <- b
                      e0 <- e
                      a0 <- a(b0, e0)
                      j0 <- j(a0)
                      m0 <- m(a0)
                    } yield Burglary(b0, e0, a0, j0, m0)

    
    // -- what is the probability that mary calls given that john calls?
    // pmj = mary ?? bJoint ||| john
    val pmj = (bJoint | (_.burglary)) ? (_.mary)

    // 
    val pmbj = (bJoint | (e => e.burglary | e.mary)) ? (_.mary)

  }

  object dice {
    //hs> type Die = Int
    type Die = Int

    //hs> die :: Dist Die
    //hs> die = uniform [1..6]

    val die = uniform(1 to 6)


    //hs> twoDice :: Dist (Die,Die)
    //hs> twoDice = prod die die
    def twoDice = prod(die)(die)

    //hs> dice :: Int -> Dist [Die]
    //hs> dice 0 = certainly []
    //hs> dice n = joinWith (:) die (dice (n-1))

    def scons[A]: A => Seq[A] => Seq[A] = a => as => Seq(a) ++ as

    def dice: Int => Dist[Seq[Die]] = {
      n => n match {
        case 0 => certainly(Seq[Die]())
        case n => joinWith (scons[Die]) (die) (dice (n-1))
      }
    }
    
    //hs> twoSixes = (==[6,6]) ?? dice 2
    def twoSixes = dice(2) ? (_ == Seq(6, 6))

    // -- sixes p n computes the probability of getting
    // -- p sixes (>1,==2,...) when rolling n dice
    // --
    // sixes :: (Int -> Bool) -> Int -> Probability
    // sixes p n = (p . length . filter (==6)) ?? dice n
    def sixes[A]: Event[Die] => Int => Probability =
      p => n => dice(n) ? (a => p(a.filter(_==6).length))
    
    // plus1 x = choose 0.5 x (x+1)
       
    // droll = do
    // 	d <- die
    // 	plus1 d
       
    // g3 = (>3) ?? die
       
    // addTwo = do
    //         d1 <- die
    //         d2 <- die
    //         return (d1+d2)
  }


}
