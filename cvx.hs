import Data.Monoid

data Vexity = Convex | Concave | Affine | None
  deriving (Eq, Show)
data Monotonicity = Increasing | Decreasing | NonMonotone
  deriving (Eq, Show)

instance Monoid Vexity where
  mempty = Affine
  mappend Affine x = x
  mappend x Affine = x
  mappend x y
    | x == y    = x
    | otherwise = None

data FunctionProps = FunctionProps { vexity :: Vexity
                                   , monotonicity :: [Monotonicity]
                                   } 

data Function = PrimFun FunctionProps
              | CompositeFun FunctionProps [Function]

{-|
  Say we are trying to determine the vexity of the
  composite function h(g_1(x), g_2(x), ..., g_k(x)).

  compatibility takes the vexity v_i of g_i and the monotonicity
  m_i of h in the ith argument (for i = 1, ..., k) and returns
  the most specific vexity of the composite function with which
  the pair (v_i, m_i) is compatible, in accordance with the vector
  composition rule.

  For example, if g_i is convex and h is increasing in the ith argument, this
  pair is compatible with the composite function being convex (the composite may
  not ultimately be determined to be convex, if there are other
  vexity/monotonicity pairs that are incompatible [e.g., say some g_j is
  nonconvex and h is increasing in the jth argument], but at least the ith
  argument is "good")
-}

compatibility :: Vexity -> Monotonicity -> Vexity
compatibility Affine _ = Affine
compatibility Convex Increasing = Convex
compatibility Concave Decreasing = Convex
compatibility Concave Increasing = Concave
compatibility Convex Decreasing = Concave
compatibility _ _ = None

inferVexity :: Function -> Vexity
inferVexity (PrimFun props) = vexity props
inferVexity (CompositeFun props args) = mconcat $ funcVexity:argVexity
  where funcVexity = vexity props
        argVexity = zipWith compatibility (map inferVexity args) (monotonicity props)

-- some tests I was playing around with

affine = PrimFun (FunctionProps { vexity = Affine, monotonicity = [Increasing, Decreasing] })
lg = PrimFun (FunctionProps { vexity = Convex, monotonicity = [Increasing] })
compound = CompositeFun (FunctionProps { vexity = Affine, monotonicity = [Decreasing, Increasing] })
                        [lg, affine]

{-
next steps:
  1. parse user input
  2. programatically determine vexity, monotonicity where possible

todo:
  1. statically ensure that length of args == length of monotonicity list
  2. GADTs to ensure inferVexity used sensibly?
-}
