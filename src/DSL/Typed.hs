module DSL.Typed ( VNode
                 , vnode
                 , vundef
                 , Type(..)
                 , isSigned
                 , isUnsigned
                 , newTempNode
                 , newSignedNumber
                 , newUnsignedNumber
                 , newSignedVar
                 , newUnsignedVar
                 , newBoolVar
                 , vassign
                 , vassert
                 , newMaybeDefinedNode
                 , newDefinedNode
                 ) where
import qualified DSL.DSL as D
import           Prelude hiding (compare)

{-|

Typed wrappers around C++ operations.
Why? To avoid having to figure this kind of thing out every time:

int x;
uint y;
if (x < y) <--- "Do I use a signed or unsigned SMT comparison?"

We also want to propagate undefined behavior. Boolector operations have no
notion of undef behavior, so we have to keep track of whether or not it has
happened according to the C++ standard.

Therefore, this file is gonna do three things:
- Type inference on nodes
- C++ operations polymorphically over all possible types
- Undefined behavior propagation

-}

--
-- Verbose, typed nodes
--

-- | Verbose verification node
data VNode = VNode { vundef :: D.Node
                   , vnode  :: D.Node
                   , vtype  :: Type
                   }

-- | Type of the node. For now we just have signed and unsigned i32s,
-- but eventually we will have more integer types
data Type = Unsigned
          | Signed
          | Double
          | Bool

isSigned :: VNode -> Bool
isSigned (VNode _ _ Signed) = True
isSigned _                  = False

-- | Is the type unsigned 32?
isUnsigned :: VNode -> Bool
isUnsigned = not . isSigned

newTempNode :: D.Node -> D.Node -> Type -> VNode
newTempNode = VNode

newSignedNumber :: Integer -> D.Verif VNode
newSignedNumber amt = do
  undef <- D.i1c 0
  num <- D.i32c amt
  return $ VNode undef num Signed

newUnsignedNumber :: Integer -> D.Verif VNode
newUnsignedNumber amt = do
  undef <- D.i1c 0
  num <- D.i32c amt
  return $ VNode undef num Unsigned

newSignedVar :: String -> D.Verif VNode
newSignedVar name = do
  undef <- D.i1v $ name ++ "_undef"
  var <- D.i32v name
  return $ VNode undef var Signed

newUnsignedVar :: String -> D.Verif VNode
newUnsignedVar name = do
  undef <- D.i1v $ name ++ "_undef"
  var <- D.i32v name
  return $ VNode undef var Unsigned

newBoolVar :: String -> D.Verif VNode
newBoolVar name = do
  undef <- D.i1v $ name ++ "_undef"
  var <- D.i1v name
  return $ VNode undef var Unsigned

vassign :: VNode -> VNode -> D.Verif ()
vassign node1 node2 = do
  D.eq (vnode node1) (vnode node2) >>= D.assert
  D.eq (vundef node1) (vundef node2) >>= D.assert

vassert :: VNode -> D.Verif ()
vassert = D.assert . vnode

-- | Make a new node whose undef flag may be set, based on the parents of the node
newMaybeDefinedNode :: VNode
                    -> VNode
                    -> D.Node
                    -> Type
                    -> D.Verif VNode
newMaybeDefinedNode parent1 parent2 node ty = do
  childUndef <- D.or (vundef parent1) (vundef parent2)
  return $ VNode childUndef node ty

-- | Make a new node whose undef flag is not set
newDefinedNode :: D.Node -> Type -> D.Verif VNode
newDefinedNode node ty = do
  undefBit <- D.i1c 0
  return $ VNode undefBit node ty

