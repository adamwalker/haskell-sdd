module SDD.SDD where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String

import SDD.C

newtype SDDManager = SDDManager {unSDDManager :: Ptr CSddManager}
newtype SDDNode    = SDDNode    {unSDDNode    :: Ptr CSddNode} deriving (Eq, Ord, Show)

managerCreate :: SDDLiteral -> Int -> IO SDDManager
managerCreate num enableGCandReorder = SDDManager <$> c_manager_create num (fromIntegral enableGCandReorder)

managerFree :: SDDManager -> IO ()
managerFree (SDDManager m) = c_manager_free m

managerTrue :: SDDManager -> IO SDDNode
managerTrue (SDDManager m) = SDDNode <$> c_manager_true m

managerFalse :: SDDManager -> IO SDDNode
managerFalse (SDDManager m) = SDDNode <$> c_manager_false m

managerLiteral :: SDDLiteral -> SDDManager -> IO SDDNode
managerLiteral lit (SDDManager m) = SDDNode <$> c_manager_literal lit m

conjoin :: SDDNode -> SDDNode -> SDDManager -> IO SDDNode
conjoin (SDDNode x) (SDDNode y) (SDDManager m) = SDDNode <$> c_conjoin x y m

disjoin :: SDDNode -> SDDNode -> SDDManager -> IO SDDNode
disjoin (SDDNode x) (SDDNode y) (SDDManager m) = SDDNode <$> c_disjoin x y m

neg :: SDDNode -> SDDManager -> IO SDDNode
neg (SDDNode x) (SDDManager m) = SDDNode <$> c_negate x m

exists :: SDDLiteral -> SDDNode -> SDDManager -> IO SDDNode
exists lit (SDDNode x) (SDDManager m) = SDDNode <$> c_exists lit x m

forall :: SDDLiteral -> SDDNode -> SDDManager -> IO SDDNode
forall lit (SDDNode x) (SDDManager m) = SDDNode <$> c_forall lit x m

nodeIsTrue :: SDDNode -> IO Int
nodeIsTrue (SDDNode x) = fromIntegral <$> c_node_is_true x

nodeIsFalse :: SDDNode -> IO Int
nodeIsFalse (SDDNode x) = fromIntegral <$> c_node_is_false x

saveAsDot :: String -> SDDNode -> IO ()
saveAsDot fName (SDDNode x) = withCAString fName $ \fName' -> c_save_as_dot fName' x

