module SDD.SDD where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array

import Control.Monad

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

existsMultiple :: [Bool] -> SDDNode -> SDDManager -> IO SDDNode
existsMultiple vars (SDDNode x) (SDDManager m) = SDDNode <$> withArray (map boolToInt vars) (\arr -> c_exists_multiple arr x m)
    where
    boolToInt True  = 1
    boolToInt False = 0

nodeIsTrue :: SDDNode -> IO Int
nodeIsTrue (SDDNode x) = fromIntegral <$> c_node_is_true x

nodeIsFalse :: SDDNode -> IO Int
nodeIsFalse (SDDNode x) = fromIntegral <$> c_node_is_false x

saveAsDot :: String -> SDDNode -> IO ()
saveAsDot fName (SDDNode x) = withCAString fName $ \fName' -> c_save_as_dot fName' x

renameVariables :: SDDNode -> [SDDLiteral] -> SDDManager -> IO SDDNode
renameVariables (SDDNode x) varMap (SDDManager m) = SDDNode <$> withArray varMap (\varMap -> c_rename_variables x varMap m)

ref :: SDDNode -> SDDManager -> IO ()
ref (SDDNode x) (SDDManager m) = void $ c_sdd_ref x m

deref :: SDDNode -> SDDManager -> IO ()
deref (SDDNode x) (SDDManager m) = void $ c_sdd_deref x m

garbageCollect :: SDDManager -> IO ()
garbageCollect (SDDManager m) = c_sdd_manager_garbage_collect m

minimize :: SDDManager -> IO ()
minimize (SDDManager m) = c_sdd_manager_minimize m

