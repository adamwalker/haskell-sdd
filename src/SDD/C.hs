module SDD.C where

import Foreign.C.Types
import Foreign.Ptr

data CSddManager
data CSddNode

type SDDLiteral = CLong

foreign import ccall safe "sdd_manager_create"
    c_manager_create :: SDDLiteral -> CInt -> IO (Ptr CSddManager)

foreign import ccall safe "sdd_manager_free"
    c_manager_free :: Ptr CSddManager -> IO ()

foreign import ccall safe "sdd_manager_true" 
    c_manager_true :: Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_manager_false"
    c_manager_false :: Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_manager_literal" 
    c_manager_literal :: SDDLiteral -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_conjoin"
    c_conjoin :: Ptr CSddNode -> Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_disjoin"
    c_disjoin :: Ptr CSddNode -> Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_negate"
    c_negate :: Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_exists"
    c_exists :: SDDLiteral -> Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_forall"
    c_forall :: SDDLiteral -> Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_exists_multiple"
    c_exists_multiple :: Ptr CInt -> Ptr CSddNode -> Ptr CSddManager -> IO (Ptr CSddNode)

foreign import ccall safe "sdd_node_is_true"
    c_node_is_true :: Ptr CSddNode -> IO CInt

foreign import ccall safe "sdd_node_is_false"
    c_node_is_false :: Ptr CSddNode -> IO CInt

foreign import ccall safe "sdd_save_as_dot"
    c_save_as_dot :: Ptr CChar -> Ptr CSddNode -> IO ()

