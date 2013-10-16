This module implements a server on port 4040 that exposes various methods for getting and setting configuration keys, along with tree shape management. 

> {-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances  #-}
> module Database.Configuration.Server(server) where
> import Network.MessagePackRpc.Server
> import qualified Database.Configuration as Pure
> import Database.Configuration.Types
> import Database.Configuration.Serialization
> import Control.Concurrent.STM
> import Data.MessagePack.Assoc
> import Data.MessagePack.Object
> import qualified Data.MessagePack.Unpack as UN
> import qualified Data.MessagePack.Pack as PA
> import Control.Applicative
> import Control.Monad ()
> import Control.Monad.IO.Class
> import Control.Concurrent
> import System.Exit (ExitCode(..))
> import qualified Control.Exception as E
> import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM, )

The server must maintain atomic mutable state (the current configuration tree). 

We use transactional memory to maintain an ordering between gets and sets. Additionally, we lazily mirror the current tree to disk. 

Fetching data:

> get :: TVar [Tree Data] -> Path -> Key -> Method (Maybe Data)
> get treevar path key = ameth (Pure.get path key <$> readTVar treevar)

> has :: TVar [Tree Data] -> Path -> Key -> Method Bool
> has treevar path key = ameth (Pure.has path key <$> readTVar treevar)

> children :: TVar [Tree Data] -> Path -> Method [Key]
> children treevar path = ameth (Pure.children path <$> readTVar treevar)

> getAll :: TVar [Tree Data] -> Bool -> Path -> Method (Assoc [(Key, Data)])
> getAll treevar expand path = ameth ((Assoc . Pure.getAll expand path) <$> readTVar treevar)

We want to be able to return trees via the API. The following is a simple serialization scheme that will probably have to change. 

> treeAt :: TVar [Tree Data] -> Path -> Method Object
> treeAt treevar   [] = ameth $ do tree <- readTVar treevar
>                                  return (toObject [Pure.treeAt [] tree])
> treeAt treevar path = ameth $ do tree <- readTVar treevar
>                                  return (toObject (Pure.treeAt path tree))

For treeAt to work, we need a way to [de]serialize trees. 

> instance UN.Unpackable a => UN.Unpackable (Tree a) where
>  get = undefined
> instance PA.Packable a => PA.Packable (Tree a) where
>  from = undefined
> instance OBJECT a => OBJECT (Tree a) where
>  toObject t = ObjectMap [(toObject "children", toObject (subtrees t)), (toObject "locals", toObject (treeCfg t))]
>  fromObject _ = error "Deserialization not supported"
> instance OBJECT a => OBJECT [Tree a] where
>  toObject = ObjectMap . map treeItem
>       where treeItem t = (toObject (treeKey t), toObject t)
>  fromObject _ = error "Deserialization not supported"

Setting and removing data:

> set    :: TVar [Tree Data] -> TVar Bool -> Path -> Key -> Data -> Method ()
> set treevar dirty path key value = mutator treevar dirty $ Pure.set path key value

> del    :: TVar [Tree Data] -> TVar Bool -> Path -> Key -> Method ()
> del treevar dirty path key = mutator treevar dirty $ Pure.del path key

> delTree :: TVar [Tree Data] -> TVar Bool -> Path -> Key -> Method ()
> delTree treevar dirty path key = mutator treevar dirty $ Pure.delTree path key

Finally, we set up our server, and indicate the available named entry-points. 

> server :: String -> IO ()
> server path = do loadedTree <- readTree path
>                  tree       <- atomically (newTVar loadedTree)
>                  dirty      <- atomically (newTVar False)
>                  termcount  <- atomically (newTVar 0)
>                  tid        <- myThreadId
>                  installHandler sigINT (Catch $ handler tid termcount) Nothing
>                  installHandler sigTERM (Catch $ handler tid termcount) Nothing
>                  _          <- forkIO (backgroundSaver tree termcount dirty path)
>                  putStrLn "cfgnestd started on port 4040:"
>                  serve 4040 [ ("get"     , toMethod (get tree))
>                             , ("has"     , toMethod (has tree))
>                             , ("del"     , toMethod (del tree dirty))
>                             , ("set"     , toMethod (set tree dirty))
>                             , ("cfg"     , toMethod (getAll tree))
>                             , ("children", toMethod (children tree))
>                             , ("deltree" , toMethod (delTree tree dirty))
>                             , ("tree"    , toMethod (treeAt tree))
>                             ]
> handler :: ThreadId -> TVar Int -> IO ()
> handler pid tcount = do putStrLn "\nINTERRUPT: waiting for threads ..."
>                         atomically (modifyTVar tcount (1 +))
>                         atomically (do count <- readTVar tcount; check (count == (-1)))
>                         threadDelay 10000 -- just make sure there is time for all cleanup actions
>                         putStrLn "INTERRUPT: Main thread exiting" 
>                         E.throwTo pid (ExitFailure 1)


For saving, we want to write as soon as changes are made so that the file on disk is as fresh as possible, but we also want to rate-limit the writing so as to not saturate IO for set-heavy users, acting as a primitive form of annealing.

This background  function will write changes immediately after the set operation is performed, unless a write has happened in the recent past, in which case we wait a short time before performing the write.

Ideally, we'd set the delay between writes so that there is time to flush out the size of our tree to storage. Hence threadDelay would ideally be a function of the size of the tree.

> backgroundSaver :: TVar [Tree Data] -> TVar Int -> TVar Bool -> String -> IO ()
> backgroundSaver treevar tcount dv path = do threadDelay 2000000 -- wait 2 seconds (rate limiter)
>                                             (tree, sigs) <- atomically getTree
>                                             if sigs == 0 then writeTree path tree >> backgroundSaver treevar tcount dv path -- we'll go on forever
>                                                          else bail
>  where bail = atomically (writeTVar tcount (-1)) >> putStrLn "INTERRUPT: background thread exiting"                               
>        getTree = do isdirty <- readTVar dv
>                     count   <- readTVar tcount
>                     check (isdirty || count > 0) -- blocks here until someone sets dv==true or signals our process
>                     tree    <- readTVar treevar
>                     writeTVar dv False
>                     return (tree, count)

Generically, mutators modify the tree. This method will atomically fetch a tree from a `treevar`, apply a `Tree -> Tree` function, and update a reference to the new version (and also set a dirty bit). 

> mutator :: TVar [Tree Data] -> TVar Bool -> ([Tree Data] -> [Tree Data]) -> Method ()
> mutator treevar dirty f = ameth $ writeTVar dirty True >> modifyTVar treevar f

Most APIs want to operate in the IO context atomically, so the following wrapper is provided:

> ameth :: STM a -> MethodT IO a
> ameth = liftIO . atomically

