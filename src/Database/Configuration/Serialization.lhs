Versioned serialization to and from a file. 

> module Database.Configuration.Serialization(readTree, writeTree) where
> import qualified Database.Configuration as Pure
> import Database.Configuration.Types
> import System.IO()
> import Control.Monad
> import Data.Binary
> import qualified Data.Map as Map
> import System.Directory(doesFileExist)

This is a very straight-forward (i.e., primitive) serialization mechanism that simply writes out binary files containing the entire tree.

> instance (Binary a) => Binary (Tree a) where
>   put x = do put currentVersion
>              put (treeKey x)
>              put (Map.toList (treeCfg x))
>              put (subtrees x)
>   get = do ver <- getWord8 -- version (in future, case out here to support migrating from older files)
>            when (ver > currentVersion)
>             (error $ "Version " ++ show ver ++ " unsupported - server supports " ++ show currentVersion ++ " and below.")
>            key <- get
>            cfg <- get
>            sts <- get
>            return Tree { treeKey  = key
>                        , treeCfg  = Map.fromList cfg
>                        , subtrees = sts
>                        }

Now let's read and write files. If the file does not exist we'll provide an empty tree. 

> readTree :: Binary a => String -> IO [Tree a]
> readTree path = do exists <- doesFileExist path
>                    if exists
>                     then decodeFile path
>                     else return []

> writeTree :: Binary a => String -> [Tree a] -> IO ()
> writeTree = encodeFile


version of this file. In the future (when there are other versions), we'll support migrating older versions to the newest. 

> currentVersion :: Word8
> currentVersion = 1
