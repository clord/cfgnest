module Database.Configuration.Types where

   import qualified Data.Map as Map
   
   type Key = String
   type Path = [Key]
   type Configuration a = Map.Map Key a
   
   -- We use this type for substitution
   type Data = String
   
   
   data Tree a = Tree { treeKey :: Key
                      , treeCfg :: Configuration a
                      , subtrees :: [Tree a] 
                      }
     deriving (Show, Eq)

   
