This library provides access to a tree of dictionaries. More specific nodes override the values found in less specific nodes, enabling configuration to be overridden in sub-nodes. Values in a given dictionary can refer to other keys, which are looked up in the same manner at the same node-scope as the dictionary. 

The resulting configuration tool is surprisingly rich. This file is a literate Haskell program implementing the pure portion of the library. 

First, let's define the module and it's imports/exports:

> module Database.Configuration where
> import qualified Data.Map as Map
> import qualified Database.Configuration.Substitution as Sub
> import Database.Configuration.Types
> import Control.Monad

To manipulate our Tree in a generic way, we define a zipper. Zippers work by leaving crumbs. With this state, the zipper can get back to the original tree at any point, possibly with permutations. The crumb needs to hold all of the data a tree node can hold, plus two lists, the left group, and the right group. Using two groups maintains ordering of the tree, which is useful for equality and ordering operations -- trees constructed in the same order with the same content are equal.

> data TreeCrumb a = TreeCrumb { crumbLeft   :: [Tree a]
>                              , crumbRight  :: [Tree a]
>                              , crumbKey    :: Key
>                              , crumbConfig :: Configuration a
>                              }
>                  |   TreeTop { topLeft     :: [Tree a]
>                              , topRight    :: [Tree a]
>                              }
>   deriving (Show, Eq)

The zipper is just a focus, and a list of crumbs that tell us how we got here in case we want to go back:

> type TreeZipper a = (Tree a, [TreeCrumb a])

We need to start walking zippers, so we need a way to enter the root node. 

> start :: Key -> [Tree a] -> TreeZipper a
> start key ts = let (ls, rest) = break (keyIs key) ts 
>                    (item, rs) = extractOrCreateTree key rest
>                 in (item, [TreeTop ls rs])

Going up the tree is simple: Just take the latest crumb and assemble our new focus from that and the current focus. In the case where we're already at the top of the tree, we'll just produce the same zipper again.

> zipUp :: TreeZipper a -> TreeZipper a
> zipUp (_, []) = error "zipUp: Zipper in invalid state -- TreeTop is required (use 'start')"
> zipUp a@(_, [TreeTop _ _]) = a
> zipUp (tree, b:bs) = (Tree { treeKey  = crumbKey b
>                            , subtrees = crumbLeft b ++ tree : crumbRight b
>                            , treeCfg  = crumbConfig b
>                            }, bs)

Sometimes, we want to just roll all the way back up to the root:

> zipTree :: TreeZipper a -> [Tree a]
> zipTree (_, []) = error "zipTree: Zipper in invalid state -- TreeTop is required (use 'start')"
> zipTree (tree, [TreeTop l r]) = l ++ (tree : r)
> zipTree e = zipTree (zipUp e)

Moving down, we break the list of children trees, and put our focus as the first on the right of the break. If there is no such node, we invent one, meaning this function does not fail if nodes are not found. Because this library avoids errors where possible, it becomes possible for *any* set operation to rename the root node's key (the empty-list case of extractOrCreateTree).

> zipDown :: Key -> TreeZipper a -> TreeZipper a
> zipDown key (tree, bs) = let (ls, rest) = break (keyIs key) (subtrees tree)
>                              (item, rs) = extractOrCreateTree key rest
>                           in (item, TreeCrumb { crumbLeft   = ls
>                                               , crumbKey    = treeKey tree
>                                               , crumbConfig = treeCfg tree
>                                               , crumbRight  = rs
>                                               } : bs)

A key helper, which finds or creates a key (siblings to the right are in `snd`)

> extractOrCreateTree :: Key -> [Tree a] -> (Tree a, [Tree a])
> extractOrCreateTree key [] = (Tree key Map.empty [], [])
> extractOrCreateTree _ (a:rs) = (a, rs)

Let us define a method for moving down entire paths:

> unzipTree :: Path -> TreeZipper a -> TreeZipper a
> unzipTree qs z = foldl (flip zipDown) z qs

Now that we can zip around, we need to be able to add, edit, and delete configuration at any location. Given a key and a value, along with a position in the tree, insert the value into the configuration node at that level in the tree. This will update the value if the key already exists at the current location.

> setHere :: Key -> a -> TreeZipper a -> TreeZipper a
> setHere key val (tree, rst) = (tree { treeCfg = Map.insert key val (treeCfg tree) }, rst)
> delHere :: Key -> TreeZipper a -> TreeZipper a
> delHere key (tree, rst) = (tree { treeCfg = Map.delete key (treeCfg tree) }, rst)
> delTreeHere :: Key -> TreeZipper a -> TreeZipper a
> delTreeHere key (tree, rst) = (tree {subtrees = filter (not . keyIs key) (subtrees tree)}, rst)

Operating on an arbitrary key at some path involves moving down a path from the root node, making the desired modification, and then zipping back up to obtain the new tree. This is fairly generic, so we introduce this generic operator:

> operateAt :: Path -> (TreeZipper a -> TreeZipper a) -> [Tree a] -> [Tree a]
> operateAt [] _ = id
> operateAt (root:path) f = zipTree . f . unzipTree path . start root

Using operateAt is straight-forward, just pass it a path, a modifier, and the tree, as such:

> set :: Path -> Key -> a -> [Tree a] -> [Tree a]
> set path key = operateAt path . setHere key

Deleting keys at arbitrary paths is done in the same manner as setting, except that we invoke 'delHere'. 

> del :: Path -> Key -> [Tree a] -> [Tree a]
> del path = operateAt path . delHere

> delTree :: Path -> Key -> [Tree a] -> [Tree a]
> delTree [] key = filter (not . keyIs key)
> delTree path key = operateAt path $ delTreeHere key


Moving on to queries, a primary concern is finding all nodes of the tree that are in the scope on a given path. This implementation takes advantage of unions over the affected maps to efficiently produce a final map that contains that set.

> configurationForPath :: Path -> [Tree a] -> Configuration a
> configurationForPath [] _ = Map.empty
> configurationForPath (q:qs) ts = let ms = filter (keyIs q) ts 
>                                   in Map.unions (map (configurationForPath qs) (map subtrees ms) ++ 
>                                                  map treeCfg ms)


Since the zipper is unzipped at the right place -- and is even kept in the right order -- it's fairly easy to turn any zipper into a configuration for the path the zipper is at. This method is more efficient than the above if you already have an unzipped zipper (you only pay the overhead for combining the configuration sets):

> configurationForZipper :: TreeZipper a -> Configuration a
> configurationForZipper (_, [])    = Map.empty
> configurationForZipper (tree, bs) = Map.unions $ treeCfg tree : map crumbConfig bs

With the configuration for a given path, all we must do is perform a lookup to get the answer. Let's define a raw lookup function that does no substitution of variables:

> getRaw :: Path -> Key -> [Tree a] -> Maybe a
> getRaw path key tree = Map.lookup key (configurationForPath path tree)

And now, a version that performs a lookup with substitution of variables. The way this is implemented, we just do a `getRaw`, look for variable names, and then for each of those variable names, perform a `lookup`. If a variable is anywhere undefined, we substitute the empty string. Note that this is specialized to only work with a particular type, as substitution only works for that type. Note that we're working with Data here, not any generic a. 

> get :: Path -> Key -> [Tree Data] -> Maybe Data
> get _    _   []   = Nothing
> get path key tree = let cpath = configurationForPath path tree
>                      in do raw <- Map.lookup key cpath
>                            return $ expandRaw cpath raw

> has :: Path -> Key -> [Tree Data] -> Bool
> has _    _   []   = False
> has path key tree = Map.member key (configurationForPath path tree)

> expandRaw :: Configuration Data -> Data -> Data
> expandRaw = Sub.substitution . flip Map.lookup

Sometimes you just want to perform your own operations on the key/values at a given path. 

> getAll :: Bool -> Path -> [Tree Data] -> [(Key, Data)]
> getAll _ _ [] = []
> getAll True  path tree = liftM2 map expand Map.toList (configurationForPath path tree)
>  where expand m (k, d) = (k, expandRaw m d)
> getAll False path tree = Map.toList (configurationForPath path tree)

> treeAt :: Path -> [Tree a] -> [Tree a]
> treeAt _ [] = []
> treeAt [] ts = ts
> treeAt (root:path) ts = case filter (keyIs root) ts of
>                              []    -> []
>                              (t:_) -> treeAt path (subtrees t)

It is useful to have the set of children at a given zipper:

> childrenForZipper :: TreeZipper a -> [Key]
> childrenForZipper = map treeKey . subtrees . fst

and also the children of a given path:

> children :: Path -> [Tree a] -> [Key]
> children p = map treeKey . treeAt p


A helper that just tells us if the key for a tree is equal to some other key:

> keyIs :: Key -> Tree a -> Bool
> keyIs a b = a == treeKey b

