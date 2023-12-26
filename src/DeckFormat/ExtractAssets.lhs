> {-# LANGUAGE ScopedTypeVariables #-}

This Literate Haskell file demonstrates recursively extracting `FilePath`s associated with a record field name ending in `Asset` from any arbitrary tree,
so long as those data types are instances of `Data` and `Typeable`.

I decided to make this module a Literate Haskell file because I had a hard time arriving at a solution, even though it seemed like it should be simple, and I wanted to share my solution with others.

There may be a faster solution using `GHC.Generics`, but I was very pleased with this solution and to get it behind me. Specifically, I believe `Data.Typeable`,
`Data.Data`, all work at runtime, rather than compmile time, which I think has potential to be inefficient.

This video on the Tweag YouTube channel, entitled "@rae: How to choose between Typeable, Data, and Generic in Haskell"
was illuminating: https://www.youtube.com/watch?v=Zj8KXD9MRA0

> module DeckFormat.ExtractAssets (extractAssets) where
> import Data.Typeable
> import Data.Data
> import Data.List (isSuffixOf)

`extractAssets` is the function which does most of the work. It's powered by `gmapQ`,
which thankfully returns a list "given in the same order as originally specified in the declaration
of the data constructors." This means we can use `zipWith` to combine the field names with the results of the recursive
calls resulting in a fairly simple function.

`gmapQ`: imagine you have a toy box with different types of toys (like blocks, cars, dolls). Now,
suppose you have a magic wand that can ask each toy what it is and then put the answers in a list.
When you use this wand (which is like gmapQ), it goes to each toy (subterm of a data structure), asks
it a question (applies a function to it), and then makes a list of all the answers. The list of answers
is in the same order as the toys were in the toy box (the same order as the data constructors in the
program).​

> {- | Extract file paths which correspond to a selector/record field name which ends in `Asset` from arbitrary datastructures recursively.
>
> Example:
> 
> >>> data ArbitraryChild = ArbitraryChild { nestAsset :: FilePath, fizz :: Int, buzz :: String } deriving (Data, Typeable)
> >>> data ArbitraryParent = ArbitraryParent { infoAsset :: FilePath, moreData :: Maybe ArbitraryChild, otherField :: String, funField :: [ArbitraryChild] } deriving (Data, Typeable)
> >>> let child1 = ArbitraryChild "val" 123 "foo"
> >>> let child2 = ArbitraryChild "val2" 456 "bar"
> >>> let parent = ArbitraryParent "val3" (Just child1) "foobar" [child2]
> >>> extractAssets parent
> [("infoAsset","val3"),("nestAsset","val"),("nestAsset","val2")]
> -}
> extractAssets :: Data a => a -> [(String, FilePath)]
> extractAssets x = case cast x of
>     Just filePath -> [("unknown", filePath)]  -- If it's a FilePath, return it with a placeholder field name
>     Nothing -> concat $ zipWith combine names (gmapQ extractAssets x)  -- Otherwise, recursively apply to each subcomponent
>   where
>     names = fields x
>     combine name results = filter (isAssetField . fst) $ map (\(fieldName, filePath) ->
>                                  if fieldName == "unknown" then (name, filePath) else (fieldName, filePath)) results

This little helper checks if a name ends with "Asset".

> isAssetField :: String -> Bool
> isAssetField name = "Asset" `isSuffixOf` name

`fields` is used to find out the names of the parts inside our data structures.

> fields :: Data a => a -> [String]
> fields x = case dataTypeRep (dataTypeOf x) of
>     AlgRep [constr] -> constrFields constr  -- Only single-constructor types are considered
>     _ -> repeat "unknown"
