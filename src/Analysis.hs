module Analysis where

import Data.Either (fromRight)
import Data.List (find)
import Env (allImportedEnvs, findAllGlobalSymbols)
import Info
import Json (Json (JsonMap, JsonString), printJson)
import qualified Meta
import Obj
import Prelude hiding (abs)

-- (do
--   (load "/Users/oliverfencott/Desktop/projects/carp/__dev__/test.carp")
--   (analysis/definition "/Users/oliverfencott/Desktop/projects/carp/__dev__/test.carp" 7 11)
-- )

-- TODO:
-- - Get everything, not just DEFs
-- - Flatten entire environment in order to find all symbols
-- - Rename function
-- - Move actual analysis function in Analysis.hs module

textHover :: Context -> String -> Int -> Int -> String
textHover ctx filePath line column =
  case binder of
    Nothing -> ""
    Just b ->
      let type_ = xobjTy (binderXObj b)
          typeInfo =
            maybe
              ""
              ( \t ->
                  "```carp \n"
                    ++ show t
                    ++ "\n```\n***\n"
              )
              type_
          doc = maybe "" (fromRight "" . unwrapStringXObj) (Meta.get "doc" (binderMeta b))
          json =
            JsonMap
              [ ( "contents",
                  JsonMap
                    [ ("kind", JsonString "markdown"),
                      ("value", JsonString (typeInfo ++ doc))
                    ]
                )
              ]
       in printJson json
  where
    env = contextGlobalEnv ctx
    allEnvs = allImportedEnvs env env ++ [env]
    inFile = concatMap (bindersInFile filePath . findAllGlobalSymbols) allEnvs
    onLine = bindersOnLine line inFile
    binder = findObj column onLine
    findObj :: Int -> [Binder] -> Maybe Binder
    findObj col binderList =
      if col < 0
        then Nothing
        else case binderAtColumn col binderList of
          Nothing -> findObj (col - 1) binderList
          res -> res

bindersInFile :: String -> [Binder] -> [Binder]
bindersInFile file =
  filter
    ( \binder ->
        case infoFromBinder binder of
          Nothing -> False
          Just info -> infoFile info == file
    )

bindersOnLine :: Int -> [Binder] -> [Binder]
bindersOnLine line =
  filter
    ( \v ->
        case infoFromBinder v of
          Nothing -> False
          Just i -> infoLine i == line
    )

binderAtColumn :: Int -> [Binder] -> Maybe Binder
binderAtColumn column =
  find
    ( \v ->
        case infoFromBinder v of
          Nothing -> False
          Just i -> infoColumn i == column
    )

infoFromBinder :: Binder -> Maybe Info
infoFromBinder binder = xobjInfo (binderXObj binder)
