{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Analysis where

-- import Env (allImportedEnvs, findAllSymbols, lookupChildren)
-- import Env (allImportedEnvs, findAllSymbols, lookupChildren)

import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (find)
import Env (allImportedEnvs, findAllForms, findAllSymbols)
import Info
import Json (Json (JsonMap, JsonString), printJson)
import Meta
import Obj
  ( Binder (binderMeta, binderXObj),
    Context (contextGlobalEnv),
    Obj (Interface, Lst, Mod),
    TypeEnv (getTypeEnv),
    XObj (XObj, xobjInfo, xobjTy),
    getName,
    unwrapStringXObj,
  )
-- import Obj
-- import Types
import Types (showMaybeTy)
import Util (joinLines, joinWith)
import Prelude hiding (abs)

-- (do
--   (load "/Users/oliverfencott/Desktop/projects/carp/__dev__/test.carp")
--   (analysis/definition "/Users/oliverfencott/Desktop/projects/carp/__dev__/test.carp" 7 11)
-- )

-- TODO:
-- - Get everything, not just DEFs
-- - Flatten entire environment in order to find all symbols

textHover :: Context -> String -> Int -> Int -> IO ()
textHover ctx filePath line column =
  case maybeBinder of
    Nothing -> pure ()
    Just binder ->
      putStrLn (printJson json)
      where
        type_ = maybe "no type found" show (xobjTy (binderXObj binder))
        file = case xobjInfo (binderXObj binder) of
          Nothing -> ""
          Just i ->
            "\n***\n*"
              ++ infoFile i
              ++ ":"
              ++ show (infoLine i)
              ++ ":"
              ++ show (infoColumn i)
              ++ "*"
        doc = maybe "" (fromRight "" . unwrapStringXObj) (Meta.get "doc" (binderMeta binder))
        name = getName (binderXObj binder)
        json =
          JsonMap
            [ ( "contents",
                JsonMap
                  [ ("kind", JsonString "markdown"),
                    ( "value",
                      JsonString
                        ( "\n__" ++ name ++ "__ `" ++ type_ ++ "`\n"
                            ++ "\n***\n"
                            ++ doc
                            ++ file
                            ++ "\n***\n"
                        )
                    )
                  ]
              )
            ]
  where
    env = contextGlobalEnv ctx
    allEnvs = allImportedEnvs env env ++ [env]
    -- inFile =
    --   env
    --     & findAllForms
    --     & bindersInFile filePath
    inFile =
      concatMap findAllSymbols allEnvs
        & bindersInFile filePath
    onLine = bindersOnLine line inFile
    maybeBinder = findObj column onLine
    findObj :: Int -> [Binder] -> Maybe Binder
    findObj col xObjList =
      if col < 0
        then Nothing
        else case binderAtColumn col xObjList of
          Nothing -> findObj (col - 1) xObjList
          res -> res

showBinderInMarkdown :: Binder -> String
showBinderInMarkdown xObj = showBinderIndented 0 (getName (binderXObj xObj), binderXObj xObj)

showBinderIndented :: Int -> (String, XObj) -> String
showBinderIndented indent (name, XObj (Mod env tenv) _ _) =
  replicate indent ' ' ++ name ++ " = {\n\n"
    ++ showBindings env
    ++ "\n\n"
    ++ showBindings (getTypeEnv tenv)
    ++ "\n\n"
    ++ replicate indent ' '
    ++ "}"
  where
    showBindings e =
      joinLines $
        filter
          (/= "")
          ( map
              ( showBinderIndented (indent + 2) . (\x -> (getName (binderXObj x), binderXObj x))
              )
              (findAllForms e)
          )
showBinderIndented indent (name, XObj (Lst [XObj (Interface t paths) _ _, _]) _ _) =
  replicate indent ' ' ++ name ++ ": " ++ show t ++ " = {\n\n  "
    ++ joinWith "\n\n  " (map show paths)
    ++ "\n\n"
    ++ replicate indent ' '
    ++ "}"
showBinderIndented indent (name, xobj) =
  replicate indent ' ' ++ name
    ++ ": "
    ++ "```"
    ++ showMaybeTy (xobjTy xobj)
    ++ " ```"

bindersInFile :: String -> [Binder] -> [Binder]
bindersInFile file =
  filter
    ( \binder ->
        case xobjInfo (binderXObj binder) of
          Nothing -> False
          Just info -> infoFile info == file
    )

bindersOnLine :: Int -> [Binder] -> [Binder]
bindersOnLine line =
  filter
    ( \v ->
        case xobjInfo (binderXObj v) of
          Nothing -> False
          Just i -> infoLine i == line
    )

binderAtColumn :: Int -> [Binder] -> Maybe Binder
binderAtColumn column =
  find
    ( \v ->
        case xobjInfo (binderXObj v) of
          Nothing -> False
          Just i -> infoColumn i == column
    )
