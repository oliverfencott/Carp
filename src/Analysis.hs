module Analysis where

-- import Env (allImportedEnvs, findAllSymbols, lookupChildren)
-- import Env (allImportedEnvs, findAllSymbols, lookupChildren)

import Data.List (find)
import Env (findAllForms)
import Info
import Json (Json (JsonMap, JsonString), printJson)
import Obj (Context (contextGlobalEnv), Obj (Interface, Lst, Mod), TypeEnv (getTypeEnv), XObj (XObj, xobjInfo, xobjTy), getName)
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
-- - Rename function
-- - Move actual analysis function in Analysis.hs module

-- textHover :: Context -> String -> Int -> Int -> IO ()
-- textHover ctx filePath line column =
--   do
--     case binder of
--       Nothing -> pure ()
--       Just b ->
--         do
--           putStrLn (printJson json)
--         where
--           xObj = binderXObj b
--           type_ = xobjTy xObj
--           _typeInfo =
--             maybe
--               ""
--               ( \t ->
--                   "```\n"
--                     ++ show t
--                     ++ "\n```\n***\n"
--               )
--               type_
--           meta = binderMeta b
--           doc = case Meta.get "doc" meta of
--             Just (XObj (Str s) _ _) -> s
--             _ -> ""
--           json =
--             JsonMap
--               [ ( "contents",
--                   JsonMap
--                     [ ("kind", JsonString "markdown"),
--                       ( "value",
--                         JsonString
--                           ( showBinderInMarkdown b ++ "\n***\n" ++ doc
--                           )
--                       )
--                     ]
--                 )
--               ]
--   where
--     env = contextGlobalEnv ctx
--     allEnvs = lookupChildren env ++ [env]
--     inFile = concatMap (bindersInFile filePath . findAllSymbols) allEnvs
--     onLine = bindersOnLine line inFile
--     binder = findObj column onLine
--     findObj :: Int -> [Binder] -> Maybe Binder
--     findObj col binderList =
--       if col < 0
--         then Nothing
--         else case binderAtColumn col binderList of
--           Nothing -> findObj (col - 1) binderList
--           res -> res

-- showBinderInMarkdown :: Binder -> String
-- showBinderInMarkdown binder = showBinderIndented 0 (getName (binderXObj binder), binder)

-- showBinderIndented :: Int -> (String, Binder) -> String
-- showBinderIndented indent (name, Binder _ (XObj (Mod env tenv) _ _)) =
--   replicate indent ' ' ++ name ++ " = {\n\n"
--     ++ showBindings env
--     ++ "\n\n"
--     ++ showBindings (getTypeEnv tenv)
--     ++ "\n\n"
--     ++ replicate indent ' '
--     ++ "}"
--   where
--     showBindings e =
--       joinLines $
--         filter
--           (/= "")
--           ( map
--               (showBinderIndented (indent + 2))
--               (Map.toList (envBindings e))
--           )
-- showBinderIndented indent (name, Binder _ (XObj (Lst [XObj (Interface t paths) _ _, _]) _ _)) =
--   replicate indent ' ' ++ name ++ ": " ++ show t ++ " = {\n\n  "
--     ++ joinWith "\n\n  " (map show paths)
--     ++ "\n\n"
--     ++ replicate indent ' '
--     ++ "}"
-- showBinderIndented indent (name, Binder _ xobj) =
--   replicate indent ' ' ++ name
--     ++ ": "
--     ++ "```"
--     ++ showMaybeTy (xobjTy xobj)
--     ++ " ```"

-- bindersInFile :: String -> [Binder] -> [Binder]
-- bindersInFile file =
--   filter
--     ( \binder ->
--         case infoFromBinder binder of
--           Nothing -> False
--           Just info -> infoFile info == file
--     )

-- bindersOnLine :: Int -> [Binder] -> [Binder]
-- bindersOnLine line =
--   filter
--     ( \v ->
--         case infoFromBinder v of
--           Nothing -> False
--           Just i -> infoLine i == line
--     )

-- binderAtColumn :: Int -> [Binder] -> Maybe Binder
-- binderAtColumn column =
--   find
--     ( \v ->
--         case infoFromBinder v of
--           Nothing -> False
--           Just i -> infoColumn i == column
--     )

-- infoFromBinder :: Binder -> Maybe Info
-- infoFromBinder binder = xobjInfo (binderXObj binder)

textHover :: Context -> String -> Int -> Int -> IO ()
textHover ctx filePath line column =
  do
    case binder of
      Nothing -> pure ()
      Just xObj ->
        do
          putStrLn (printJson json)
        where
          type_ = maybe "no type found" show (xobjTy xObj)
          _file = maybe "" infoFile (xobjInfo xObj)
          -- type_ = case Meta.get "doc" meta of
          --   Just (XObj (Str s) _ _) -> s
          --   _ -> ""
          json =
            JsonMap
              [ ( "contents",
                  JsonMap
                    [ ("kind", JsonString "markdown"),
                      ( "value",
                        JsonString
                          ( showBinderInMarkdown2 xObj ++ "\n***\n" ++ type_ ++ "\n***\n" ++ _file
                          )
                      )
                    ]
                )
              ]
  where
    env = contextGlobalEnv ctx
    inFile = concatMap (bindersInFile2 filePath . findAllForms) [env]
    onLine = bindersOnLine2 line inFile
    binder = findObj column onLine
    findObj :: Int -> [XObj] -> Maybe XObj
    findObj col binderList =
      if col < 0
        then Nothing
        else case binderAtColumn2 col binderList of
          Nothing -> findObj (col - 1) binderList
          res -> res

showBinderInMarkdown2 :: XObj -> String
showBinderInMarkdown2 xObj = showBinderIndented2 0 (getName xObj, xObj)

showBinderIndented2 :: Int -> (String, XObj) -> String
showBinderIndented2 indent (name, XObj (Mod env tenv) _ _) =
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
              ( showBinderIndented2 (indent + 2) . (\x -> (getName x, x))
              )
              (findAllForms e)
          )
showBinderIndented2 indent (name, XObj (Lst [XObj (Interface t paths) _ _, _]) _ _) =
  replicate indent ' ' ++ name ++ ": " ++ show t ++ " = {\n\n  "
    ++ joinWith "\n\n  " (map show paths)
    ++ "\n\n"
    ++ replicate indent ' '
    ++ "}"
showBinderIndented2 indent (name, xobj) =
  replicate indent ' ' ++ name
    ++ ": "
    ++ "```"
    ++ showMaybeTy (xobjTy xobj)
    ++ " ```"

bindersInFile2 :: String -> [XObj] -> [XObj]
bindersInFile2 file =
  filter
    ( \binder ->
        case xobjInfo binder of
          Nothing -> False
          Just info -> infoFile info == file
    )

bindersOnLine2 :: Int -> [XObj] -> [XObj]
bindersOnLine2 line =
  filter
    ( \v ->
        case xobjInfo v of
          Nothing -> False
          Just i -> infoLine i == line
    )

binderAtColumn2 :: Int -> [XObj] -> Maybe XObj
binderAtColumn2 column =
  find
    ( \v ->
        case xobjInfo v of
          Nothing -> False
          Just i -> infoColumn i == column
    )
