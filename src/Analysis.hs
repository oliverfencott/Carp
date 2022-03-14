{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Fuse mapM_/map" #-}

module Analysis where

import Data.Function ((&))
import Data.List (find, sortBy)
import Env (allImportedEnvs, findAllSymbols, findAllXObjs)
import Info
import Json (printJson)
import Lsp (documentSymbolToJson, hoverToJson)
import qualified Lsp
import Obj
  ( Binder (binderXObj),
    Context (contextGlobalEnv),
    XObj (xobjInfo, xobjTy),
    getName,
    pretty,
  )
import Prelude hiding (abs)

-- TODO:
-- - Get everything, not just DEFs
-- - Flatten entire environment in order to find all symbols

textHover :: Context -> String -> Int -> Int -> IO ()
textHover ctx filePath line column =
  case maybeBinder of
    Nothing -> pure ()
    Just binder ->
      putStrLn (printJson (hoverToJson (Lsp.Hover binder)))
  where
    env = contextGlobalEnv ctx
    allEnvs = allImportedEnvs env env ++ [env]
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

textDocumentDocumentSymbol :: Context -> String -> IO ()
textDocumentDocumentSymbol ctx filePath =
  do
    putStrLn ("called 'textDocumentDocumentSymbol' with " ++ filePath)
    globalEnvSymbols
      & map Lsp.DocumentSymbol
      & map documentSymbolToJson
      & map printJson
      & mapM_ putStrLn
  where
    globalEnvSymbols =
      contextGlobalEnv ctx
        & findAllSymbols
        & filter
          ( \binder ->
              fileFromBinder binder
                == filePath
          )

fileFromBinder :: Binder -> String
fileFromBinder binder =
  maybe "" infoFile info
  where
    xobj = binderXObj binder
    info = xobjInfo xobj

debugAllSymbolsInFile :: Context -> String -> IO ()
debugAllSymbolsInFile ctx filePath =
  mapM_
    ( \xobj ->
        do
          putStrLn ("Name: " ++ getName xobj)
          putStrLn
            ( maybe
                ""
                ( \info ->
                    "line: " ++ show (infoLine info) ++ ", column: " ++ show (infoColumn info)
                )
                (xobjInfo xobj)
            )
          putStrLn (maybe "" (\ty -> "Type: " ++ show ty) (xobjTy xobj))
          print (pretty xobj)
          putStrLn "\n"
    )
    xobjs
  where
    globals = findAllXObjs (contextGlobalEnv ctx)

    sort a b
      | aa < bb = LT
      | aa > bb = GT
      | otherwise = EQ
      where
        aa = maybe 0 infoLine (xobjInfo a)
        bb = maybe 0 infoLine (xobjInfo b)

    xobjs =
      globals
        & filter
          ( \xobj ->
              case xobjInfo xobj of
                Nothing -> False
                Just info -> infoFile info == filePath
          )
        & sortBy sort
