{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Fuse mapM_/map" #-}

module Analysis where

import Data.Either
import Data.Function ((&))
import Data.List (find, sortBy)
import Data.Maybe (isNothing)
import Env (findAllNestedBinders, findAllXObjsInFile, lookupMeta, searchValueBinder)
import Info
import Json (Json (JsonList), printJson)
import Lsp (documentSymbolToJson, hoverToJson)
import qualified Lsp
import qualified Meta
import Obj
  ( Binder (binderMeta, binderXObj),
    Context (contextGlobalEnv),
    XObj (xobjInfo, xobjTy),
    getName,
    getPath,
    pretty,
    unwrapStringXObj,
  )
import Util (stripFileProtocol)
import Prelude hiding (abs)

textDocumentDocumentSymbol :: Context -> String -> IO ()
textDocumentDocumentSymbol ctx rawPath =
  contextGlobalEnv ctx
    & findAllNestedBinders
    & filter ((== stripFileProtocol rawPath) . fileFromBinder)
    & map Lsp.DocumentSymbol
    & map documentSymbolToJson
    & JsonList
    & printJson
    & putStrLn

textDocumentCompletion :: Context -> String -> IO ()
textDocumentCompletion ctx _filePath =
  findAllNestedBinders (contextGlobalEnv ctx)
    & filter (isNothing . Meta.get "hidden" . binderMeta)
    & concatMap
      ( \binder ->
          let res = [binder]
           in res
      )
    & map Lsp.CompletionItem
    & map Lsp.completionItemToJson
    & JsonList
    & printJson
    & putStrLn

textHover :: Context -> String -> Int -> Int -> IO ()
textHover ctx rawPath line column =
  case maybeXObj of
    Nothing -> pure ()
    Just xobj ->
      putStrLn (printJson (hoverToJson (Lsp.HoverXObj env xobj)))
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine

fileFromBinder :: Binder -> String
fileFromBinder binder =
  maybe "" infoFile info
  where
    xobj = binderXObj binder
    info = xobjInfo xobj

debugAllSymbolsInFile :: Context -> String -> IO ()
debugAllSymbolsInFile ctx rawPath =
  mapM_
    ( \binder ->
        let xobj = binderXObj binder
            symPath = getPath xobj
            meta = lookupMeta env symPath
            docLookup = case meta of
              Left _ -> Left ""
              Right m ->
                case Meta.get "doc" m of
                  Nothing -> Right ""
                  Just x -> unwrapStringXObj x
            doc = either id id docLookup
         in do
              putStrLn ("Name: " ++ getName xobj)
              putStrLn ("Doc: " ++ doc)
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
    binders
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    valueBinders = findAllNestedBinders env
    allBinders = valueBinders

    -- NOTE: This isn't needed. Just for debugging
    sort :: Binder -> Binder -> Ordering
    sort a b
      | lineA < lineB = LT
      | lineA > lineB = GT
      | otherwise = if columnA < columnB then LT else if columnA > columnB then GT else EQ
      where
        aInfo = xobjInfo (binderXObj a)
        bInfo = xobjInfo (binderXObj b)
        lineA = maybe 0 infoLine aInfo
        lineB = maybe 0 infoLine bInfo
        columnA = maybe 0 infoColumn aInfo
        columnB = maybe 0 infoColumn bInfo

    binders =
      allBinders
        & filter
          ( \binder ->
              case xobjInfo (binderXObj binder) of
                Nothing -> False
                Just info -> infoFile info == filePath
          )
        & sortBy sort

definitionLocation :: Context -> String -> Int -> Int -> IO ()
definitionLocation ctx rawPath line column =
  case maybeBinder of
    Nothing -> pure ()
    Just binder ->
      putStrLn (printJson (Lsp.locationToJson (Lsp.Location binder)))
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine
    maybeBinder =
      maybeXObj
        >>= (either (const Nothing) Just . searchValueBinder env . getPath)

xobjsOnLine :: Int -> [XObj] -> [XObj]
xobjsOnLine line =
  filter
    ( \v ->
        case xobjInfo v of
          Nothing -> False
          Just i -> infoLine i == line
    )

xobjAtColumn :: Int -> [XObj] -> Maybe XObj
xobjAtColumn column =
  find
    ( \v ->
        case xobjInfo v of
          Nothing -> False
          Just i -> infoColumn i == column
    )

findObjAtColumn :: Int -> [XObj] -> Maybe XObj
findObjAtColumn col xObjList =
  if col < 0
    then Nothing
    else case xobjAtColumn col xObjList of
      Nothing -> findObjAtColumn (col - 1) xObjList
      res -> res
