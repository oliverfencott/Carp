{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Fuse mapM_/map" #-}

module Analysis where

import Control.Monad (unless)
import Data.Either
import Data.Function ((&))
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe)
import Env (findAllNestedBinders, findAllXObjsInFile, lookupMeta, searchValueBinder)
import Info
import Json (Json (JsonList, JsonMap, JsonNull, JsonString), ToJson (toJson))
import Lsp (SymbolTag (Deprecated))
import qualified Lsp
import Map (assocs)
import qualified Meta
import Obj
  ( Binder (Binder, binderMeta, binderXObj),
    Context (contextGlobalEnv, contextTypeEnv),
    Env,
    MetaData (getMeta),
    Obj (Bol, Str),
    TypeEnv (getTypeEnv),
    XObj (XObj, xobjInfo, xobjObj, xobjTy),
    getName,
    getPath,
    metaIsTrue,
    objToLspSymbolKind,
    pretty,
    toLspCompletionItemKind,
    unwrapStringXObj,
  )
import Types (tyToLspSymbolKind)
import Util (joinLines, stripFileProtocol)
import Prelude hiding (abs)

newtype CompletionItem = CompletionItem Binder

uriToString :: Info -> String
uriToString info = "file://" ++ infoFile info

getKind :: Binder -> Lsp.CompletionItemKind
getKind binder = toLspCompletionItemKind $ xobjObj $ binderXObj binder

textDocumentDocumentSymbol :: Context -> String -> IO ()
textDocumentDocumentSymbol ctx rawPath =
  contextGlobalEnv ctx
    & findAllNestedBinders
    & filter binderNotHidden
    & filter ((== stripFileProtocol rawPath) . fileFromBinder)
    & map documentSymbolToJson
    & JsonList
    & print

-- TODO: THIS IS RETURNING NULL BECAUSE OF A LACK OF METADATA
documentSymbolToJson :: Binder -> Json
documentSymbolToJson ((Binder _ (XObj _ Nothing _))) = JsonNull
documentSymbolToJson ((Binder meta xobj@(XObj obj (Just info) _))) =
  toJson symbolInformation
  where
    symbolInformation = Lsp.SymbolInformation name kind tags location
    tags = case Meta.get "deprecated" meta of
      Just (XObj (Bol True) _ _) -> [Deprecated]
      Nothing -> []
      Just _ -> []
    name = getName xobj
    location =
      Lsp.Location uri (infoToLspRange info)
    uri = uriToString info
    kind = maybe (objToLspSymbolKind obj) tyToLspSymbolKind (xobjTy xobj)

textDocumentCompletion :: Context -> String -> IO ()
textDocumentCompletion ctx _filePath =
  findAllNestedBinders (contextGlobalEnv ctx)
    & filter binderNotHidden
    & concatMap
      ( \binder ->
          let res = [binder]
           in res
      )
    & map CompletionItem
    & map completionItemToJson
    & JsonList
    & print

completionItemToJson :: CompletionItem -> Json
completionItemToJson (CompletionItem binder) =
  JsonMap
    [ ("label", JsonString label),
      ("detail", JsonString detail),
      ( "documentation",
        JsonMap
          [ ("kind", JsonString "markdown"),
            ("value", JsonString documentation)
          ]
      ),
      ("kind", toJson kind)
    ]
  where
    documentation = case Meta.get "doc" (binderMeta binder) of
      Just (XObj (Str doc) _ _) -> show doc
      Just _ -> ""
      Nothing -> ""
    label = getName (binderXObj binder)
    detail = maybe "" show (xobjTy (binderXObj binder))
    kind = getKind binder

binderNotHidden :: Binder -> Bool
binderNotHidden binder =
  not (metaIsTrue (binderMeta binder) "hidden")

textHover :: Context -> String -> Int -> Int -> IO ()
textHover ctx rawPath line column =
  case maybeXObj of
    Nothing -> pure ()
    Just xobj ->
      putStrLn (hoverToJson env xobj)
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine

hoverToJson :: Env -> XObj -> String
hoverToJson env xobj =
  show (Lsp.Hover contents range)
  where
    info = fromMaybe dummyInfo (xobjInfo xobj)
    range = infoToLspRange info
    value =
      "```carp\n"
        ++ type_
        ++ "\n```"
        ++ "\n***\n"
        ++ either id id doc
    contents = Lsp.MarkupContent Lsp.Markdown value
    symPath = getPath xobj
    binder = either (const Nothing) Just (searchValueBinder env symPath)
    fallBackType = maybe "" show (binder >>= xobjTy . binderXObj)
    type_ = maybe fallBackType show (xobjTy xobj)
    meta = lookupMeta env symPath
    doc = case meta of
      Left _ -> Left ""
      Right m ->
        case Meta.get "doc" m of
          Nothing -> Right ""
          Just x -> unwrapStringXObj x

fileFromBinder :: Binder -> String
fileFromBinder binder = maybe "" infoFile (xobjInfo (binderXObj binder))

debugAllSymbolsInFile :: Context -> String -> IO ()
debugAllSymbolsInFile ctx rawPath =
  do
    putStrLn rawPath
    putStrLn filePath
    print (getTypeEnv (contextTypeEnv ctx))
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
              allMeta =
                either
                  (const [])
                  ( joinLines
                      . map
                        (\(k, v) -> k ++ ": " ++ pretty v)
                      . assocs
                      . getMeta
                  )
                  meta
           in do
                putStrLn ("Name: " ++ getName xobj)
                unless (null doc) (putStrLn ("Doc: " ++ doc))
                unless (null allMeta) (putStrLn ("All meta: " ++ allMeta))
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
                case xobjInfo xobj of
                  Nothing -> pure ()
                  Just t -> print t
                putStrLn "\n"
      )
      (binders ++ typeEnvBinders)
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    valueBinders = findAllNestedBinders env
    allBinders = valueBinders
    typeEnvBinders = findAllNestedBinders (getTypeEnv (contextTypeEnv ctx))

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
  case maybeInfo of
    Nothing -> pure ()
    Just info -> print (locationToJson info)
  where
    filePath = stripFileProtocol rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine
    maybeBinder =
      maybeXObj
        >>= (either (const Nothing) Just . searchValueBinder env . getPath)
    maybeInfo = maybeBinder >>= (xobjInfo . binderXObj)
    locationToJson info =
      toJson (Lsp.Location (uriToString info) (infoToLspRange info))

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
