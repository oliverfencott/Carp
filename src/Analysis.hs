module Analysis where

import Control.Monad (unless)
import Data.Either
import Data.Function ((&))
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe)
import Env (EnvironmentError, findAllNestedBinders, findAllXObjsInFile, lookupMeta, searchValueBinder)
import Info
import Json (Json (JsonList, JsonMap, JsonNull, JsonString), ToJson (toJson))
import LanguageServer (infoToRange, maybeInfoToFileUri, objToCompletionItemKind, objToSymbolKind, parseFilePath, tagsFromMeta, tyToSymbolKind)
import qualified Lsp
import Map (assocs)
import qualified Meta
import Obj
  ( Binder (Binder, binderMeta, binderXObj),
    Context (contextGlobalEnv),
    Env,
    MetaData (getMeta),
    Obj (Mod, Str),
    XObj (XObj, xobjInfo, xobjObj, xobjTy),
    getName,
    getPath,
    metaIsTrue,
    pretty,
    unwrapStringXObj,
  )
import SymPath (SymPath)
import Util (joinLines)
import Prelude hiding (abs)

newtype CompletionItem = CompletionItem Binder

uriToString :: Info -> String
uriToString = maybeInfoToFileUri . Just

getKind :: Binder -> Lsp.CompletionItemKind
getKind binder = objToCompletionItemKind $ xobjObj $ binderXObj binder

textDocumentDocumentSymbol :: Context -> String -> IO ()
textDocumentDocumentSymbol ctx rawPath =
  contextGlobalEnv ctx
    & findAllNestedBinders
    & filter binderNotHidden
    & filter ((== parseFilePath rawPath) . fileFromBinder)
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
    tags = tagsFromMeta meta
    name = getName xobj
    location =
      Lsp.Location uri (infoToRange info)
    uri = uriToString info
    kind = maybe (objToSymbolKind obj) tyToSymbolKind (xobjTy xobj)

textDocumentCompletion :: Context -> String -> IO ()
textDocumentCompletion ctx _filePath =
  map
    (completionItemToJson . CompletionItem)
    ( findAllNestedBinders (contextGlobalEnv ctx)
        & filter binderNotHidden
        & concatMap (: [])
    )
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
    filePath = parseFilePath rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine

hoverToJson :: Env -> XObj -> String
hoverToJson env xobj =
  show (Lsp.Hover contents range)
  where
    info = fromMaybe dummyInfo (xobjInfo xobj)
    range = infoToRange info
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

definitionLocation :: Context -> String -> Int -> Int -> IO ()
definitionLocation ctx rawPath line column =
  case maybeInfo of
    Nothing -> pure ()
    Just info -> print (locationToJson info)
  where
    filePath = parseFilePath rawPath
    env = contextGlobalEnv ctx
    allSymbols = findAllXObjsInFile env filePath
    onLine = xobjsOnLine line allSymbols
    maybeXObj = findObjAtColumn column onLine
    maybeBinder =
      maybeXObj
        >>= (either (const Nothing) Just . searchValueBinder env . getPath)
    maybeInfo = maybeBinder >>= (xobjInfo . binderXObj)
    locationToJson info =
      toJson (Lsp.Location (uriToString info) (infoToRange info))

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

debugAllBindersInFile :: Context -> String -> IO ()
debugAllBindersInFile ctx rawPath =
  do
    mapM_
      ( \binder ->
          let xobj = binderXObj binder
              symPath = getPath xobj
              meta = case binder of
                Binder m (XObj (Mod _ _) _ _) -> Right m
                _ -> lookupMeta env symPath
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
                maybe
                  (pure ())
                  ( \info ->
                      do
                        putStrLn ("Line: " ++ show (infoLine info))
                        putStrLn ("Column: " ++ show (infoColumn info))
                  )
                  (xobjInfo xobj)
                unless (null doc) (putStrLn ("Doc: " ++ doc))
                unless (null allMeta) (putStrLn ("All meta: " ++ allMeta))
                putStr (maybe "" (\ty -> "Type: " ++ show ty ++ "\n") (xobjTy xobj))
                print (pretty xobj)
                case xobjInfo xobj of
                  Nothing -> pure ()
                  Just t -> print t
                putStrLn "\n"
      )
      binders
  where
    filePath = parseFilePath rawPath
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

debugAllSymbolsInFile :: Context -> String -> IO ()
debugAllSymbolsInFile ctx rawPath =
  do
    mapM_
      ( \xobj ->
          let symPath = getPath xobj
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
      xobjs
  where
    filePath = parseFilePath rawPath
    env = contextGlobalEnv ctx
    valueBinders = findAllXObjsInFile env filePath
    allBinders = valueBinders

    -- NOTE: This isn't needed. Just for debugging
    sort :: XObj -> XObj -> Ordering
    sort a b
      | lineA < lineB = LT
      | lineA > lineB = GT
      | otherwise = if columnA < columnB then LT else if columnA > columnB then GT else EQ
      where
        aInfo = xobjInfo a
        bInfo = xobjInfo b
        lineA = maybe 0 infoLine aInfo
        lineB = maybe 0 infoLine bInfo
        columnA = maybe 0 infoColumn aInfo
        columnB = maybe 0 infoColumn bInfo

    xobjs =
      allBinders
        & filter
          ( \xobj ->
              case xobjInfo xobj of
                Nothing -> False
                Just info -> infoFile info == filePath
          )
        & sortBy sort

metaFromBinder :: Env -> SymPath -> Binder -> Either EnvironmentError MetaData
metaFromBinder env symPath binder = case binder of
  Binder m (XObj (Mod _ _) _ _) -> Right m
  _ -> lookupMeta env symPath
