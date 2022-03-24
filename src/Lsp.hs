module Lsp where

import Env (lookupMeta, searchValueBinder)
import Info (Info (infoColumn, infoFile, infoLine))
import Json (Json (JsonList, JsonMap, JsonNull, JsonNumber, JsonString))
import qualified Meta
import Obj

newtype DocumentSymbol = DocumentSymbol Binder

documentSymbolToJson :: DocumentSymbol -> Json
documentSymbolToJson (DocumentSymbol (Binder _ (XObj _ Nothing _))) = JsonNull
documentSymbolToJson (DocumentSymbol (Binder meta xobj@(XObj obj (Just info) _))) =
  json
  where
    json =
      JsonMap
        [ ("name", JsonString name_),
          ("kind", JsonNumber (show kind)),
          ("tags", JsonList tags),
          ("location", location)
        ]

    tags = case Meta.get "deprecated" meta of
      Just (XObj (Bol t) _ _) ->
        [JsonString (show Deprecated) | t]
      Nothing -> []
      Just _ -> []
    name_ = getName xobj
    location =
      JsonMap
        [ ("uri", JsonString uri),
          ("range", makeRange info)
        ]
    uri = uriToString info

    kind = case obj of
      Sym {} -> SymbolKindVariable
      MultiSym {} -> SymbolKindArray
      InterfaceSym {} -> SymbolKindInterface
      Num {} -> SymbolKindNumber
      Str {} -> SymbolKindString
      Pattern {} -> SymbolKindString
      Chr {} -> SymbolKindString
      Bol {} -> SymbolKindBoolean
      Lst {} -> SymbolKindArray
      Arr {} -> SymbolKindArray
      StaticArr {} -> SymbolKindArray
      Dict {} -> SymbolKindObject
      Closure {} -> SymbolKindFunction
      Defn {} -> SymbolKindFunction
      Def {} -> SymbolKindVariable
      Fn {} -> SymbolKindFunction
      Do {} -> SymbolKindKey
      Let {} -> SymbolKindVariable
      LocalDef {} -> SymbolKindVariable
      While {} -> SymbolKindEvent
      Break {} -> SymbolKindEvent
      If {} -> SymbolKindEvent
      Match {} -> SymbolKindEvent
      Mod {} -> SymbolKindModule
      Deftype {} -> SymbolKindFile -- TODO
      DefSumtype {} -> SymbolKindEnum
      With {} -> SymbolKindEvent
      External {} -> SymbolKindVariable
      ExternalType {} -> SymbolKindVariable
      MetaStub {} -> SymbolKindObject
      Deftemplate {} -> SymbolKindConstant
      Instantiate {} -> SymbolKindConstructor
      Defalias {} -> SymbolKindVariable
      SetBang {} -> SymbolKindFunction
      Macro {} -> SymbolKindConstructor
      Dynamic {} -> SymbolKindVariable
      DefDynamic {} -> SymbolKindVariable
      Command {} -> SymbolKindEvent
      Primitive {} -> SymbolKindVariable
      The {} -> SymbolKindTypeParameter
      Ref {} -> SymbolKindVariable
      Deref {} -> SymbolKindVariable
      Interface {} -> SymbolKindInterface
      C {} -> SymbolKindConstant

data Tag = Deprecated

instance Show Tag where
  show Deprecated = "1"

data SymbolKind
  = SymbolKindFile
  | SymbolKindModule
  | SymbolKindNamespace
  | SymbolKindPackage
  | SymbolKindClass
  | SymbolKindMethod
  | SymbolKindProperty
  | SymbolKindField
  | SymbolKindConstructor
  | SymbolKindEnum
  | SymbolKindInterface
  | SymbolKindFunction
  | SymbolKindVariable
  | SymbolKindConstant
  | SymbolKindString
  | SymbolKindNumber
  | SymbolKindBoolean
  | SymbolKindArray
  | SymbolKindObject
  | SymbolKindKey
  | SymbolKindNull
  | SymbolKindEnumMember
  | SymbolKindStruct
  | SymbolKindEvent
  | SymbolKindOperator
  | SymbolKindTypeParameter

data CompletionItemKind
  = CompletionItemKindClass
  | CompletionItemKindColor
  | CompletionItemKindConstant
  | CompletionItemKindConstructor
  | --
    CompletionItemKindEnum
  | CompletionItemKindEnumMember
  | CompletionItemKindEvent
  | --
    CompletionItemKindField
  | CompletionItemKindFile
  | CompletionItemKindFolder
  | CompletionItemKindFunction
  | --
    CompletionItemKindInterface
  | --
    CompletionItemKindKeyword
  | --
    CompletionItemKindMethod
  | CompletionItemKindModule
  | --
    CompletionItemKindOperator
  | --
    CompletionItemKindProperty
  | --
    CompletionItemKindReference
  | --
    CompletionItemKindSnippet
  | CompletionItemKindStruct
  | --
    CompletionItemKindText
  | CompletionItemKindTypeParameter
  | --
    CompletionItemKindUnit
  | --
    CompletionItemKindValue
  | CompletionItemKindVariable

newtype CompletionItem = CompletionItem Binder

instance Show CompletionItemKind where
  show CompletionItemKindText = "1"
  show CompletionItemKindMethod = "2"
  show CompletionItemKindFunction = "3"
  show CompletionItemKindConstructor = "4"
  show CompletionItemKindField = "5"
  show CompletionItemKindVariable = "6"
  show CompletionItemKindClass = "7"
  show CompletionItemKindInterface = "8"
  show CompletionItemKindModule = "9"
  show CompletionItemKindProperty = "10"
  show CompletionItemKindUnit = "11"
  show CompletionItemKindValue = "12"
  show CompletionItemKindEnum = "13"
  show CompletionItemKindKeyword = "14"
  show CompletionItemKindSnippet = "15"
  show CompletionItemKindColor = "16"
  show CompletionItemKindFile = "17"
  show CompletionItemKindReference = "18"
  show CompletionItemKindFolder = "19"
  show CompletionItemKindEnumMember = "20"
  show CompletionItemKindConstant = "21"
  show CompletionItemKindStruct = "22"
  show CompletionItemKindEvent = "23"
  show CompletionItemKindOperator = "24"
  show CompletionItemKindTypeParameter = "25"

instance Show SymbolKind where
  show SymbolKindFile = "1"
  show SymbolKindModule = "2"
  show SymbolKindNamespace = "3"
  show SymbolKindPackage = "4"
  show SymbolKindClass = "5"
  show SymbolKindMethod = "6"
  show SymbolKindProperty = "7"
  show SymbolKindField = "8"
  show SymbolKindConstructor = "9"
  show SymbolKindEnum = "10"
  show SymbolKindInterface = "11"
  show SymbolKindFunction = "12"
  show SymbolKindVariable = "13"
  show SymbolKindConstant = "14"
  show SymbolKindString = "15"
  show SymbolKindNumber = "16"
  show SymbolKindBoolean = "17"
  show SymbolKindArray = "18"
  show SymbolKindObject = "19"
  show SymbolKindKey = "20"
  show SymbolKindNull = "21"
  show SymbolKindEnumMember = "22"
  show SymbolKindStruct = "23"
  show SymbolKindEvent = "24"
  show SymbolKindOperator = "25"
  show SymbolKindTypeParameter = "26"

data Hover
  = HoverXObj Env XObj

hoverToJson :: Hover -> Json
hoverToJson (HoverXObj env xobj) =
  json
  where
    json =
      JsonMap
        [ ( "contents",
            JsonMap
              [ ("kind", JsonString "markdown"),
                ( "value",
                  JsonString
                    ( "\n__" ++ name ++ "__ `" ++ type_ ++ "`\n"
                        ++ "\n***\n"
                        ++ either id id doc
                        ++ file
                        ++ "\n***\n"
                    )
                )
              ]
          )
        ]
    file = case xobjInfo xobj of
      Nothing -> ""
      Just i ->
        "\n***\n*"
          ++ infoFile i
          ++ ":"
          ++ show (infoLine i)
          ++ ":"
          ++ show (infoColumn i)
          ++ "*"
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
    name = getName xobj

newtype Location = Location Binder

locationToJson :: Location -> Json
locationToJson (Location binder) =
  case xobjInfo (binderXObj binder) of
    Nothing -> JsonNull
    Just info ->
      JsonMap
        [ ("uri", JsonString (uriToString info)),
          ("range", makeRange info)
        ]

makeRange :: Info -> Json
makeRange info =
  range
  where
    range =
      JsonMap [("start", start), ("end", end)]

    lineStart = infoLine info
    columnStart = infoColumn info
    start =
      JsonMap
        [ ("line", JsonNumber (show (lineStart - 1))),
          ("character", JsonNumber (show (columnStart - 1)))
        ]
    end =
      JsonMap
        [ ("line", JsonNumber (show lineStart)),
          ("character", JsonNumber (show columnStart)) -- TODO: Default to see if it actually works. Figure out how to do this correctly
        ]

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
      ("kind", JsonNumber (show kind))
    ]
  where
    documentation = case Meta.get "doc" (binderMeta binder) of
      Just (XObj (Str doc) _ _) -> show doc
      Just _ -> ""
      Nothing -> ""
    label = getName (binderXObj binder)
    detail = maybe "" show (xobjTy (binderXObj binder))
    kind = getKind binder

uriToString :: Info -> String
uriToString info = "file://" ++ infoFile info

getKind :: Binder -> CompletionItemKind
getKind binder =
  case xobjObj (binderXObj binder) of
    C {} -> CompletionItemKindValue
    Lst {} -> CompletionItemKindValue
    Arr {} -> CompletionItemKindValue
    StaticArr {} -> CompletionItemKindValue
    Dict {} -> CompletionItemKindStruct
    Num {} -> CompletionItemKindValue
    Str {} -> CompletionItemKindValue
    Pattern {} -> CompletionItemKindValue
    Chr {} -> CompletionItemKindValue
    Sym {} -> CompletionItemKindVariable
    MultiSym {} -> CompletionItemKindVariable
    InterfaceSym {} -> CompletionItemKindInterface
    Bol {} -> CompletionItemKindValue
    Defn {} -> CompletionItemKindFunction
    Def -> CompletionItemKindVariable
    Fn {} -> CompletionItemKindFunction
    Closure {} -> CompletionItemKindFunction
    If -> CompletionItemKindKeyword
    Match {} -> CompletionItemKindKeyword
    While -> CompletionItemKindKeyword
    Do -> CompletionItemKindKeyword
    Let -> CompletionItemKindKeyword
    LocalDef -> CompletionItemKindKeyword
    Mod {} -> CompletionItemKindModule
    Deftype {} -> CompletionItemKindTypeParameter
    DefSumtype {} -> CompletionItemKindEnum
    Deftemplate {} -> CompletionItemKindFunction
    Instantiate {} -> CompletionItemKindFunction
    External {} -> CompletionItemKindValue
    ExternalType {} -> CompletionItemKindValue
    MetaStub -> CompletionItemKindField
    Defalias _ -> CompletionItemKindVariable
    SetBang -> CompletionItemKindValue
    Macro -> CompletionItemKindFunction
    Dynamic -> CompletionItemKindFunction
    DefDynamic -> CompletionItemKindFunction
    Command _ -> CompletionItemKindFunction
    Primitive _ -> CompletionItemKindValue
    The -> CompletionItemKindKeyword
    Ref -> CompletionItemKindFunction
    Deref -> CompletionItemKindFunction
    Break -> CompletionItemKindKeyword
    Interface _ _ -> CompletionItemKindInterface
    With -> CompletionItemKindKeyword
