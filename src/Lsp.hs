module Lsp where

import Env (lookupMeta, searchValueBinder)
import Info (Info (infoColumn, infoFile, infoLine))
import Json (Json (JsonList, JsonMap, JsonNull, JsonNumber, JsonString), printJson)
import qualified Meta
import Obj
import Text.Parsec (ParseError)
import Types

newtype DocumentSymbol = DocumentSymbol Binder

data DiagnosticSeverity
  = Error
  | Warning
  | Information
  | Hint

instance Show DiagnosticSeverity where
  show Error = "1"
  show Warning = "2"
  show Information = "3"
  show Hint = "4"

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

data Hover = HoverXObj Env XObj

newtype Location = Location Binder

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
    kind = maybe (objToSymbolKind obj) tyToSymbolKind (xobjTy xobj)

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
                    ( --
                      -- "__"
                      --   ++ name
                      --   ++ "__ `"
                      "```carp\n"
                        ++ type_
                        ++ "\n```"
                        ++ "\n***\n"
                        ++ either id id doc
                    )
                )
              ]
          )
        ]
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
    _name = getName xobj

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
  JsonMap [("start", start), ("end", end)]
  where
    lineStart = infoLine info
    columnStart = infoColumn info
    start =
      JsonMap
        [ ("line", JsonNumber (show (lineStart - 1))),
          ("character", JsonNumber (show (columnStart - 1)))
        ]
    end =
      JsonMap
        [ ("line", JsonNumber (show (lineStart - 1))),
          ("character", JsonNumber (show columnStart)) -- TODO: Default to see if it actually works. Figure out how to do this correctly
        ]

rangeFromXobj :: XObj -> Json
rangeFromXobj xobj =
  maybe JsonNull makeRange (xobjInfo xobj)

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
getKind binder = completionItemKindFromObj $ xobjObj $ binderXObj binder

completionItemKindFromObj :: Obj -> CompletionItemKind
completionItemKindFromObj C {} = CompletionItemKindValue
completionItemKindFromObj Lst {} = CompletionItemKindValue
completionItemKindFromObj Arr {} = CompletionItemKindValue
completionItemKindFromObj StaticArr {} = CompletionItemKindValue
completionItemKindFromObj Dict {} = CompletionItemKindStruct
completionItemKindFromObj Num {} = CompletionItemKindValue
completionItemKindFromObj Str {} = CompletionItemKindValue
completionItemKindFromObj Pattern {} = CompletionItemKindValue
completionItemKindFromObj Chr {} = CompletionItemKindValue
completionItemKindFromObj Sym {} = CompletionItemKindVariable
completionItemKindFromObj MultiSym {} = CompletionItemKindVariable
completionItemKindFromObj InterfaceSym {} = CompletionItemKindInterface
completionItemKindFromObj Bol {} = CompletionItemKindValue
completionItemKindFromObj Defn {} = CompletionItemKindFunction
completionItemKindFromObj Def = CompletionItemKindVariable
completionItemKindFromObj Fn {} = CompletionItemKindFunction
completionItemKindFromObj Closure {} = CompletionItemKindFunction
completionItemKindFromObj If = CompletionItemKindKeyword
completionItemKindFromObj Match {} = CompletionItemKindKeyword
completionItemKindFromObj While = CompletionItemKindKeyword
completionItemKindFromObj Do = CompletionItemKindKeyword
completionItemKindFromObj Let = CompletionItemKindKeyword
completionItemKindFromObj LocalDef = CompletionItemKindKeyword
completionItemKindFromObj Mod {} = CompletionItemKindModule
completionItemKindFromObj Deftype {} = CompletionItemKindTypeParameter
completionItemKindFromObj DefSumtype {} = CompletionItemKindEnum
completionItemKindFromObj Deftemplate {} = CompletionItemKindFunction
completionItemKindFromObj Instantiate {} = CompletionItemKindFunction
completionItemKindFromObj External {} = CompletionItemKindValue
completionItemKindFromObj ExternalType {} = CompletionItemKindValue
completionItemKindFromObj MetaStub = CompletionItemKindField
completionItemKindFromObj (Defalias _) = CompletionItemKindVariable
completionItemKindFromObj SetBang = CompletionItemKindValue
completionItemKindFromObj Macro = CompletionItemKindFunction
completionItemKindFromObj Dynamic = CompletionItemKindFunction
completionItemKindFromObj DefDynamic = CompletionItemKindFunction
completionItemKindFromObj (Command _) = CompletionItemKindFunction
completionItemKindFromObj (Primitive _) = CompletionItemKindValue
completionItemKindFromObj The = CompletionItemKindKeyword
completionItemKindFromObj Ref = CompletionItemKindFunction
completionItemKindFromObj Deref = CompletionItemKindFunction
completionItemKindFromObj Break = CompletionItemKindKeyword
completionItemKindFromObj (Interface _ _) = CompletionItemKindInterface
completionItemKindFromObj With = CompletionItemKindKeyword

objToSymbolKind :: Obj -> SymbolKind
objToSymbolKind Sym {} = SymbolKindVariable
objToSymbolKind MultiSym {} = SymbolKindArray
objToSymbolKind InterfaceSym {} = SymbolKindInterface
objToSymbolKind Num {} = SymbolKindNumber
objToSymbolKind Str {} = SymbolKindString
objToSymbolKind Pattern {} = SymbolKindString
objToSymbolKind Chr {} = SymbolKindString
objToSymbolKind Bol {} = SymbolKindBoolean
objToSymbolKind Lst {} = SymbolKindArray
objToSymbolKind Arr {} = SymbolKindArray
objToSymbolKind StaticArr {} = SymbolKindArray
objToSymbolKind Dict {} = SymbolKindObject
objToSymbolKind Closure {} = SymbolKindFunction
objToSymbolKind Defn {} = SymbolKindFunction
objToSymbolKind Def {} = SymbolKindVariable
objToSymbolKind Fn {} = SymbolKindFunction
objToSymbolKind Do {} = SymbolKindKey
objToSymbolKind Let {} = SymbolKindVariable
objToSymbolKind LocalDef {} = SymbolKindVariable
objToSymbolKind While {} = SymbolKindEvent
objToSymbolKind Break {} = SymbolKindEvent
objToSymbolKind If {} = SymbolKindEvent
objToSymbolKind Match {} = SymbolKindEvent
objToSymbolKind Mod {} = SymbolKindModule
objToSymbolKind Deftype {} = SymbolKindFile -- TODO
objToSymbolKind DefSumtype {} = SymbolKindEnum
objToSymbolKind With {} = SymbolKindEvent
objToSymbolKind External {} = SymbolKindVariable
objToSymbolKind ExternalType {} = SymbolKindVariable
objToSymbolKind MetaStub {} = SymbolKindObject
objToSymbolKind Deftemplate {} = SymbolKindConstant
objToSymbolKind Instantiate {} = SymbolKindConstructor
objToSymbolKind Defalias {} = SymbolKindVariable
objToSymbolKind SetBang {} = SymbolKindFunction
objToSymbolKind Macro {} = SymbolKindConstructor
objToSymbolKind Dynamic {} = SymbolKindVariable
objToSymbolKind DefDynamic {} = SymbolKindVariable
objToSymbolKind Command {} = SymbolKindEvent
objToSymbolKind Primitive {} = SymbolKindVariable
objToSymbolKind The {} = SymbolKindTypeParameter
objToSymbolKind Ref {} = SymbolKindVariable
objToSymbolKind Deref {} = SymbolKindVariable
objToSymbolKind Interface {} = SymbolKindInterface
objToSymbolKind C {} = SymbolKindConstant

tyToSymbolKind :: Ty -> SymbolKind
tyToSymbolKind IntTy {} = SymbolKindNumber
tyToSymbolKind LongTy {} = SymbolKindNumber
tyToSymbolKind ByteTy {} = SymbolKindVariable
tyToSymbolKind BoolTy {} = SymbolKindBoolean
tyToSymbolKind FloatTy {} = SymbolKindNumber
tyToSymbolKind DoubleTy {} = SymbolKindNumber
tyToSymbolKind StringTy {} = SymbolKindString
tyToSymbolKind PatternTy {} = SymbolKindString -- TODO
tyToSymbolKind CharTy {} = SymbolKindConstant
tyToSymbolKind CCharTy {} = SymbolKindConstant
tyToSymbolKind FuncTy {} = SymbolKindFunction
tyToSymbolKind VarTy {} = SymbolKindVariable
tyToSymbolKind UnitTy {} = SymbolKindNull
tyToSymbolKind ModuleTy {} = SymbolKindModule
tyToSymbolKind PointerTy {} = SymbolKindConstant -- TODO
tyToSymbolKind RefTy {} = SymbolKindConstant
tyToSymbolKind StaticLifetimeTy {} = SymbolKindConstant
tyToSymbolKind StructTy {} = SymbolKindStruct
tyToSymbolKind ConcreteNameTy {} = SymbolKindConstant -- TODO
tyToSymbolKind TypeTy {} = SymbolKindInterface
tyToSymbolKind MacroTy {} = SymbolKindInterface
tyToSymbolKind DynamicTy {} = SymbolKindInterface
tyToSymbolKind InterfaceTy {} = SymbolKindInterface
tyToSymbolKind CTy {} = SymbolKindInterface
tyToSymbolKind Universe {} = SymbolKindInterface

printEvalError :: EvalError -> String
printEvalError (EvalError msg _ _ info) = printErrorDiagnostic msg info
printEvalError (HasStaticCall _xObj info) = printErrorDiagnostic "HasStaticCall error" info

printParseError :: ParseError -> Maybe Info -> String
printParseError parseError = printErrorDiagnostic (show parseError)

printErrorDiagnostic :: String -> Maybe Info -> String
printErrorDiagnostic = printDiagnostic Error

printWarningDiagnostic :: String -> Maybe Info -> String
printWarningDiagnostic = printDiagnostic Warning

printDiagnostic :: DiagnosticSeverity -> String -> Maybe Info -> String
printDiagnostic severity message xobj =
  printJson
    ( JsonMap
        [ ("uri", uri),
          ("diagnostics", diagnostics)
        ]
    )
  where
    uri =
      maybe
        JsonNull
        (JsonString . uriToString)
        xobj
    diagnostics = JsonList [makeDiagnostic severity message xobj]

makeDiagnostic :: DiagnosticSeverity -> String -> Maybe Info -> Json
makeDiagnostic severity message xobj =
  JsonMap
    [ ("message", JsonString message),
      ("source", JsonString "carp"),
      ("severity", JsonString (show severity)),
      ("range", range)
    ]
  where
    range = maybe JsonNull makeRange xobj
