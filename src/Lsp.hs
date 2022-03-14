module Lsp where

import Data.Either (fromRight)
import Info (Info (infoColumn, infoFile, infoLine))
import Json (Json (JsonList, JsonMap, JsonNull, JsonNumber, JsonString))
import qualified Meta
import Obj

newtype DocumentSymbol = DocumentSymbol Binder

documentSymbolToJson :: DocumentSymbol -> Json
documentSymbolToJson (Lsp.DocumentSymbol (Binder _ (XObj _ Nothing _))) = JsonNull
documentSymbolToJson (Lsp.DocumentSymbol (Binder meta xobj@(XObj obj (Just info) _))) =
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
          ("range", range)
        ]
    range =
      JsonMap
        [ ( "start",
            start
          ),
          ( "end",
            end
          )
        ]
    uri = infoFile info
    lineStart = infoLine info
    columnStart = infoColumn info
    start =
      JsonMap
        [ ("line", JsonNumber (show (lineStart - 1))),
          ("character", JsonNumber (show columnStart))
        ]
    end =
      JsonMap
        [ ("line", JsonNumber (show lineStart)),
          ("character", JsonNumber (show (columnStart + 1))) -- TODO: + 1 is a default to see if it actually works
        ]
    kind = case obj of
      Sym {} -> Variable
      MultiSym {} -> Array
      InterfaceSym {} -> Lsp.Interface
      Num {} -> Number
      Str {} -> String
      Pattern {} -> String
      Chr {} -> String
      Bol {} -> Boolean
      Lst {} -> Array
      Arr {} -> Array
      StaticArr {} -> Array
      Dict {} -> Object
      Closure {} -> Function
      Defn {} -> Function
      Def {} -> Variable
      Fn {} -> Function
      Do {} -> Key
      Let {} -> Variable
      LocalDef {} -> Variable
      While {} -> Event
      Break {} -> Event
      If {} -> Event
      Match {} -> Event
      Mod {} -> Module
      Deftype {} -> File -- TODO
      DefSumtype {} -> Enum
      With {} -> Event
      External {} -> Variable
      ExternalType {} -> Variable
      MetaStub {} -> Object
      Deftemplate {} -> Constant
      Instantiate {} -> Constructor
      Defalias {} -> Variable
      SetBang {} -> Function
      Macro {} -> Constructor
      Dynamic {} -> Variable
      DefDynamic {} -> Variable
      Command {} -> Event
      Primitive {} -> Variable
      The {} -> TypeParameter
      Ref {} -> Variable
      Deref {} -> Variable
      Obj.Interface {} -> Lsp.Interface
      C {} -> Constant

data Tag = Deprecated

instance Show Tag where
  show Deprecated = "1"

data SymbolKind
  = File
  | Module
  | Namespace
  | Package
  | Class
  | Method
  | Property
  | Field
  | Constructor
  | Enum
  | Interface
  | Function
  | Variable
  | Constant
  | String
  | Number
  | Boolean
  | Array
  | Object
  | Key
  | Null
  | EnumMember
  | Struct
  | Event
  | Operator
  | TypeParameter

instance Show SymbolKind where
  show File = "1"
  show Module = "2"
  show Namespace = "3"
  show Package = "4"
  show Class = "5"
  show Method = "6"
  show Property = "7"
  show Field = "8"
  show Constructor = "9"
  show Enum = "10"
  show Lsp.Interface = "11"
  show Function = "12"
  show Variable = "13"
  show Constant = "14"
  show String = "15"
  show Number = "16"
  show Boolean = "17"
  show Array = "18"
  show Object = "19"
  show Key = "20"
  show Null = "21"
  show EnumMember = "22"
  show Struct = "23"
  show Event = "24"
  show Operator = "25"
  show TypeParameter = "26"

newtype Hover = Hover Binder

hoverToJson :: Hover -> Json
hoverToJson (Hover binder) =
  json
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
    name = getSimpleName (binderXObj binder)
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
