module Lsp where

import Info (Info (infoColumn, infoFile, infoLine))
import Json (Json (JsonList, JsonMap, JsonNull, JsonNumber, JsonString))
import qualified Meta
import Obj

newtype Symbol = Symbol Binder

data SymbolInformation = SymbolInformation
  { symbolInformationName :: String,
    symbolInformationKind :: SymbolKind,
    symbolInformationTags :: [Tag],
    symbolInformationLocation :: Location
  }

data Location = Location
  { locationUri :: String,
    locationRange :: Range
  }

data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }

data Position = Position
  { positionLine :: Int,
    positionCharacter :: Int
  }

toJson :: Symbol -> Json
toJson (Lsp.Symbol (Binder _ (XObj _ Nothing _))) = JsonNull
toJson (Lsp.Symbol (Binder meta xobj@(XObj obj (Just info) _))) =
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

-- export interface SymbolInformation {
--     /**
--      * The name of this symbol.
--      */
--     name: string;
--     /**
--      * The kind of this symbol.
--      */
--     kind: SymbolKind;
--     /**
--      * Tags for this completion item.
--      *
--      * @since 3.16.0
--      */
--     tags?: SymbolTag[];
--     /**
--      * Indicates if this symbol is deprecated.
--      *
--      * @deprecated Use tags instead
--      */
--     deprecated?: boolean;
--     /**
--      * The location of this symbol. The location's range is used by a tool
--      * to reveal the location in the editor. If the symbol is selected in the
--      * tool the range's start information is used to position the cursor. So
--      * the range usually spans more than the actual symbol's name and does
--      * normally include thinks like visibility modifiers.
--      *
--      * The range doesn't have to denote a node range in the sense of a abstract
--      * syntax tree. It can therefore not be used to re-construct a hierarchy of
--      * the symbols.
--      */
--     location: Location;
--     /**
--      * The name of the symbol containing this symbol. This information is for
--      * user interface purposes (e.g. to render a qualifier in the user interface
--      * if necessary). It can't be used to re-infer a hierarchy for the document
--      * symbols.
--      */
--     containerName?: string;
-- }
