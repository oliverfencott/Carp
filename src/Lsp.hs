module Lsp where

import Json (Json (JsonList, JsonMap, JsonNull, JsonNumber, JsonString), JsonNumberKind (JsonNumberInt), ToJson (toJson))

data DiagnosticSeverity
  = Error
  | Warning
  | Information
  | Hint

instance ToJson DiagnosticSeverity where
  toJson Error = JsonNumber (JsonNumberInt 1)
  toJson Warning = JsonNumber (JsonNumberInt 2)
  toJson Information = JsonNumber (JsonNumberInt 3)
  toJson Hint = JsonNumber (JsonNumberInt 4)

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

-- newtype CompletionItem = CompletionItem Binder

instance ToJson CompletionItemKind where
  toJson CompletionItemKindText = JsonNumber (JsonNumberInt 1)
  toJson CompletionItemKindMethod = JsonNumber (JsonNumberInt 2)
  toJson CompletionItemKindFunction = JsonNumber (JsonNumberInt 3)
  toJson CompletionItemKindConstructor = JsonNumber (JsonNumberInt 4)
  toJson CompletionItemKindField = JsonNumber (JsonNumberInt 5)
  toJson CompletionItemKindVariable = JsonNumber (JsonNumberInt 6)
  toJson CompletionItemKindClass = JsonNumber (JsonNumberInt 7)
  toJson CompletionItemKindInterface = JsonNumber (JsonNumberInt 8)
  toJson CompletionItemKindModule = JsonNumber (JsonNumberInt 9)
  toJson CompletionItemKindProperty = JsonNumber (JsonNumberInt 10)
  toJson CompletionItemKindUnit = JsonNumber (JsonNumberInt 11)
  toJson CompletionItemKindValue = JsonNumber (JsonNumberInt 12)
  toJson CompletionItemKindEnum = JsonNumber (JsonNumberInt 13)
  toJson CompletionItemKindKeyword = JsonNumber (JsonNumberInt 14)
  toJson CompletionItemKindSnippet = JsonNumber (JsonNumberInt 15)
  toJson CompletionItemKindColor = JsonNumber (JsonNumberInt 16)
  toJson CompletionItemKindFile = JsonNumber (JsonNumberInt 17)
  toJson CompletionItemKindReference = JsonNumber (JsonNumberInt 18)
  toJson CompletionItemKindFolder = JsonNumber (JsonNumberInt 19)
  toJson CompletionItemKindEnumMember = JsonNumber (JsonNumberInt 20)
  toJson CompletionItemKindConstant = JsonNumber (JsonNumberInt 21)
  toJson CompletionItemKindStruct = JsonNumber (JsonNumberInt 22)
  toJson CompletionItemKindEvent = JsonNumber (JsonNumberInt 23)
  toJson CompletionItemKindOperator = JsonNumber (JsonNumberInt 24)
  toJson CompletionItemKindTypeParameter = JsonNumber (JsonNumberInt 25)

instance ToJson SymbolKind where
  toJson SymbolKindFile = JsonNumber (JsonNumberInt 1)
  toJson SymbolKindModule = JsonNumber (JsonNumberInt 2)
  toJson SymbolKindNamespace = JsonNumber (JsonNumberInt 3)
  toJson SymbolKindPackage = JsonNumber (JsonNumberInt 4)
  toJson SymbolKindClass = JsonNumber (JsonNumberInt 5)
  toJson SymbolKindMethod = JsonNumber (JsonNumberInt 6)
  toJson SymbolKindProperty = JsonNumber (JsonNumberInt 7)
  toJson SymbolKindField = JsonNumber (JsonNumberInt 8)
  toJson SymbolKindConstructor = JsonNumber (JsonNumberInt 9)
  toJson SymbolKindEnum = JsonNumber (JsonNumberInt 10)
  toJson SymbolKindInterface = JsonNumber (JsonNumberInt 11)
  toJson SymbolKindFunction = JsonNumber (JsonNumberInt 12)
  toJson SymbolKindVariable = JsonNumber (JsonNumberInt 13)
  toJson SymbolKindConstant = JsonNumber (JsonNumberInt 14)
  toJson SymbolKindString = JsonNumber (JsonNumberInt 15)
  toJson SymbolKindNumber = JsonNumber (JsonNumberInt 16)
  toJson SymbolKindBoolean = JsonNumber (JsonNumberInt 17)
  toJson SymbolKindArray = JsonNumber (JsonNumberInt 18)
  toJson SymbolKindObject = JsonNumber (JsonNumberInt 19)
  toJson SymbolKindKey = JsonNumber (JsonNumberInt 20)
  toJson SymbolKindNull = JsonNumber (JsonNumberInt 21)
  toJson SymbolKindEnumMember = JsonNumber (JsonNumberInt 22)
  toJson SymbolKindStruct = JsonNumber (JsonNumberInt 23)
  toJson SymbolKindEvent = JsonNumber (JsonNumberInt 24)
  toJson SymbolKindOperator = JsonNumber (JsonNumberInt 25)
  toJson SymbolKindTypeParameter = JsonNumber (JsonNumberInt 26)

data Position = Position
  { positionLine :: Int,
    positionCharacter :: Int
  }

data Range = Range
  { rangeStart :: Position,
    rangeEnd :: Position
  }

data Diagnostic = Diagnostic
  { diagnosticSeverity :: DiagnosticSeverity,
    diagnosticMessage :: String,
    diagnosticRange :: Range,
    diagnosticCode :: Maybe String
  }

data PublishDiagnosticsParams = PublishDiagnosticsParams
  { publishDiagnosticsParamsUri :: String,
    publishDiagnosticsParamsDiagnostics :: [Diagnostic]
  }

instance ToJson Position where
  toJson position =
    JsonMap
      [ ("line", line),
        ("character", character)
      ]
    where
      line = toJson (max (positionLine position) 0)
      character = toJson (max (positionCharacter position) 0)

instance ToJson Range where
  toJson range =
    JsonMap
      [ ("start", start),
        ("end", end)
      ]
    where
      start = toJson (rangeStart range)
      end = toJson (rangeEnd range)

instance ToJson Diagnostic where
  toJson diagnostic =
    JsonMap
      [ ("severity", severity),
        ("message", message),
        ("range", range),
        ("code", code),
        ("source", source)
      ]
    where
      severity = toJson (diagnosticSeverity diagnostic)
      message = JsonString (diagnosticMessage diagnostic)
      range = toJson (diagnosticRange diagnostic)
      code = maybe JsonNull JsonString (diagnosticCode diagnostic)
      source = JsonString "carp"

instance ToJson PublishDiagnosticsParams where
  toJson publishDiagnosticsParams =
    JsonMap
      [ ("uri", uri),
        ("diagnostics", diagnostics)
      ]
    where
      uri =
        JsonString
          ( publishDiagnosticsParamsUri
              publishDiagnosticsParams
          )
      diagnostics =
        JsonList
          ( map
              toJson
              (publishDiagnosticsParamsDiagnostics publishDiagnosticsParams)
          )

instance Show PublishDiagnosticsParams where
  show = show . toJson

data MarkupKind = Plaintext | Markdown

data MarkupContent = MarkupContent
  { markupContentKind :: MarkupKind,
    markupContentValue :: String
  }

data Hover = Hover
  { hoverContents :: MarkupContent,
    hoverRange :: Range
  }

instance Show MarkupKind where
  show Plaintext =
    "plaintext"
  show Markdown =
    "markdown"

instance ToJson MarkupContent where
  toJson markupContent =
    JsonMap
      [ ("kind", JsonString (show (markupContentKind markupContent))),
        ("value", JsonString (markupContentValue markupContent))
      ]

instance ToJson Hover where
  toJson hover =
    JsonMap
      [ ("contents", toJson (hoverContents hover)),
        ("range", toJson (hoverRange hover))
      ]

instance Show Hover where
  show = show . toJson

data SymbolTag = Deprecated

instance Show SymbolTag where
  show Deprecated = "1"

data Location = Location
  { locationUri :: String,
    locationRange :: Range
  }

data SymbolInformation = SymbolInformation
  { symbolInformationName :: String,
    symbolInformationKind :: SymbolKind,
    symbolInformationTags :: [SymbolTag],
    symbolInformationLocation :: Location
  }

instance ToJson Location where
  toJson location =
    json
    where
      uri = locationUri location
      range = locationRange location
      json = JsonMap [("uri", JsonString uri), ("range", toJson range)]

instance ToJson SymbolInformation where
  toJson symbolInformation =
    JsonMap
      [ ("name", name),
        ("kind", kind),
        ("tags", tags),
        ("location", location)
      ]
    where
      name = JsonString (symbolInformationName symbolInformation)
      kind = toJson (symbolInformationKind symbolInformation)
      tags =
        JsonList
          ( map
              (JsonString . show)
              (symbolInformationTags symbolInformation)
          )
      uri = locationUri (symbolInformationLocation symbolInformation)
      range = locationRange (symbolInformationLocation symbolInformation)
      location =
        JsonMap
          [ ("uri", JsonString uri),
            ("range", toJson range)
          ]

data CompletionItem = CompletionItem
  { completionItemLabel :: String,
    completionItemKind :: CompletionItemKind,
    completionItemDetail :: String,
    completionItemDocumentation :: MarkupContent
  }

instance Show CompletionItem where
  show _completionItem =
    show item
    where
      item = JsonNull
