module LanguageServer where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Info (Info (infoColumn, infoFile, infoLine), dummyInfo)
import Lsp
  ( CompletionItemKind
      ( CompletionItemKindEnum,
        CompletionItemKindField,
        CompletionItemKindFunction,
        CompletionItemKindInterface,
        CompletionItemKindKeyword,
        CompletionItemKindModule,
        CompletionItemKindStruct,
        CompletionItemKindTypeParameter,
        CompletionItemKindValue,
        CompletionItemKindVariable
      ),
    Diagnostic (Diagnostic),
    DiagnosticSeverity (Error, Warning),
    Position (Position),
    PublishDiagnosticsParams (PublishDiagnosticsParams),
    Range (Range),
    SymbolKind
      ( SymbolKindArray,
        SymbolKindBoolean,
        SymbolKindConstant,
        SymbolKindConstructor,
        SymbolKindEnum,
        SymbolKindEvent,
        SymbolKindFile,
        SymbolKindFunction,
        SymbolKindInterface,
        SymbolKindKey,
        SymbolKindModule,
        SymbolKindNull,
        SymbolKindNumber,
        SymbolKindObject,
        SymbolKindString,
        SymbolKindStruct,
        SymbolKindTypeParameter,
        SymbolKindVariable
      ),
    SymbolTag (..),
  )
import Meta (get)
import Obj (MetaData, Obj (..), XObj (XObj, xobjInfo))
import Types (Ty (..))

-- Transformers
makeWarningDiagnostic :: Maybe XObj -> String -> String
makeWarningDiagnostic xobj warning =
  show diagnostics
  where
    info = xobj >>= xobjInfo
    uri = maybeInfoToFileUri info
    diagnostic = Diagnostic Warning warning (maybeInfoToRange info) Nothing
    diagnostics = PublishDiagnosticsParams uri [diagnostic]

makeErrorDiagnosticWithLabel :: String -> Maybe Info -> String -> String
makeErrorDiagnosticWithLabel err info label =
  show diagnostics
  where
    uri = maybeInfoToFileUri info
    diagnostic = Diagnostic Error err (maybeInfoToRange info) (Just label)
    diagnostics = PublishDiagnosticsParams uri [diagnostic]

-- Conversion helpers
xobjToRange :: XObj -> Range
xobjToRange xobj =
  infoToRange (fromMaybe dummyInfo (xobjInfo xobj))

infoToRange :: Info -> Range
infoToRange info =
  Range
    (Position lineStart columnStart)
    (Position lineStart (columnStart + 1))
  where
    lineStart = max (infoLine info - 1) 0
    columnStart = max (infoColumn info - 1) 0 -- TODO: Default to see if it actually works. Figure out how to do this correctly

maybeInfoToRange :: Maybe Info -> Range
maybeInfoToRange info = infoToRange (fromMaybe dummyInfo info)

maybeInfoToFileUri :: Maybe Info -> String
maybeInfoToFileUri = maybe "" prependUriFile

prependUriFile :: Info -> String
prependUriFile = prependFileProtocol . infoFile

parseFilePath :: String -> String
parseFilePath rawPath =
  if protocol `isPrefixOf` rawPath
    then drop (length protocol) rawPath
    else rawPath

prependFileProtocol :: String -> String
prependFileProtocol rawPath = protocol ++ rawPath

protocol :: [Char]
protocol = "file://"

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

tagsFromMeta :: MetaData -> [SymbolTag]
tagsFromMeta meta =
  case Meta.get "deprecated" meta of
    Just (XObj (Bol True) _ _) -> [Deprecated]
    Nothing -> []
    Just _ -> []

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

objToCompletionItemKind :: Obj -> CompletionItemKind
objToCompletionItemKind C {} = CompletionItemKindValue
objToCompletionItemKind Lst {} = CompletionItemKindValue
objToCompletionItemKind Arr {} = CompletionItemKindValue
objToCompletionItemKind StaticArr {} = CompletionItemKindValue
objToCompletionItemKind Dict {} = CompletionItemKindStruct
objToCompletionItemKind Num {} = CompletionItemKindValue
objToCompletionItemKind Str {} = CompletionItemKindValue
objToCompletionItemKind Pattern {} = CompletionItemKindValue
objToCompletionItemKind Chr {} = CompletionItemKindValue
objToCompletionItemKind Sym {} = CompletionItemKindVariable
objToCompletionItemKind MultiSym {} = CompletionItemKindVariable
objToCompletionItemKind InterfaceSym {} = CompletionItemKindInterface
objToCompletionItemKind Bol {} = CompletionItemKindValue
objToCompletionItemKind Defn {} = CompletionItemKindFunction
objToCompletionItemKind Def = CompletionItemKindVariable
objToCompletionItemKind Fn {} = CompletionItemKindFunction
objToCompletionItemKind Closure {} = CompletionItemKindFunction
objToCompletionItemKind If = CompletionItemKindKeyword
objToCompletionItemKind Match {} = CompletionItemKindKeyword
objToCompletionItemKind While = CompletionItemKindKeyword
objToCompletionItemKind Do = CompletionItemKindKeyword
objToCompletionItemKind Let = CompletionItemKindKeyword
objToCompletionItemKind LocalDef = CompletionItemKindKeyword
objToCompletionItemKind Mod {} = CompletionItemKindModule
objToCompletionItemKind Deftype {} = CompletionItemKindTypeParameter
objToCompletionItemKind DefSumtype {} = CompletionItemKindEnum
objToCompletionItemKind Deftemplate {} = CompletionItemKindFunction
objToCompletionItemKind Instantiate {} = CompletionItemKindFunction
objToCompletionItemKind External {} = CompletionItemKindValue
objToCompletionItemKind ExternalType {} = CompletionItemKindValue
objToCompletionItemKind MetaStub = CompletionItemKindField
objToCompletionItemKind (Defalias _) = CompletionItemKindVariable
objToCompletionItemKind SetBang = CompletionItemKindValue
objToCompletionItemKind Macro = CompletionItemKindFunction
objToCompletionItemKind Dynamic = CompletionItemKindFunction
objToCompletionItemKind DefDynamic = CompletionItemKindFunction
objToCompletionItemKind (Command _) = CompletionItemKindFunction
objToCompletionItemKind (Primitive _) = CompletionItemKindValue
objToCompletionItemKind The = CompletionItemKindKeyword
objToCompletionItemKind Ref = CompletionItemKindFunction
objToCompletionItemKind Deref = CompletionItemKindFunction
objToCompletionItemKind Break = CompletionItemKindKeyword
objToCompletionItemKind (Interface _ _) = CompletionItemKindInterface
objToCompletionItemKind With = CompletionItemKindKeyword
