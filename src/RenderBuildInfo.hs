{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module RenderBuildInfo where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Info (Info (infoColumn, infoFile, infoLine))
import Json
import qualified Map
import Obj
  ( Binder (..),
    Env (envBindings, envModuleName),
    MetaData (getMeta),
    Obj (Mod),
    XObj (..),
    isMod,
    pretty,
  )
import Path
import Project
import TypeError (typeVariablesInOrderOfAppearance)
import Types

-- TODO: Move the beautification to a much earlier place, preferably when the function is defined/concretized-
-- This might be a duplicate with the work in a PR by @jacereda
beautifyType :: Ty -> Ty
beautifyType t =
  let tys = List.nub (typeVariablesInOrderOfAppearance t)
      mappings =
        Map.fromList
          ( List.zip
              (List.map (\(VarTy name) -> name) tys)
              (List.map (VarTy . (: [])) ['a' ..])
          )
   in replaceTyVars mappings t

saveBuildInfoForEnvs :: Project -> [(SymPath, Binder)] -> IO ()
saveBuildInfoForEnvs ctx pathsAndEnvBinders =
  let dir = "__dev__"
      _dependencies =
        getDependenciesForEnvs
          ( Prelude.map
              (\(p, b) -> (p, fst (getEnvAndMetaFromBinder b)))
              pathsAndEnvBinders
          )
      -- pathsAndEnvBinders' = pathsAndEnvBinders ++ dependencies
      pathsAndEnvBinders' = pathsAndEnvBinders
      allEnvNames = fmap fst pathsAndEnvBinders'
   in do
        mapM_ (saveDocsForEnvBinder ctx dir allEnvNames) pathsAndEnvBinders'
        putStrLn ("Generated Json to '" ++ dir ++ "'")
  where
    getDependenciesForEnvs = concatMap getEnvDependencies
    getEnvDependencies (SymPath paths path, env) =
      let envs =
            Prelude.map
              (first (SymPath (paths ++ [path])))
              ( Prelude.filter
                  (\(_, Binder _ x) -> isMod x)
                  ( Map.toList (envBindings env)
                  )
              )
       in envs
            ++ getDependenciesForEnvs (Prelude.map (\(n, Binder _ (XObj (Mod env _) _ _)) -> (n, env)) envs)

-- | This function expects a binder that contains an environment, anything else is a runtime error.
getEnvAndMetaFromBinder :: Binder -> (Env, MetaData)
getEnvAndMetaFromBinder envBinder =
  case envBinder of
    Binder meta (XObj (Mod env _) _ _) -> (env, meta)
    _ -> error "Binder's not a module. This should be detected in 'commandSaveDocsInternal'."

getModuleName :: Env -> String
getModuleName env = fromMaybe "Global" (envModuleName env)

saveDocsForEnvBinder :: Project -> String -> [SymPath] -> (SymPath, Binder) -> IO ()
saveDocsForEnvBinder ctx dir moduleNames (envPath, envBinder) =
  do
    let moduleName = show envPath
        fullPath = dir </> moduleName ++ ".json"
        contents = envBinderToJson envBinder ctx (show envPath) moduleNames
        json = printJson contents
        _modules = ctx
    -- contents = "{ \"key\": \"here is my Json string\" }"
    -- writeFile fullPath string
    putStrLn ("Env path: " ++ show envPath)
    putStrLn ("Would generate module: " ++ moduleName ++ " to: " ++ fullPath)
    putStrLn "Contents: "
    -- putStrLn json
    createDirectoryIfMissing False dir
    when
      -- TODO: PUT THIS BACK
      True
      (writeFile fullPath json)

envBinderToJson :: Binder -> Project -> String -> [SymPath] -> Json
envBinderToJson env ctx moduleName moduleNames =
  toJson env ctx moduleName moduleNames & JsonMap
  where
    toJson env ctx moduleName moduleNames =
      let meta = binderMeta env
          xobj = binderXObj env
          metaJson = ("meta", metaToJson meta)
       in case xobjObj xobj of
            Mod env _ ->
              let jsonBindings =
                    envBindings env
                      & Map.toList
                      & List.map
                        ( JsonMap
                            . ( \(symbolName, binder) ->
                                  ("symbol", JsonString symbolName) :
                                  toJson binder ctx moduleName moduleNames
                              )
                        )
               in xObjToPairs xobj
                    ++ [ metaJson,
                         ("bindings", JsonList jsonBindings)
                       ]
            _ ->
              xObjToPairs xobj ++ [metaJson]

xObjToPairs :: XObj -> [(String, Json)]
xObjToPairs xObj =
  let type_ = case xobjTy xObj of
        Nothing -> JsonNull
        Just t -> JsonString (show (beautifyType t))
      info = maybe JsonNull infoToJson (xobjInfo xObj)
   in [ ("type", type_),
        ("info", info)
      ]

objToJson :: Obj -> String
objToJson _obj = ""

infoToJson :: Info.Info -> Json
infoToJson info =
  let file = JsonString (infoFile info)
      line = JsonNumber (show (infoLine info))
      column = JsonNumber (show (infoColumn info))
   in JsonMap
        [ ("line", line),
          ("column", column),
          ("file", file)
        ]

metaToJson :: MetaData -> Json
metaToJson meta =
  getMeta meta
    & Map.map pretty
    & Map.map JsonString
    & Map.toList
    & JsonMap
