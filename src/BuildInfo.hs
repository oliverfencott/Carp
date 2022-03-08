{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BuildInfo where

import Data.Function ((&))
import qualified Data.List as List
import Env (binders)
import Info (Info (infoColumn, infoFile, infoLine))
import Json
import qualified Map
import Obj
  ( Binder (..),
    Context (contextGlobalEnv),
    Env (envBindings),
    MetaData (getMeta),
    Obj (Mod),
    XObj (..),
    pretty,
  )
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
  let pathsAndEnvBinders' = pathsAndEnvBinders
      -- allEnvNames = fmap fst pathsAndEnvBinders'
      _allEnvNames = fmap snd pathsAndEnvBinders'
      json = map (envBinderToJson ctx) pathsAndEnvBinders' & JsonList
   in putStrLn (printJson json)

envBinderToJson :: Project -> (SymPath, Binder) -> Json
envBinderToJson ctx (path, env) =
  toJson env ctx & JsonMap
  where
    toJson env ctx =
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
                                  toJson binder ctx
                              )
                        )
               in xObjToPairs xobj
                    ++ [ ("type", JsonString "module"),
                         ("symbol", JsonString (show path)),
                         metaJson,
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

fromEnv :: Context -> String
fromEnv e =
  e
    & contextGlobalEnv
    & binders
    & Map.toList
    & map
      ( JsonMap
          . ( \(k, binder) ->
                xObjToPairs (binderXObj binder)
                  ++ [ ("symbol", JsonString k),
                       ("meta", metaToJson (binderMeta binder))
                     ]
            )
      )
    & JsonList
    & printJson

-- & map
--   ( JsonMap
--       . ( \(k, binder) ->
--             xObjToPairs (binderXObj binder)
--               ++ [ ("symbol", JsonString k),
--                    ("meta", metaToJson (binderMeta binder))
--                  ]
--         )
--   )
