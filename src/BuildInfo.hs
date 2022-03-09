{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module BuildInfo where

import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (isNothing)
import Env (binders)
import Info (Info (infoColumn, infoFile, infoLine))
import Json
import Json (Json (JsonBool))
import qualified Map
import Obj
  ( Binder (..),
    Env,
    MetaData (getMeta),
    XObj (..),
    isMod,
    pretty,
  )
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

xObjToPairs :: XObj -> [(String, Json)]
xObjToPairs xObj =
  let type_ = case xobjTy xObj of
        Nothing -> JsonNull
        Just t -> JsonString (show (beautifyType t))
      info = maybe JsonNull infoToJson (xobjInfo xObj)
   in [ ("type", type_),
        ("info", info),
        ("isBuiltIn", JsonBool (not (isMod xObj) && isNothing (xobjInfo xObj)))
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

fromEnv :: Env -> String
fromEnv e =
  e
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
