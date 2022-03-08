{-# LANGUAGE OverloadedStrings #-}

module RenderBuildInfo where

import CMark
import Control.Monad (when)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text as Text
import Info (Info (infoColumn, infoFile, infoLine))
import Json
import qualified Map
import qualified Meta
import Obj
import Path
import Project
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import TypeError (typeVariablesInOrderOfAppearance)
import Types

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) x f = f x

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
  let dir = "json-outputs"
      dependencies = getDependenciesForEnvs (Prelude.map (\(p, b) -> (p, fst (getEnvAndMetaFromBinder b))) pathsAndEnvBinders)
      pathsAndEnvBinders' = pathsAndEnvBinders ++ dependencies
      allEnvNames = fmap fst pathsAndEnvBinders'
   in do
        mapM_ (saveDocsForEnvBinder ctx dir allEnvNames) pathsAndEnvBinders'
        putStrLn ("Generated Json to '" ++ dir ++ "'")
  where
    getDependenciesForEnvs = Prelude.concat . Prelude.map getEnvDependencies
    getEnvDependencies (SymPath ps p, e) =
      let envs =
            Prelude.map
              (\(n, b) -> (SymPath (ps ++ [p]) n, b))
              ( Prelude.filter
                  (\(_, Binder _ x) -> isMod x)
                  ( Map.toList (envBindings e)
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

headOfPage :: String -> H.Html
headOfPage css =
  H.head $
    do
      H.meta ! A.charset "UTF-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
      H.link ! A.rel "stylesheet" ! A.href (H.stringValue css)

getModuleName :: Env -> String
getModuleName env = fromMaybe "Global" (envModuleName env)

saveDocsForEnvBinder :: Project -> String -> [SymPath] -> (SymPath, Binder) -> IO ()
saveDocsForEnvBinder ctx dir moduleNames (envPath, envBinder) =
  do
    let moduleName = show envPath
        fullPath = dir </> moduleName ++ ".json"
        contents = envBinderToJson envBinder ctx (show envPath) moduleNames
        json = printJson contents
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
envBinderToJson envBinder ctx moduleName moduleNames =
  envBinderToJson2 envBinder ctx moduleName moduleNames & JsonMap

-- let meta = binderMeta envBinder
--     xobj = binderXObj envBinder
--     metaJson = Map.toList (Map.map xObjToJson (getMeta meta))
--  in case xobjObj xobj of
--       --  If it's a module, return
--       --   {
--       --     "type": "module",
--       --     "bindings": binding[],
--       --     "meta": meta
--       --   }
--       --   else
--       --   {
--       --     "bindings": binding[],
--       --     "meta": meta
--       --   }

--       Mod env _ ->
--         let jsonBindings =
--               envBindings env
--                 & Map.map
--                   ( \binder ->
--                       envBinderToJson binder ctx moduleName moduleNames
--                   )
--                 & Map.toList
--             _jsonBindings =
--               envBindings env
--                 & Map.map
--                   ( \binder ->
--                       envBinderToJson binder ctx moduleName moduleNames
--                   )
--                 & Map.toList
--                 & List.map (\(k, a) -> a)
--             json =
--               JsonMap
--                 [ ("type", JsonString "module"),
--                   ("meta", JsonMap metaJson),
--                   ("bindings", JsonMap jsonBindings)
--                 ]
--          in json
--       _ ->
--         JsonMap
--           [ ("meta", JsonMap metaJson),
--             ("info", xObjToJson xobj)
--           ]

envBinderToJson2 :: Binder -> Project -> String -> [SymPath] -> [(String, Json)]
envBinderToJson2 envBinder ctx moduleName moduleNames =
  let meta = binderMeta envBinder
      xobj = binderXObj envBinder
      metaJson = Map.toList (Map.map xObjToJson (getMeta meta))
   in case xobjObj xobj of
        Mod env _ ->
          -- This may be a source of bugs
          let _jsonBindings =
                envBindings env
                  & Map.toList
                  & List.map
                    ( JsonMap
                        . ( \(symbolName, binder) ->
                              [ ("symbol", JsonString symbolName),
                                ("bindings", envBinderToJson binder ctx moduleName moduleNames)
                              ]
                          )
                    )

              -- & List.concatMap
              --   ( \(k, binder) ->
              --       [("symbol" :: [Char], JsonString k), ("bindings", envBinderToJson binder ctx moduleName moduleNames)]
              --   )
              -- & List.map snd
              json =
                -- [("type", JsonString "module"), ("bindings", jsonBindings)]
                [("type", JsonString "module"), ("bindings", JsonList _jsonBindings)]
           in json
        _ ->
          [ ("meta", JsonMap metaJson),
            ("info", xObjToJson xobj)
          ]

xObjToJson :: XObj -> Json
xObjToJson xObj =
  let type_ = case xobjTy xObj of
        Nothing -> JsonNull
        Just t -> JsonString (show (beautifyType t))
      info = maybe JsonNull infoToJson (xobjInfo xObj)
   in JsonMap
        [ ("type", type_),
          ("info", info)
        ]

objToJson :: Obj -> String
objToJson _obj = ""

infoToJson :: Info.Info -> Json
infoToJson info =
  let file = JsonString (infoFile info)
      identifier = JsonNull
      line = JsonNumber (show (infoLine info))
      column = JsonNumber (show (infoColumn info))
   in JsonMap
        [ ("line", line),
          ("column", column),
          ("file", file),
          ("identifier", identifier)
        ]

-- printJson :: Json -> String
-- printJson (JsonString s) = "\"" ++ s ++ "\""
-- printJson JsonNull = "null"
-- printJson (JsonList nodes) =
--   "[" ++ loop nodes ++ "]"
--   where
--     loop collection = case collection of
--       [] -> ""
--       (x : xs) ->
--         printJson x
--           ++ List.foldl
--             (\memo b -> memo ++ ", " ++ printJson b)
--             ""
--             xs
-- printJson (JsonMap kvs) =
--   let x =
--         List.foldl
--           ( \memo (key, value) ->
--               let start = if memo == "" then "" else ",\n "
--                in memo ++ start ++ (printJson (JsonString key) ++ ": ") ++ printJson value
--           )
--           ""
--           kvs
--    in "{" ++ x ++ "}"

-- data Json
--   = JsonString String
--   | JsonMap [(String, Json)]
--   | JsonNull
--   | JsonList
--       [Json]

--  in H.docTypeHtml $
--       do
--         headOfPage css
--         H.body $
--           H.div ! A.class_ "content" $
--             do
--               H.div ! A.class_ "logo" $
--                 do
--                   H.a ! A.href (H.stringValue url) $
--                     H.img ! A.src (H.stringValue logo)
--                   --span_ "CARP DOCS FOR"
--                   H.div ! A.class_ "title" $ H.toHtml title
--                   moduleIndex moduleNames
--               H.div ! A.class_ "module" $
--                 do
--                   H.h1 (H.toHtml moduleName)
--                   H.div ! A.class_ "module-description" $ H.preEscapedToHtml moduleDescriptionHtml
--                   mapM_ (binderToHtml moduleName . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

-- envBinderToHtml :: Binder -> Project -> String -> [SymPath] -> H.Html
-- envBinderToHtml envBinder ctx moduleName moduleNames =
--   let (env, meta) = getEnvAndMetaFromBinder envBinder
--       title = projectTitle ctx
--       css = projectDocsStyling ctx
--       url = projectDocsURL ctx
--       logo = projectDocsLogo ctx
--       moduleDescription = case Meta.get "doc" meta of
--         Just (XObj (Str s) _ _) -> s
--         Nothing -> ""
--         _ -> error "moduledescription"
--       moduleDescriptionHtml = commonmarkToHtml [optSafe] $ Text.pack moduleDescription
--    in H.docTypeHtml $
--         do
--           headOfPage css
--           H.body $
--             H.div ! A.class_ "content" $
--               do
--                 H.div ! A.class_ "logo" $
--                   do
--                     H.a ! A.href (H.stringValue url) $
--                       H.img ! A.src (H.stringValue logo)
--                     --span_ "CARP DOCS FOR"
--                     H.div ! A.class_ "title" $ H.toHtml title
--                     moduleIndex moduleNames
--                 H.div ! A.class_ "module" $
--                   do
--                     H.h1 (H.toHtml moduleName)
--                     H.div ! A.class_ "module-description" $ H.preEscapedToHtml moduleDescriptionHtml
--                     mapM_ (binderToHtml moduleName . snd) (Prelude.filter shouldEmitDocsForBinder (Map.toList (envBindings env)))

shouldEmitDocsForBinder :: (String, Binder) -> Bool
shouldEmitDocsForBinder (_, Binder meta _) =
  not (metaIsTrue meta "hidden")

moduleIndex :: [SymPath] -> H.Html
moduleIndex moduleNames =
  H.div ! A.class_ "index" $ grouped moduleNames
  where
    grouped names = H.ul $ mapM_ gen (order names)
    gen (m, subs) =
      H.li $
        if Prelude.null subs
          then moduleLink m
          else H.details $
            do
              H.summary (moduleLink m)
              grouped subs
    order [] = []
    order (m : mods) =
      let (isIn, isNotIn) = List.partition (symBelongsToMod m) mods
       in (m, isIn) : order isNotIn
    symBelongsToMod (SymPath xs x) (SymPath ys y) =
      List.isPrefixOf (xs ++ [x]) (ys ++ [y])

moduleLink :: SymPath -> H.Html
moduleLink p@(SymPath _ name) =
  H.a ! A.href (H.stringValue (show p ++ ".html")) $ H.toHtml name

binderToHtml :: String -> Binder -> H.Html
binderToHtml moduleName (Binder meta xobj) =
  let name = getSimpleName xobj
      maybeNameAndArgs = getSimpleNameWithArgs xobj
      description = getBinderDescription xobj
      typeSignature = case xobjTy xobj of
        Just t -> show (beautifyType t) -- NOTE: This destroys user-defined names of type variables!
        Nothing -> ""
      isDeprecated = case Meta.get "deprecated" meta of
        Just (XObj (Bol True) _ _) -> True
        Just (XObj (Str _) _ _) -> True
        _ -> False
      deprecationStr = case Meta.get "deprecated" meta of
        Just (XObj (Str s) _ _) -> commonmarkToHtml [optSafe] $ Text.pack s
        _ -> ""
      docString = case Meta.get "doc" meta of
        Just (XObj (Str s) _ _) -> s
        Just found -> pretty found
        Nothing -> ""
      htmlDoc = commonmarkToHtml [optSafe] $ Text.pack docString
   in H.div ! A.class_ "binder" $
        do
          H.a ! A.class_ "anchor" ! A.href (H.stringValue ("#" ++ name)) $
            H.h3 ! A.id (H.stringValue name) $
              do
                if isMod xobj
                  then H.a ! A.href (H.stringValue (moduleName ++ "." ++ pretty xobj ++ ".html")) $ H.toHtml (pretty xobj)
                  else H.toHtml name
                when isDeprecated $
                  H.span ! A.class_ "deprecation-notice" $
                    H.toHtml ("deprecated" :: String)
          H.div ! A.class_ "description" $ H.toHtml description
          H.p ! A.class_ "sig" $ H.toHtml typeSignature
          case maybeNameAndArgs of
            Just nameAndArgs -> H.pre ! A.class_ "args" $ H.toHtml nameAndArgs
            Nothing -> H.span $ H.toHtml ("" :: String)
          H.p ! A.class_ "doc" $ H.preEscapedToHtml htmlDoc
          when isDeprecated $
            H.div ! A.class_ "deprecation-text" $
              H.preEscapedToHtml deprecationStr

-- p_ (toHtml (description))
