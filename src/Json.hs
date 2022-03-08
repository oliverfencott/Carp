{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

-- module RenderBuildInfo where
module Json where

import qualified Data.List as List

data Json
  = JsonString String
  | JsonNumber String
  | JsonMap [(String, Json)]
  | JsonNull
  | JsonList [Json]

printJson :: Json -> String
printJson (JsonString s) = show s
printJson (JsonNumber n) = n
printJson JsonNull = "null"
printJson (JsonList nodes) =
  "[" ++ loop nodes ++ "]"
  where
    loop collection = case collection of
      [] -> ""
      (x : xs) ->
        printJson x
          ++ List.foldl
            (\memo b -> memo ++ ", " ++ printJson b)
            ""
            xs
printJson (JsonMap kvs) =
  let x =
        List.foldl
          ( \memo (key, value) ->
              let start = if memo == "" then "" else ",\n "
               in memo ++ start ++ (printJson (JsonString key) ++ ": ") ++ printJson value
          )
          ""
          kvs
   in "{" ++ x ++ "}"
