{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Json where

import qualified Data.List as List

data Json
  = JsonString String
  | JsonNumber String
  | JsonMap [(String, Json)]
  | JsonBool Bool
  | JsonList [Json]
  | JsonNull

printJson :: Json -> String
printJson (JsonString s) = show s
printJson (JsonNumber n) = n
printJson (JsonBool b) = if b then "true" else "false"
printJson JsonNull = "null"
printJson (JsonList nodes) =
  "[" ++ loop nodes ++ "]"
  where
    loop collection = case collection of
      [] -> ""
      (x : xs) ->
        printJson x
          ++ List.foldl
            ( \memo b ->
                memo
                  ++ ","
                  ++ printJson b
            )
            ""
            xs
printJson (JsonMap kvs) =
  "{" ++ loop kvs ++ "}"
  where
    loop nodes = case nodes of
      [] -> ""
      ((k, v) : rest) ->
        printJson (JsonString k) ++ ":" ++ printJson v
          ++ List.foldl
            ( \memo json ->
                memo ++ ","
                  ++ printJson (JsonString (fst json))
                  ++ ":"
                  ++ printJson (snd json)
            )
            ""
            rest
