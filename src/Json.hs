{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Json where

import qualified Data.List as List

data JsonNumberKind = JsonNumberInt Int | JsonNumberFloat Float

data Json
  = JsonString String
  | JsonNumber JsonNumberKind
  | JsonMap [(String, Json)]
  | JsonBool Bool
  | JsonList [Json]
  | JsonNull

class ToJson a where
  toJson :: a -> Json

instance ToJson Int where
  toJson i = JsonNumber (JsonNumberInt i)

instance ToJson Float where
  toJson i = JsonNumber (JsonNumberFloat i)

instance Show Json where
  show (JsonString s) = show s
  show (JsonNumber (JsonNumberInt n)) = show n
  show (JsonNumber (JsonNumberFloat n)) = show n
  show (JsonBool b) = if b then "true" else "false"
  show JsonNull = "null"
  show (JsonList nodes) =
    "[" ++ loop nodes ++ "]"
    where
      loop collection = case collection of
        [] -> ""
        (x : xs) ->
          show x
            ++ List.foldl
              ( \memo b ->
                  memo
                    ++ ","
                    ++ show b
              )
              ""
              xs
  show (JsonMap kvs) =
    "{" ++ loop kvs ++ "}"
    where
      loop nodes = case nodes of
        [] -> ""
        ((k, v) : rest) ->
          show (JsonString k) ++ ":" ++ show v
            ++ List.foldl
              ( \memo json ->
                  memo ++ ","
                    ++ show (JsonString (fst json))
                    ++ ":"
                    ++ show (snd json)
              )
              ""
              rest
