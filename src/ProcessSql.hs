{-# LANGUAGE DeriveDataTypeable #-}

module ProcessSql where

import Data.Generics
import Data.Generics.Aliases
import Data.Data
import Data.Generics.Basics
import Data.Generics.Schemes
import Data.Maybe

import Type

get_table_name :: TableName -> [String]
get_table_name (TableName name) = [name]

get_all_tables :: SQL -> [String]
get_all_tables sql = filter (/= "") $ (everything (++) (mkQ [""] get_table_name) sql)
