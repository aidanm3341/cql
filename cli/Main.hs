{-
SPDX-License-Identifier: AGPL-3.0-only

This file is part of `statebox/cql`, the categorical query language.

Copyright (C) 2019 Stichting Statebox <https://statebox.nl>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}


--{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Language.CQL

import           Data.Aeson --hiding (Options)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

--------------------------
import Options.Applicative
import Language.CQL.Common
import Language.CQL.Program
import Data.Semigroup ((<>))

data Args = Args
  { format :: String
  , files :: [String] }

-- args :: Parser Bool
-- args = switch
--           ( long "json"
--          <> help "Output to the json format" )
args :: Parser Args
args = Args 
    <$> strOption
      (  long "format"
      <> help "which format to output to"
      <> showDefault
      <> value "cql"
      <> metavar "FORMAT_TYPE")
    <*> some (argument str (metavar "FILES"))


main :: IO ()
main = greet =<< execParser opts
 where
   opts = info (args <**> helper)
     ( fullDesc
    -- <> progDesc "A decription for CQL"
    <> header "CQL - Categorical Query Language" )


greet :: Args -> IO ()
greet (Args "json" s) = output outputJSON s
greet (Args _ s) = output outputCQL s

output :: (Err (Prog, Types, Env) -> String) -> [String] -> IO () 
output f s = do
  src  <- mapM readFile s
  _    <- mapM (putStrLn . f . runProg) src
  return ()


outputJSON :: Err (Prog, Types, Env) -> String 
outputJSON r = case r of
      Right (_, _, env) ->
        B.unpack (encodePretty (toJSON env))
      Left err -> err

outputCQL :: Err (Prog, Types, Env) -> String
outputCQL r = case r of
      Right (_, types, env) ->
        "////////////////////////////////////////////////////////////////////////////////\n" ++
        "// types                                                                      //\n" ++
        "////////////////////////////////////////////////////////////////////////////////\n" ++
        "\n" ++
        "\n" ++
        show types ++
        "////////////////////////////////////////////////////////////////////////////////\n" ++
        "// environment                                                                //\n" ++
        "////////////////////////////////////////////////////////////////////////////////\n" ++
        "\n" ++
        show env
      Left err -> err

---------------------

-- cql :: IO ()
-- cql = do
--   args <- getArgs
--   src  <- mapM readFile args
--   _    <- mapM (putStrLn . showResult . runProg) src
--   return ()
--   where
--     showResult r = case r of
--       Right (_, types, env) ->
--         "////////////////////////////////////////////////////////////////////////////////\n" ++
--         "// types                                                                      //\n" ++
--         "////////////////////////////////////////////////////////////////////////////////\n" ++
--         "\n" ++
--         "\n" ++
--         show types ++
--         "////////////////////////////////////////////////////////////////////////////////\n" ++
--         "// environment                                                                //\n" ++
--         "////////////////////////////////////////////////////////////////////////////////\n" ++
--         "\n" ++
--         show env
--       Left err -> err
