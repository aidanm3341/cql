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
import           System.Environment

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

--------------------------
import Options.Applicative
import Data.Semigroup ((<>))

-- data Args = Args
--   { json :: Bool }

args :: Parser Bool
args = switch
          ( long "json"
         <> help "Output to the json format" )


main :: IO ()
main = greet =<< execParser opts
 where
   opts = info (args <**> helper)
     ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

greet :: Bool -> IO ()
greet True = outputJSON --putStrLn $ "{'test' : 'boom'}"
greet False = cql


outputJSON :: IO ()
outputJSON = do
  -- args <- getArgs
  src  <- mapM readFile ["examples/Employee.cql"] -- args
  _    <- mapM (putStrLn . showResult . runProg) src
  return ()
  where
    showResult r = case r of
      Right (_, types, env) ->
        B.unpack (encodePretty (toJSON types))
        ++
        B.unpack (encodePretty (toJSON env))
      Left err -> err

---------------------

cql :: IO ()
cql = do
  args <- getArgs
  src  <- mapM readFile args
  _    <- mapM (putStrLn . showResult . runProg) src
  return ()
  where
    showResult r = case r of
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
