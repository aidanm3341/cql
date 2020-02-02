{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.CQL.AesonTest where
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B

import Language.CQL.Term

test2 :: IO ()
test2 = B.putStrLn $ encodePretty (toJSON ((Fk "a foreign key" (Var "yo")) :: Term String String String String String String String String))

-- test3 :: IO ()
-- test3 = B.putStrLn $ encodePretty (toJSON (Typeside ))
