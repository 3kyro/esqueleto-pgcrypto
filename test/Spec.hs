{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Database.Esqueleto.Experimental
import qualified Database.Esqueleto.PostgreSQL.Pgcrypto as PGC
import Model
import Test.Hspec
import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll mkConnectionPool $ do
    testCrypt

testCrypt :: SpecDb
testCrypt =
    describe "Crypt password hashing functions" $ do
        itDb "authenticates a user" $ do
            rawExecute "CREATE EXTENSION IF NOT EXISTS pgcrypto" []
            _ <- insertSelect $ do
                pure $
                    User
                        <# val "name"
                        <&> PGC.toCrypt (PGC.BF $ Just 6) "1234password"
            authenticated <-
                select $ do
                    user' <- from $ table @User
                    where_ $
                        user' ^. UserUsername ==. val "name"
                            &&. PGC.fromCrypt (user' ^. UserPasswordHash) "1234password"
                    pure user'
            let authUser = entityVal $ head authenticated
            asserting $ do
                userUsername authUser `shouldBe` "name"
                userPasswordHash authUser `shouldNotBe` "1234password"
