module LibSpec (spec,main) where

import Language.Java.Parser
import Language.Java.Syntax
import Test.Hspec
import Data.List
import Lib (className,xmlDependencies,XmlDependency(..),ParsedClass(..),ClassName(..))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll (readFile "test/ColsAccessServicesIT.java") $ do
    describe "Lib" $ do
      it "Gets the correct name of the class" $ \j -> do
        let c = parser compilationUnit j
        case c of
          Right cu -> (className cu) `shouldBe` "ColsAccessServicesIT"
      it "Returns the correct XML dependecies" $ \j -> do
        let c = parser compilationUnit j
        case c of
          Right cu -> (xmlDependencies cu) `shouldBe`
            (ParsedClass (ClassName "ColsAccessServicesIT")
              [
                 XmlDependency "testInputData/cpeData.xml"
                ,XmlDependency "testInputData/bperData_from_dataExport.xml"
                ,XmlDependency "testInputData/testUser.xml"
                ,XmlDependency "testInputData/CpeLanPortName_ForTranslationCheck.xml"
              ]
            )
