-- | A library to do stuff.
module Lib
    (
      className, xmlDependencies, ParsedClass(..), ClassName(..), XmlDependency(..)
    ) where

import Language.Java.Syntax
import Language.Java.Parser
import Data.List
import Data.Maybe
import Control.Monad
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

newtype ClassName = ClassName String deriving (Eq, Show)
newtype XmlDependency = XmlDependency String deriving (Eq, Show)
data ParsedClass = ParsedClass ClassName [XmlDependency] deriving (Eq, Show)


-- | Recursivly find all Java *IT files in the given directory
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (isSuffixOf "IT.java") $ filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)


-- | Return a list of ParsedClass's for the given top directory
parsedClasses :: FilePath -> IO [ParsedClass]
parsedClasses f = do
  files <- getRecursiveContents f
  pFiles <- mapM readFile files
  return $ map (xmlDependencies . p) pFiles
    where
      p fn = case (parser compilationUnit fn) of
               Left e -> error $ show e ++ ">>"
               Right cu -> cu


-- | Return all XML dependencies from the given Java class
xmlDependencies :: CompilationUnit -> ParsedClass
xmlDependencies cu@(CompilationUnit _ _ t) = ParsedClass (ClassName (className cu)) (nub $ concatMap mapType t)
  where
    mapType :: TypeDecl -> [XmlDependency]
    mapType (ClassTypeDecl (ClassDecl _ _ _ _ _ (ClassBody cb))) = concatMap mapMethod cb
    mapType _ = []
    mapMethod :: Decl -> [XmlDependency]
    mapMethod (MemberDecl (MethodDecl a _ _ _ _ _ _)) = concatMap mapAnnotation a
    mapMethod _ = []


-- | Returns a list of XmlDependency if it is a "DataSet" annotation
mapAnnotation :: Modifier -> [XmlDependency]
mapAnnotation (Annotation NormalAnnotation {annName = Name identNames, annKV = kv}) =
  case annoName identNames of
    "DataSet" -> concatMap parseDataSetAnno kv
    _ -> []
  where
    annoName ((Ident n):[]) = n
    annoName _ = ""
mapAnnotation _ = []


-- | Returns a list of XmlDependency for an "insert" attribute
parseDataSetAnno :: (Ident, ElementValue) -> [XmlDependency]
parseDataSetAnno (Ident "inserts", EVVal (InitArray (ArrayInit fileList))) = concatMap parseFileName fileList
  where
    parseFileName (InitExp (Lit (String s))) = [XmlDependency s]
    parseFileName _ = []
parseDataSetAnno _ = []


-- | Returns the name of the compilation unit
className ::CompilationUnit -> String
className (CompilationUnit _ _ typeDecls) =
  case typeDecls of
    [t] -> name t -- Just a single class
    _   -> case (find isMainClass typeDecls) of
              Just t  -> name t
              Nothing -> error "No public class found in CompilationUnit"
   where
     isMainClass (ClassTypeDecl (ClassDecl modis _ _ _ _ _)) = isJust $ find (\a -> a == Public) modis
     isMainClass _ = False
     name (ClassTypeDecl ((ClassDecl _ (Ident n) _ _ _ _))) = n
     name _ = error "Could not derive class name"
