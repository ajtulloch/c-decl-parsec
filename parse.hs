import           Control.Applicative
import           Text.ParserCombinators.Parsec hiding ((<|>))

data Ty = Ty String deriving (Show)
data FnTy = FnTy String [(Ty, String)] Ty deriving (Show)
data StructTy = StructTy String [(Ty, String)] deriving (Show)
data Stmt = Fn FnTy | Struct StructTy deriving (Show)

spaced :: Parser a -> Parser a
spaced = between spaces spaces

parseStmt :: Parser Stmt
parseStmt = Struct <$> parseStruct <|> Fn <$> parseFn

parseStruct :: Parser StructTy
parseStruct = StructTy
              <$> (string "struct" *> spaced (many1 letter))
              <*> between (char '{') (char '}') parseFields
      where
        parseFields = spaced $ parseNamedTy `endBy` (char ';' *> spaces)

parseNamedTy :: Parser (Ty, String)
parseNamedTy = (,) <$> parseTy <*> spaced (many1 letter)

parseFn :: Parser FnTy
parseFn = fnty
          <$> parseTy
          <*> (spaces *> many1 letter)
          <*> between (char '(') (char ')') parseArgs
    where
      fnty retTy name args = FnTy name args retTy
      parseArgs = parseNamedTy `sepBy` (char ',' *> spaces)

parseTy :: Parser Ty
parseTy = Ty <$> many1 letter

main :: IO ()
main = do
  print $ p parseTy "int"
  print $ p parseNamedTy "int apple"
  print $ p parseFn "void apple(int a, double b)"
  print $ p parseStruct "struct A { int apple; double berry; }"
  print $ p parseStmt "struct A { int apple; double berry; }"
  print $ p parseStmt "void apple(int a, double b)"
      where
        p parser = parse parser ""
