module Haskey.Parser
(
  parse
) where

import qualified Haskey.Ast   as Ast
import qualified Haskey.Token as Tk
import           Text.Printf

----------------------------------------------------------------------------------------------------
-- Parser combinator

newtype Parser a = Parser { runParser :: [Tk.Token] -> Result a}

data Result a = Done a Remaining
              | Fail Reason  Remaining
  deriving (Eq, Show)

type Remaining = [Tk.Token]
type Reason = String

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = Parser (\input -> case runParser p input of
                            (Fail reason remaining)        -> Fail reason remaining
                            (Done result remaining) -> Done (g result) remaining)

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = Parser (\input -> Done v input)

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = Parser (\input -> case runParser pg input of
                             (Fail reason remaining)        -> Fail reason remaining
                             (Done result remaining) -> runParser (fmap result px) remaining)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = Parser (\input -> case runParser p input of
                           (Fail reason remaining)        -> Fail reason remaining
                           (Done result remaining) -> runParser (f result) remaining)

   -- return :: a -> Parser a
   -- return's default implementation is pure

   -- fail :: String -> m a
   fail s = Parser (\input -> Fail s input)

-- | item
--
-- 先頭のトークンを返し入力を消費する
--
item :: Parser Tk.Token
item = Parser (\input -> if null input
                           then Fail "empty imput" []
                           else Done (head input) (tail input))

-- | peek
--
-- 先頭のトークンを返すが入力を消費しない
--
peek :: Parser Tk.Token
peek = Parser (\input -> if null input
                            then Fail "empty imput" []
                            else Done (head input) input)

----------------------------------------------------------------------------------------------------

-- | parse
--
-- TODO:
-- error 関数を最終的にはとりのぞく
--
parse :: [Tk.Token] -> Ast.Program
parse = Ast.program . result
  where
    result ts'
        | (Tk.tokenIs Tk.Eof . head) ts' = []
        | otherwise = case runParser parseStatement ts' of
            (Done a r)              -> a : result r
            (Fail reason remaining) -> error reason

-- | parseStatement
--
parseStatement :: Parser Ast.Statement
parseStatement = parseLetStatement


-- | parseLetStatement
--
parseLetStatement :: Parser Ast.Statement
parseLetStatement = do
    t <- parseLet
    name <- parseIdentifire
    parseAssign
--    value <- parseExpression

    -- TODO: セミコロンまで読み飛ばしている
    takeWhileToken Tk.Semicolon
    parseSemicolon
    return $ Ast.LetStatement t name Ast.Nil


-- | parseLet
--
parseLet :: Parser Tk.Token
parseLet = parseToken Tk.Let

-- | parseAssign
--
parseAssign :: Parser Tk.Token
parseAssign = parseToken Tk.Assign

-- | parseSemicolon
--
parseSemicolon :: Parser Tk.Token
parseSemicolon = parseToken Tk.Semicolon

-- | parseIdentifire
--
parseIdentifire :: Parser Ast.Expression
parseIdentifire = do
    t <- parseToken Tk.Ident
    return $ Ast.Identifire t (Tk.literal t)

-- | parseToken
--
parseToken :: Tk.TokenType -> Parser Tk.Token
parseToken expected = do
    t <- peek
    if Tk.tokenIs expected t
        then item
        else fail (printf "invalid token:%s expected token type:%s" (show t) (show expected))

-- | takeWhileToken
--
takeWhileToken :: Tk.TokenType -> Parser ()
takeWhileToken target = do
    t <- peek
    if Tk.tokenIs target t
        then return ()
        else item >> takeWhileToken target

