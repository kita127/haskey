module Haskey.Parser
(
  parse
) where

import           Control.Applicative
import qualified Data.Text           as T
import qualified Haskey.Ast          as Ast
import qualified Haskey.Token        as Tk
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

instance Alternative Parser where
  -- many :: f a -> f [a]
  -- | (<|>)
  pa <|> pb = Parser (\input -> case runParser pa input of
                           r@(Done _ _) -> r
                           (Fail _ _)   -> runParser pb input)

-- | nextToken
--
-- 先頭のトークンを返し入力を消費する
--
nextToken :: Parser Tk.Token
nextToken = Parser (\input -> case input of
                           []     -> Fail "empty imput" []
                           (x:xs) -> Done x xs)

-- | curToken
--
-- 先頭のトークンを返すが入力を消費しない
--
curToken :: Parser Tk.Token
curToken = Parser (\input -> case input of
                           []    -> Fail "empty imput" []
                           (x:_) -> Done x input)



----------------------------------------------------------------------------------------------------

-- | Precedence
--
data Precedence = Lowest
                | Equals            -- ==
                | LessGreater       -- > or <
                | Sum               -- +
                | Product           -- *
                | Prefix            -- -X or !X
                | Call              -- myFunction(X)
                deriving (Eq, Show, Ord)

-- | parse
--
parse :: [Tk.Token] -> Ast.Program
parse = Ast.program . result
  where
    result ts
        | (Tk.tokenIs Tk.Eof . head) ts = []
        | otherwise = case runParser parseStatement ts of
            (Done a r)           -> a : result r
            (Fail reason (r:rs)) -> newFailStmt r reason : result rs

-- | newFailStmt
--
newFailStmt :: Tk.Token -> String -> Ast.Statement
newFailStmt = Ast.FailStatement

-- | parseStatement
--
parseStatement :: Parser Ast.Statement
parseStatement = do
    t <- curToken
    case Tk.tokenType t of
        Tk.Let    -> parseLetStatement
        Tk.Return -> parseReturnStatement
        _         -> parseExpressionStatement

-- | parseExpressionStatement
--
parseExpressionStatement :: Parser Ast.Statement
parseExpressionStatement = do
    t <- curToken
    exp <- parseExpression Lowest
    consumeOneOrZero parseSemicolon
    return $ Ast.ExpressionStatement t exp


-- | parseExpression
--
parseExpression :: Precedence -> Parser Ast.Expression
parseExpression _ = prefixExpression

-- prefixExpression
--
prefixExpression :: Parser Ast.Expression
prefixExpression = do
    t <- curToken
    case Tk.tokenType t of
        Tk.Ident -> parseIdentifire
        Tk.Int -> parseIntegerLiteral
        Tk.Bang -> parsePrefixExpression
        Tk.Minus -> parsePrefixExpression
        _ -> fail . printf "no prefix parse function for %s found" . show . Tk.tokenType $ t

-- | parseReturnStatement
--
parseReturnStatement :: Parser Ast.Statement
parseReturnStatement = do
    t <- parseReturn
    -- value <- parseExpression

    -- TODO: セミコロンまで読み飛ばしている
    takeWhileToken Tk.Semicolon
    parseSemicolon

    return $ Ast.ReturnStatement t Ast.Nil

-- | parseReturn
--
parseReturn :: Parser Tk.Token
parseReturn = parseToken Tk.Return

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

-- | parsePrefixExpression
--
parsePrefixExpression :: Parser Ast.Expression
parsePrefixExpression = do
    t <- curToken
    nextToken
    r <- parseExpression Prefix
    return $ Ast.PrefixExpression t (Tk.literal t) r

-- | parseIdentifire
--
parseIdentifire :: Parser Ast.Expression
parseIdentifire = do
    t <- parseToken Tk.Ident
    return $ Ast.Identifire t (Tk.literal t)

-- | parseIntegerLiteral
--
parseIntegerLiteral :: Parser Ast.Expression
parseIntegerLiteral = do
    t <- curToken
    v <- parseInteger
    return $ Ast.IntegerLiteral t v

-- | parseInteger
--
parseInteger :: Parser Integer
parseInteger = do
    v <- parseToken Tk.Int
    let intV = read . T.unpack . Tk.literal $ v :: Integer
    return intV

-- | parseToken
--
-- TODO:
-- Token 全て表示すると情報過多のためタイプだけとかに検討する
--
parseToken :: Tk.TokenType -> Parser Tk.Token
parseToken expected = do
    t <- curToken
    if Tk.tokenIs expected t
        then nextToken
        else fail (printf "invalid token:%s expected token type:%s" (show t) (show expected))

-- | takeWhileToken
--
takeWhileToken :: Tk.TokenType -> Parser ()
takeWhileToken target = do
    t <- curToken
    if Tk.tokenIs target t
        then return ()
        else nextToken >> takeWhileToken target

-- | consumeOneOrZero
--
consumeOneOrZero :: Parser a -> Parser ()
consumeOneOrZero p = p >> pure () <|> pure ()
