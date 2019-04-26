module Haskey.Parser
(
  parse
) where

import           Control.Applicative
import qualified Data.Map            as M
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

-- | peekToken
--
peekToken :: Parser Tk.Token
peekToken = Parser (\input -> case input of
                           (_:x:_) -> Done x input
                           _       -> Fail "empty imput" [])



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

-- | precedences
--
precedences :: M.Map Tk.TokenType Precedence
precedences = M.fromList [
                (Tk.Eq, Equals)
              , (Tk.NotEq, Equals)
              , (Tk.Lt, LessGreater)
              , (Tk.Gt, LessGreater)
              , (Tk.Plus, Sum)
              , (Tk.Minus, Sum)
              , (Tk.Slash, Product)
              , (Tk.Asterisk, Product)
              ]

-- | prefixParseFns
--
prefixParseFns :: M.Map Tk.TokenType (Parser Ast.Expression)
prefixParseFns = M.fromList [
                   (Tk.Ident, parseIdentifire)
                 , (Tk.Int, parseIntegerLiteral)
                 , (Tk.Bang, parsePrefixExpression)
                 , (Tk.Minus, parsePrefixExpression)
                 ]

-- | infixParseFns
--
infixParseFns :: M.Map Tk.TokenType (Ast.Expression -> Parser Ast.Expression)
infixParseFns = M.fromList [
                  (Tk.Plus, parseInfixExpression)
                , (Tk.Minus, parseInfixExpression)
                , (Tk.Slash, parseInfixExpression)
                , (Tk.Asterisk, parseInfixExpression)
                , (Tk.Eq, parseInfixExpression)
                , (Tk.NotEq, parseInfixExpression)
                , (Tk.Lt, parseInfixExpression)
                , (Tk.Gt, parseInfixExpression)
                ]


-- | parse
--
-- TODO:
-- parseProgram にしたい
--
parse :: [Tk.Token] -> Ast.Program
parse = Ast.program . result
  where
    result ts
        | (Tk.tokenIs Tk.Eof . head) ts = []
        | otherwise = case runParser parseStatement ts of
            -- 前の statement 最後のトークンで終わっているので次にトークンを進める
            (Done a (_:rs))      -> a : result rs
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
    expression <- parseExpression Lowest

    -- カレントトークンの次がセミコロンならそこまで進める
    (next . parsePeek) Tk.Semicolon <|> curToken
    return $ Ast.ExpressionStatement t expression


-- | parseExpression
--
parseExpression :: Precedence -> Parser Ast.Expression
parseExpression precedence = do
    t <- curToken
    let prefixFn  = M.findWithDefault defaultFn (Tk.tokenType t) prefixParseFns
        defaultFn = fail . printf "no prefix parse function for %s found" . show . Tk.tokenType $ t
    leftExp <- prefixFn
    prattParse precedence leftExp


-- | prattParse
--
prattParse :: Precedence -> Ast.Expression -> Parser Ast.Expression
prattParse precedence leftExp = do
    pk <- peekToken
    pkPre <- peekPrecedence
    if not (Tk.tokenIs Tk.Semicolon pk) && precedence < pkPre
    then do
        t <- next peekToken
        let infixFn   = M.findWithDefault defaultFn (Tk.tokenType t) infixParseFns
            defaultFn = fail . printf "no infix parse function for %s found" . show . Tk.tokenType $ t

        infixFn leftExp >>= prattParse precedence
    else return leftExp



-- | parseReturnStatement
--
parseReturnStatement :: Parser Ast.Statement
parseReturnStatement = do
    t <- next parseReturn
    -- value <- parseExpression

    -- TODO: セミコロンまで読み飛ばしている
    takeWhileToken Tk.Semicolon

    return $ Ast.ReturnStatement t Ast.Nil

-- | parseReturn
--
parseReturn :: Parser Tk.Token
parseReturn = parseToken Tk.Return

-- | parseLetStatement
--
parseLetStatement :: Parser Ast.Statement
parseLetStatement = do
    t <- next parseLet
    name <- next parseIdentifire
    next parseAssign
--    value <- parseExpression

    -- TODO: セミコロンまで読み飛ばしている
    takeWhileToken Tk.Semicolon
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

-- | parseInfixExpression
--
parseInfixExpression :: Ast.Expression -> Parser Ast.Expression
parseInfixExpression leftExp = do
    t <- curToken
    precedence <- next curPrecedence
    rightExp <- parseExpression precedence
    return $ Ast.InfixExpression t leftExp (Tk.literal t) rightExp

-- | parsePrefixExpression
--
parsePrefixExpression :: Parser Ast.Expression
parsePrefixExpression = do
    t <- next curToken
    r <- parseExpression Prefix
    return $ Ast.PrefixExpression t (Tk.literal t) r

-- | parseIdentifire
--
parseIdentifire :: Parser Ast.Expression
parseIdentifire = Ast.Identifire <$> curToken <*> fmap Tk.literal curToken

-- | parseIntegerLiteral
--
parseIntegerLiteral :: Parser Ast.Expression
parseIntegerLiteral = Ast.IntegerLiteral <$> curToken <*> parseInteger

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
        then return t
        else fail (printf "invalid token:%s expected token type:%s" (show t) (show expected))

-- | parsePeek
--
-- TODO:
-- Token 全て表示すると情報過多のためタイプだけとかに検討する
--
parsePeek :: Tk.TokenType -> Parser Tk.Token
parsePeek expected = do
    t <- peekToken
    if Tk.tokenIs expected t
        then return t
        else fail (printf "invalid peek token:%s expected peek token type:%s" (show t) (show expected))

-- | takeWhileToken
--
takeWhileToken :: Tk.TokenType -> Parser ()
takeWhileToken target = do
    t <- curToken
    if Tk.tokenIs target t
        then return ()
        else nextToken >> takeWhileToken target

-- | next
--
next :: Parser a -> Parser a
next p = p <* nextToken


-- | peekPrecedence
--
peekPrecedence :: Parser Precedence
peekPrecedence = do
    t <- peekToken
    case M.lookup (Tk.tokenType t) precedences of
        Just v  -> return v
        Nothing -> return Lowest

-- | curPrecedence
--
curPrecedence :: Parser Precedence
curPrecedence = do
    t <- curToken
    case M.lookup (Tk.tokenType t) precedences of
        Just v  -> return v
        Nothing -> return Lowest
