{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Haskey.Parser
    ( parse
    )
where

import           Control.Applicative
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Haskey.Ast                    as Ast
import qualified Haskey.Token                  as Tok
import           Text.Printf
import           Data.Functor

----------------------------------------------------------------------------------------------------
-- Parser combinator

newtype Parser a = Parser { runParser :: [Tok.Token] -> Result a}

data Result a = Done a Remaining
              | Fail Reason  Remaining
  deriving (Eq, Show)

type Remaining = [Tok.Token]
type Reason = String

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = Parser
        (\input -> case runParser p input of
            (Fail reason remaining) -> Fail reason remaining
            (Done result remaining) -> Done (g result) remaining
        )

instance Applicative Parser where
   -- pure :: a -> Parser a
    pure v = Parser (Done v)

-- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = Parser
        (\input -> case runParser pg input of
            (Fail reason remaining) -> Fail reason remaining
            (Done result remaining) -> runParser (fmap result px) remaining
        )

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser
        (\input -> case runParser p input of
            (Fail reason remaining) -> Fail reason remaining
            (Done result remaining) -> runParser (f result) remaining
        )

-- return :: a -> Parser a
-- return's default implementation is pure

-- fail :: String -> m a
    fail s = Parser (Fail s)

instance Alternative Parser where
  -- many :: f a -> f [a]
  -- (<|>) :: f a -> f a -> f a
  -- empty :: f a
  --
    pa <|> pb = Parser
        (\input -> case runParser pa input of
            r@(Done _ _) -> r
            (  Fail _ _) -> runParser pb input
        )
    empty = Parser (\_ -> Fail "parse fail" [])

-- | nextToken
--
-- 先頭のトークンを返し入力を消費する
--
nextToken :: Parser Tok.Token
nextToken = Parser
    (\case
        []       -> Fail "empty imput" []
        (x : xs) -> Done x xs
    )

-- | curToken
--
-- 先頭のトークンを返すが入力を消費しない
--
curToken :: Parser Tok.Token
curToken = Parser
    (\input -> case input of
        []      -> Fail "empty imput" []
        (x : _) -> Done x input
    )

-- | peekToken
--
peekToken :: Parser Tok.Token
peekToken = Parser
    (\input -> case input of
        (_ : x : _) -> Done x input
        _           -> Fail "empty imput" []
    )



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
                | Index             -- array[index]
                deriving (Eq, Show, Ord)

-- | precedences
--
precedences :: M.Map Tok.TokenType Precedence
precedences = M.fromList
    [ (Tok.Eq      , Equals)
    , (Tok.NotEq   , Equals)
    , (Tok.Lt      , LessGreater)
    , (Tok.Gt      , LessGreater)
    , (Tok.Plus    , Sum)
    , (Tok.Minus   , Sum)
    , (Tok.Slash   , Product)
    , (Tok.Asterisk, Product)
    , (Tok.Lparen  , Call)
    , (Tok.Lbracket, Index)
    ]

-- | prefixParseFns
--
prefixParseFns :: M.Map Tok.TokenType (Parser Ast.Expression)
prefixParseFns = M.fromList
    [ (Tok.Ident   , parseIdentifire)
    , (Tok.Int     , parseIntegerLiteral)
    , (Tok.Bang    , parsePrefixExpression)
    , (Tok.Minus   , parsePrefixExpression)
    , (Tok.TRUE    , parseBoolLiteral)
    , (Tok.FALSE   , parseBoolLiteral)
    , (Tok.If      , parseIfExpression)
    , (Tok.Function, parseFunctionLiteral)
    , (Tok.Lparen  , parseGroupedExpression)
    , (Tok.STRING  , parseStringLiteral)
    , (Tok.Lbracket, parseArrayLiteral)
    ]

-- | infixParseFns
--
infixParseFns :: M.Map Tok.TokenType (Ast.Expression -> Parser Ast.Expression)
infixParseFns = M.fromList
    [ (Tok.Plus    , parseInfixExpression)
    , (Tok.Minus   , parseInfixExpression)
    , (Tok.Slash   , parseInfixExpression)
    , (Tok.Asterisk, parseInfixExpression)
    , (Tok.Eq      , parseInfixExpression)
    , (Tok.NotEq   , parseInfixExpression)
    , (Tok.Lt      , parseInfixExpression)
    , (Tok.Gt      , parseInfixExpression)
    , (Tok.Lparen  , parseCallExpression)
    , (Tok.Lbracket, parseIndexExpression)
    ]


-- | parse
--
-- TODO:
-- parseProgram にしたい
--
parse :: [Tok.Token] -> Ast.Program
parse = Ast.program . result
  where
    result []                     = []
    result [Tok.Token Tok.Eof ""] = []
    result ts                     = case runParser parseStatement ts of
            -- 前の statement 最後のトークンで終わっているので次にトークンを進める
        (Done a      (_ : rs)) -> a : result rs
        (Fail reason (r : rs)) -> newFailStmt r reason : result rs

-- | newFailStmt
--
newFailStmt :: Tok.Token -> String -> Ast.Statement
newFailStmt = Ast.FailStatement

-- | parseStatement
--
parseStatement :: Parser Ast.Statement
parseStatement = do
    t <- curToken
    case Tok.tokenType t of
        Tok.Let    -> parseLetStatement
        Tok.Return -> parseReturnStatement
        _          -> parseExpressionStatement

-- | parseLetStatement
--
parseLetStatement :: Parser Ast.Statement
parseLetStatement =
    Ast.LetStatement
        <$> next parseLet
        <*> next parseIdentifire
        <*  next parseAssign
        <*> parseExpression Lowest
        <*  goAheadIfNextSemicolon

-- | parseReturnStatement
--
parseReturnStatement :: Parser Ast.Statement
parseReturnStatement =
    Ast.ReturnStatement
        <$> next parseReturn
        <*> parseExpression Lowest
        <*  goAheadIfNextSemicolon

-- | parseExpressionStatement
--
parseExpressionStatement :: Parser Ast.Statement
parseExpressionStatement =
    Ast.ExpressionStatement
        <$> curToken
        <*> parseExpression Lowest
        <*  goAheadIfNextSemicolon


-- | parseExpression
--
parseExpression :: Precedence -> Parser Ast.Expression
parseExpression precedence = do
    t <- curToken
    let prefixFn = M.findWithDefault defaultFn (Tok.tokenType t) prefixParseFns
        defaultFn =
            fail
                . printf "no prefix parse function for %s found"
                . show
                . Tok.tokenType
                $ t
    leftExp <- prefixFn
    prattParse precedence leftExp


-- | prattParse
--
prattParse :: Precedence -> Ast.Expression -> Parser Ast.Expression
prattParse precedence leftExp = do
    pk    <- peekToken
    pkPre <- peekPrecedence
    if not (Tok.isToken Tok.Semicolon pk) && precedence < pkPre
        then do
            t <- next peekToken
            let infixFn =
                    M.findWithDefault defaultFn (Tok.tokenType t) infixParseFns
                defaultFn =
                    fail
                        . printf "no infix parse function for %s found"
                        . show
                        . Tok.tokenType
                        $ t

            infixFn leftExp >>= prattParse precedence
        else return leftExp

-- | parsePrefixExpression
--
parsePrefixExpression :: Parser Ast.Expression
parsePrefixExpression =
    Ast.PrefixExpression
        <$> curToken
        <*> next (fmap Tok.literal curToken)
        <*> parseExpression Prefix

-- | parseInfixExpression
--
parseInfixExpression :: Ast.Expression -> Parser Ast.Expression
parseInfixExpression leftExp =
    Ast.InfixExpression
        <$> curToken
        <*> pure leftExp
        <*> fmap Tok.literal curToken
        <*> (next curPrecedence >>= parseExpression)

-- | parseIdentifire
--
parseIdentifire :: Parser Ast.Expression
parseIdentifire = Ast.Identifire <$> expectCur Tok.Ident <*> fmap
    Tok.literal
    (expectCur Tok.Ident)

-- | parseIntegerLiteral
--
parseIntegerLiteral :: Parser Ast.Expression
parseIntegerLiteral = Ast.IntegerLiteral <$> curToken <*> parseInteger

-- | parseBoolLiteral
--
parseBoolLiteral :: Parser Ast.Expression
parseBoolLiteral = Ast.Boolean <$> curToken <*> parseBool

-- | parseIfExpression
--
parseIfExpression :: Parser Ast.Expression
parseIfExpression =
    Ast.IfExpression
        <$> next (expectCur Tok.If)
        <*> parentheses (parseExpression Lowest)
        <*  nextToken
        <*> parseBlockStatement
        <*> (parseElseBlock <|> pure Ast.NilStatement)

-- | parseElseBlock
--
parseElseBlock :: Parser Ast.Statement
parseElseBlock =
    next (expectPeek Tok.Else)
        *> next (expectPeek Tok.Lbrace)
        *> parseBlockStatement


-- | parseBlockStatement
--
parseBlockStatement :: Parser Ast.Statement
parseBlockStatement =
    Ast.BlockStatement
        <$> next parseLbrace
        <*> many (next parseStatement)
        <*  parseRbrace

-- | parseFunctionLiteral
--
parseFunctionLiteral :: Parser Ast.Expression
parseFunctionLiteral =
    Ast.FunctionLiteral
        <$> next parseFn
        <*> next parseFunctionParameters
        <*> parseBlockStatement


-- | parseFunctionParameters
--
parseFunctionParameters :: Parser [Ast.Expression]
parseFunctionParameters = do
    next $ expectCur Tok.Lparen
    parameters <- sepBy parseIdentifire Tok.Comma
    expectCur Tok.Rparen
    return parameters


-- | parseCallExpression
--
parseCallExpression :: Ast.Expression -> Parser Ast.Expression
parseCallExpression function =
    Ast.CallExpression
        <$> next (expectCur Tok.Lparen)
        <*> pure function
        <*> sepBy (parseExpression Lowest) Tok.Comma
        <*  expectCur Tok.Rparen

-- | parseGroupedExpression
--
parseGroupedExpression :: Parser Ast.Expression
parseGroupedExpression = do
    nextToken
    expression <- parseExpression Lowest
    next (expectPeek Tok.Rparen)
    return expression

-- | parseStringLiteral
--
parseStringLiteral :: Parser Ast.Expression
parseStringLiteral =
    Ast.StringLiteral <$> curToken <*> (Tok.literal <$> curToken)


-- | parseArrayLiteral
--
parseArrayLiteral :: Parser Ast.Expression
parseArrayLiteral =
    Ast.ArrayLiteral <$> nextToken <*> sepBy (parseExpression Lowest) Tok.Comma

-- | parseIndexExpression
--
parseIndexExpression :: Ast.Expression -> Parser Ast.Expression
parseIndexExpression left =
    Ast.IndexExpression
        <$> nextToken
        <*> pure left
        <*> parseExpression Lowest
        <*  next (expectPeek Tok.Rbracket)

-- | parseFn
--
parseFn :: Parser Tok.Token
parseFn = expectCur Tok.Function

-- | parseLbrace
--
parseLbrace :: Parser Tok.Token
parseLbrace = expectCur Tok.Lbrace

-- | parseRbrace
--
parseRbrace :: Parser Tok.Token
parseRbrace = expectCur Tok.Rbrace

-- | parseBool
--
parseBool :: Parser Bool
parseBool = (expectCur Tok.TRUE $> True) <|> (expectCur Tok.FALSE $> False)

-- | parseLet
--
parseLet :: Parser Tok.Token
parseLet = expectCur Tok.Let

-- | parseAssign
--
parseAssign :: Parser Tok.Token
parseAssign = expectCur Tok.Assign

-- | parseReturn
--
parseReturn :: Parser Tok.Token
parseReturn = expectCur Tok.Return


-- | parseInteger
--
parseInteger :: Parser Integer
parseInteger = do
    v <- expectCur Tok.Int
    let intV = read . T.unpack . Tok.literal $ v :: Integer
    return intV

-- | goAheadIfNextSemicolon
-- | カレントトークンの次がセミコロンならそこまで進める
--
goAheadIfNextSemicolon :: Parser ()
goAheadIfNextSemicolon =
    ((expectPeek Tok.Semicolon *> nextToken) $> ()) <|> pure ()

-- | parentheses
--
parentheses :: Parser a -> Parser a
parentheses p =
    next (expectCur Tok.Lparen) *> p <* next (expectPeek Tok.Rparen)

-- | expectCur
--
-- TODO:
-- Token 全て表示すると情報過多のためタイプだけとかに検討する
--
expectCur :: Tok.TokenType -> Parser Tok.Token
expectCur expected = do
    t <- curToken
    if Tok.isToken expected t
        then return t
        else
            fail
                (printf "invalid token:%s expected token type:%s"
                        (show t)
                        (show expected)
                )

-- | expectPeek
--
-- TODO:
-- Token 全て表示すると情報過多のためタイプだけとかに検討する
--
expectPeek :: Tok.TokenType -> Parser Tok.Token
expectPeek expected = do
    t <- peekToken
    if Tok.isToken expected t
        then return t
        else fail
            (printf "invalid peek token:%s expected peek token type:%s"
                    (show t)
                    (show expected)
            )

-- | takeWhileToken
--
takeWhileToken :: Tok.TokenType -> Parser ()
takeWhileToken target = do
    t <- curToken
    if Tok.isToken target t
        then return ()
        else nextToken >> takeWhileToken target

-- | sepBy
--
sepBy :: Parser a -> Tok.TokenType -> Parser [a]
sepBy p tokType = someParams <|> return []
  where
    someParams = do
        r <- p
        (next . next . expectPeek) tokType
            *>  fmap (r :) (sepBy p tokType)
            <|> next (return [r])
-- | next
--
next :: Parser a -> Parser a
next p = p <* nextToken


-- | peekPrecedence
--
peekPrecedence :: Parser Precedence
peekPrecedence = do
    t <- peekToken
    return $ M.findWithDefault Lowest (Tok.tokenType t) precedences

-- | curPrecedence
--
curPrecedence :: Parser Precedence
curPrecedence = do
    t <- curToken
    return $ M.findWithDefault Lowest (Tok.tokenType t) precedences
