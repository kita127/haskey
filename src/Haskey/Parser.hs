module Haskey.Parser
(
  parse
) where

import           Control.Applicative
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Haskey.Ast          as Ast
import qualified Haskey.Token        as Tok
import           Text.Printf

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
nextToken :: Parser Tok.Token
nextToken = Parser (\input -> case input of
                           []     -> Fail "empty imput" []
                           (x:xs) -> Done x xs)

-- | curToken
--
-- 先頭のトークンを返すが入力を消費しない
--
curToken :: Parser Tok.Token
curToken = Parser (\input -> case input of
                           []    -> Fail "empty imput" []
                           (x:_) -> Done x input)

-- | peekToken
--
peekToken :: Parser Tok.Token
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
precedences :: M.Map Tok.TokenType Precedence
precedences = M.fromList [
                (Tok.Eq, Equals)
              , (Tok.NotEq, Equals)
              , (Tok.Lt, LessGreater)
              , (Tok.Gt, LessGreater)
              , (Tok.Plus, Sum)
              , (Tok.Minus, Sum)
              , (Tok.Slash, Product)
              , (Tok.Asterisk, Product)
              , (Tok.Lparen, Call)
              ]

-- | prefixParseFns
--
prefixParseFns :: M.Map Tok.TokenType (Parser Ast.Expression)
prefixParseFns = M.fromList [
                   (Tok.Ident, parseIdentifire)
                 , (Tok.Int, parseIntegerLiteral)
                 , (Tok.Bang, parsePrefixExpression)
                 , (Tok.Minus, parsePrefixExpression)
                 , (Tok.TRUE, parseBoolean)
                 , (Tok.FALSE, parseBoolean)
                 , (Tok.If, parseIfExpression)
                 , (Tok.Function, parseFunctionLiteral)
                 , (Tok.Lparen, parseGroupedExpression)
                 ]

-- | infixParseFns
--
infixParseFns :: M.Map Tok.TokenType (Ast.Expression -> Parser Ast.Expression)
infixParseFns = M.fromList [
                  (Tok.Plus, parseInfixExpression)
                , (Tok.Minus, parseInfixExpression)
                , (Tok.Slash, parseInfixExpression)
                , (Tok.Asterisk, parseInfixExpression)
                , (Tok.Eq, parseInfixExpression)
                , (Tok.NotEq, parseInfixExpression)
                , (Tok.Lt, parseInfixExpression)
                , (Tok.Gt, parseInfixExpression)
                , (Tok.Lparen, parseCallExpression)
                ]


-- | parse
--
-- TODO:
-- parseProgram にしたい
--
parse :: [Tok.Token] -> Ast.Program
parse = Ast.program . result
  where
    result ts
        | null ts = []
        | (Tok.isToken Tok.Eof . head) ts = []
        | otherwise = case runParser parseStatement ts of
            -- 前の statement 最後のトークンで終わっているので次にトークンを進める
            (Done a (_:rs))      -> a : result rs
            (Fail reason (r:rs)) -> newFailStmt r reason : result rs

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

-- | parseExpressionStatement
--
parseExpressionStatement :: Parser Ast.Statement
parseExpressionStatement = do
    t <- curToken
    expression <- parseExpression Lowest

    -- カレントトークンの次がセミコロンならそこまで進める
    goAheadIfNextSemicolon
    return $ Ast.ExpressionStatement t expression


-- | parseExpression
--
parseExpression :: Precedence -> Parser Ast.Expression
parseExpression precedence = do
    t <- curToken
    let prefixFn  = M.findWithDefault defaultFn (Tok.tokenType t) prefixParseFns
        defaultFn = fail . printf "no prefix parse function for %s found" . show . Tok.tokenType $ t
    leftExp <- prefixFn
    prattParse precedence leftExp


-- | prattParse
--
prattParse :: Precedence -> Ast.Expression -> Parser Ast.Expression
prattParse precedence leftExp = do
    pk <- peekToken
    pkPre <- peekPrecedence
    if not (Tok.isToken Tok.Semicolon pk) && precedence < pkPre
    then do
        t <- next peekToken
        let infixFn   = M.findWithDefault defaultFn (Tok.tokenType t) infixParseFns
            defaultFn = fail . printf "no infix parse function for %s found" . show . Tok.tokenType $ t

        infixFn leftExp >>= prattParse precedence
    else return leftExp



-- | parseReturnStatement
--
parseReturnStatement :: Parser Ast.Statement
parseReturnStatement = Ast.ReturnStatement
                      <$> next parseReturn
                      <*> parseExpression Lowest
                      <* goAheadIfNextSemicolon

-- | parseReturn
--
parseReturn :: Parser Tok.Token
parseReturn = expectCur Tok.Return

-- | parseLetStatement
--
parseLetStatement :: Parser Ast.Statement
parseLetStatement = Ast.LetStatement
                    <$> next parseLet
                    <*> next parseIdentifire <*  next parseAssign
                    <*> parseExpression Lowest <* goAheadIfNextSemicolon


-- | parseLet
--
parseLet :: Parser Tok.Token
parseLet = expectCur Tok.Let

-- | parseAssign
--
parseAssign :: Parser Tok.Token
parseAssign = expectCur Tok.Assign

-- | parseSemicolon
--
parseSemicolon :: Parser Tok.Token
parseSemicolon = expectCur Tok.Semicolon

-- | parseInfixExpression
--
parseInfixExpression :: Ast.Expression -> Parser Ast.Expression
parseInfixExpression leftExp = do
    t <- curToken
    precedence <- next curPrecedence
    rightExp <- parseExpression precedence
    return $ Ast.InfixExpression t leftExp (Tok.literal t) rightExp

-- | parsePrefixExpression
--
parsePrefixExpression :: Parser Ast.Expression
parsePrefixExpression = do
    t <- next curToken
    r <- parseExpression Prefix
    return $ Ast.PrefixExpression t (Tok.literal t) r

-- | parseBoolean
--
parseBoolean :: Parser Ast.Expression
parseBoolean = Ast.Boolean <$> curToken <*> parseBoolLiteral


-- | parseBoolLiteral
--
parseBoolLiteral :: Parser Bool
parseBoolLiteral =     expectCur Tok.TRUE  *> pure True
                   <|> expectCur Tok.FALSE *> pure False

-- | parseIfExpression
--
parseIfExpression :: Parser Ast.Expression
parseIfExpression = do
    t <- nextToken
    condition <- parentheses (parseExpression Lowest)
    nextToken
    consequence <- parseBlockStatement
    alternative <- parseElseBlock <|> pure Ast.NilStatement
    return $ Ast.IfExpression t condition consequence alternative

-- | parseElseBlock
--
parseElseBlock :: Parser Ast.Statement
parseElseBlock = do
    next $ expectPeek Tok.Else
    next $ expectPeek Tok.Lbrace
    parseBlockStatement

-- | parseBlockStatement
--
parseBlockStatement :: Parser Ast.Statement
parseBlockStatement = do
    t <- next (expectCur Tok.Lbrace)
    stmts <- many (next parseStatement)
    expectCur Tok.Rbrace
    return $ Ast.BlockStatement t stmts

-- | parseFunctionLiteral
--
parseFunctionLiteral :: Parser Ast.Expression
parseFunctionLiteral = do
    t <- next (expectCur Tok.Function)
    parameters <- next parseFunctionParameters
    body <- parseBlockStatement
    return $ Ast.FunctionLiteral t parameters body

-- | parseFunctionParameters
--
parseFunctionParameters :: Parser [Ast.Expression]
parseFunctionParameters = do
    next $ expectCur Tok.Lparen
    parameters <- sepBy parseIdentifire Tok.Comma
    expectCur Tok.Rparen
    return parameters

-- | sepBy
--
sepBy :: Parser a -> Tok.TokenType -> Parser [a]
sepBy p tokType = someParams <|> return []
  where
    someParams = do
        r <- p
        (next . next . expectPeek) Tok.Comma *> fmap (r:) (sepBy p tokType)
            <|> next (return [r])

-- | parseCallExpression
--
parseCallExpression :: Ast.Expression -> Parser Ast.Expression
parseCallExpression function = Ast.CallExpression
                               <$> next (expectCur Tok.Lparen)
                               <*> pure function
                               <*> sepBy (parseExpression Lowest) Tok.Comma
                               <*  expectCur Tok.Rparen


-- | parseGroupedExpression
--
parseGroupedExpression :: Parser Ast.Expression
parseGroupedExpression = do
    nextToken
    expression <- parseExpression(Lowest)

    -- NOTE:
    -- Goインタプリタ本では次のトークンが閉じ括弧かどうかだけを確かめて、トークンは進めていない
    -- しかしここで前に進めないと「式パースの終わりはその式の最後のトークン」の原則
    -- に反するため、Haskey ではトークンを前に進める。Go 版がトークンを進めないのは現状で不明
    --
    -- > Go 版は expectPeek が成功したらトークンを次に進める
    --
    next (expectPeek Tok.Rparen)
    return expression

-- | parseIdentifire
--
parseIdentifire :: Parser Ast.Expression
parseIdentifire = Ast.Identifire <$> expectCur Tok.Ident <*> fmap Tok.literal (expectCur Tok.Ident)

-- | parseIntegerLiteral
--
parseIntegerLiteral :: Parser Ast.Expression
parseIntegerLiteral = Ast.IntegerLiteral <$> curToken <*> parseInteger

-- | parseInteger
--
parseInteger :: Parser Integer
parseInteger = do
    v <- expectCur Tok.Int
    let intV = read . T.unpack . Tok.literal $ v :: Integer
    return intV

-- | goAheadIfNextSemicolon
--
goAheadIfNextSemicolon :: Parser ()
goAheadIfNextSemicolon = expectPeek Tok.Semicolon *> nextToken *> pure () <|> pure ()


-- | parentheses
--
parentheses :: Parser a -> Parser a
parentheses p = next (expectCur Tok.Lparen) *> p <* next (expectPeek Tok.Rparen)




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
        else fail (printf "invalid token:%s expected token type:%s" (show t) (show expected))

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
        else fail (printf "invalid peek token:%s expected peek token type:%s" (show t) (show expected))

-- | takeWhileToken
--
takeWhileToken :: Tok.TokenType -> Parser ()
takeWhileToken target = do
    t <- curToken
    if Tok.isToken target t
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
    return $ M.findWithDefault Lowest (Tok.tokenType t) precedences

-- | curPrecedence
--
curPrecedence :: Parser Precedence
curPrecedence = do
    t <- curToken
    return $ M.findWithDefault Lowest (Tok.tokenType t) precedences
