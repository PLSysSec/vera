{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.Parser where
import           Data.Either                            (partitionEithers)
import           Data.List
import           Data.Text                              (Text)
import           Data.Void
import qualified DSL.DSL                                as D
import qualified DSL.Typed                              as DT
import qualified Generate.Lang                          as L
import           Generate.SMTAST
import           Generate.State
import           Text.Parsec                            (Parsec)
import           Text.ParserCombinators.Parsec          hiding (Parser)
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token


type CStmt = Codegen SStmt
type CExpr = Codegen SExpr
type Parser = Parsec String ()

-- Programs
parseProgram :: String -> Either ParseError L.Program
parseProgram = parse program ""

program :: Parser (L.Program)
program = do
    whiteSpace
    els <- many $ parseEither func classDef
    eof
    let (funcs, classes) = partitionEithers els
    return $ L.Program funcs classes

-- Classes
parseClass :: String -> Either ParseError L.ClassDef
parseClass = parse classDef ""

classDef :: Parser (L.ClassDef)
classDef = do
    reserved "struct"
    i <- identifier <?> "struct name"
    els <- braces $ many $ parseEither (classField <* semi) func
    let (fields, fns) = partitionEithers els
    semi
    return $ L.ClassDef i fields fns

classField :: Parser (FieldName, DT.Type)
classField = do
    t <- prim_type <?> "class field type"
    i <- identifier <?> "class field name"
    return (i, t)

-- Functions
parseFunc :: String -> Either ParseError L.FunctionDef
parseFunc = parse func ""

func :: Parser L.FunctionDef
func = do
    ty <- decl_type <?> "function declaration return type"
    n <- identifier <?> "function declaration name"
    args <- parens (commaSep func_arg_decl <?> "function arg")
    body <- block <?> "function body"
    return $ L.Function n ty args body

func_arg_decl :: Parser (VarName, STy)
func_arg_decl = do
    ty <- decl_type
    optional $ do
        reserved "const"
        reservedOp "&"
    n <- identifier
    return (n, ty)

-- Statements
block :: Parser [CStmt]
block = do
    ss <- braces $ many stmt
    return $ concat ss

stmt :: Parser [CStmt]
stmt = choice [
    pure <$> try if_stmt <?> "if statement",
    try decl <* semi <?> "variable declaration",
    pure <$> try assign <* semi <?> "assignment",
    pure <$> try opAssigns <* semi <?> "operater assignment",
    pure <$> try void_stmt <* semi <?> "void call",
    pure <$> return_stmt <* semi <?> "return"] <?> "statement"

-- Decl
decl :: Parser [CStmt]
decl = do
    ty <- decl_type
    i <- identifier
    opt_expr <- optionMaybe $ do
        reservedOp "="
        expr

    case opt_expr of
        Just e -> return [L.declare ty i, L.assign (L.v i) e]
        Nothing -> return [L.declare ty i]

decl_type :: Parser STy
decl_type =
    prim <|> void <|> clas
    where
        prim = PrimType <$> prim_type
        void = return Void <$> reserved "void"
        clas = Class <$> identifier

-- Assign
assign :: Parser CStmt
assign = do
    lval <- expr
    reservedOp "="
    rval <- expr
    return $ L.assign lval rval

-- Operator Assigner (+=, -=, |=, &=)
opAssigns :: Parser CStmt
opAssigns = do
    lval <- expr
    op <- choice $ map opParser [
        ("+", Add),
        ("-", Sub),
        ("&", And),
        ("|", Or),
        ("^", XOr)]
    rval <- expr
    return $ L.assign lval (op <$> lval <*> rval)
    where opParser (s,op) =
                return op <* (reservedOp $ s ++ "=")

-- If Statement
if_stmt :: Parser CStmt
if_stmt = do
    reserved "if"
    cond <- parens expr
    t_branch <- choice [stmt, block]
    f_branch <- option [] $ do
        reserved "else"
        choice [stmt, block]

    return $ If <$> cond <*> sequence t_branch <*> sequence f_branch

-- Void
void_stmt :: Parser CStmt
void_stmt = do
    i <- identifier
    a <- fn_args
    return $ VoidCall i <$> sequence a

-- Return
return_stmt :: Parser CStmt
return_stmt = do
    reserved "return"
    e <- expr
    return $ Return <$> e


js_builtins_1 :: [(String, SExpr -> SExpr)]
js_builtins_1 = [
      ("not", JSNot)
    , ("abs", JSAbs)
    , ("ceil", JSCeil)
    , ("floor", JSFloor)
    , ("sign", JSSign)]

js_builtins_2 :: [(String, SExpr -> SExpr -> SExpr)]
js_builtins_2 = [
      ("and", JSAnd)
    , ("sub", JSSub)
    , ("mul", JSMul)
    , ("or", JSOr)
    , ("xor", JSXOr)
    , ("min", JSMin)
    , ("max", JSMax)
    , ("lsh", JSLsh)
    , ("rsh", JSRsh)
    , ("rrsh", JSUrsh)]

fn_args :: Parser [CExpr]
fn_args = do
    parens $ expr `sepBy` (symbol ",")

builtin_call_1 :: (String, SExpr -> SExpr) -> Parser CExpr
builtin_call_1 (n, c) = do
    symbol n
    args <- fn_args
    case args of
        [a] -> return $ L.unaryOp a c
        _ ->  fail $ "Wrong number of args for builtin: '" ++ show n ++ "' (accepts 1)"

builtin_call_2 :: (String, SExpr -> SExpr -> SExpr) -> Parser CExpr
builtin_call_2 (n, c) = do
    symbol n
    args <- fn_args
    case args of
        [a1, a2] -> return $ L.binOp a1 a2 c
        _ ->  fail $ "Wrong number of args for builtin: '" ++ show n ++ "' (accepts 2)"

js_builtin = do
    reserved "js"
    reservedOp "::"
    choice $ one_args ++ two_args
    where
        one_args = map (try . builtin_call_1) js_builtins_1
        two_args = map (try . builtin_call_2) js_builtins_2


math_builtins_1 :: [(String, SExpr -> SExpr)]
math_builtins_1 = [
      ("abs", Abs)
    , ("exp", GetExp)
    , ("is_neg", IsNegative)
    , ("is_zero", IsZero)
    , ("isnan", IsNan)
    , ("isinf", IsInf)]

math_builtins_2 :: [(String, SExpr -> SExpr -> SExpr)]
math_builtins_2 = [
      ("min", Min)
    , ("max", Max)]

math_builtin :: Parser CExpr
math_builtin = do
    reserved "math"
    reservedOp "::"
    choice $ one_args ++ two_args
    where
        one_args = map (try . builtin_call_1) math_builtins_1
        two_args = map (try . builtin_call_2) math_builtins_2

parseExpr :: [Char] -> Either ParseError CExpr
parseExpr = parse expr ""

var :: Parser CExpr
var = do
    i <- identifier
    return $ L.v i

call :: Parser CExpr
call = do
    i <- identifier
    a <- fn_args
    return $ L.call i a

expr :: Parser CExpr
expr = buildExpressionParser operators term
    <?> "expression"

float_lit :: Parser CExpr
float_lit = do
    f <- float
    return $ L.d DT.Double f


int_type :: Parser DT.Type
int_type = do
    choice [
        reserved "uint8_t" >> return DT.Unsigned8,
        reserved "uint16_t" >> return DT.Unsigned16,
        reserved "uint32_t" >> return DT.Unsigned,
        reserved "uint64_t" >> return DT.Unsigned64,
        reserved "int8_t" >> return DT.Signed8,
        reserved "int16_t" >> return DT.Signed16,
        reserved "int32_t" >> return DT.Signed,
        reserved "int64_t" >> return DT.Signed64]

float_type :: Parser DT.Type
float_type = do
    reserved "double" >> return DT.Double

num_type :: Parser DT.Type
num_type = do
    try int_type <|> float_type

prim_type :: Parser DT.Type
prim_type = do
    try num_type <|> (reserved "bool" >> return DT.Bool)

paren_expr :: Parser CExpr
paren_expr = do
    try typed_lit
    <|> parens expr

term :: Parser CExpr
term    =  paren_expr <|> float_lit <|> try js_builtin <|> try math_builtin <|> try call <|> var
    <?> "simple expression"

cast :: Parser (CExpr -> CExpr)
cast = do
    t <- parens prim_type
    notFollowedBy signedNaturalOrFloat
    return $ \e -> L.cast e t

typed_lit :: Parser CExpr
typed_lit = do
    t <- parens prim_type
    n <- signedNaturalOrFloat
    case (n, t) of
        (Left i, DT.Double) -> return $ L.d DT.Double $ fromIntegral i
        (Left i, _) -> return $ L.n t i
        (Right f, _) -> return $ L.cast (L.d DT.Double f) t


prefixOp :: [Char] -> (CExpr -> CExpr) -> Parser (CExpr -> CExpr)
prefixOp p f = do
    reservedOp p
    return f

member :: Parser(CExpr -> CExpr)
member = do
    reservedOp "."
    i <- identifier
    return $ (L..->. i)

-- This list does its best to match: https://en.cppreference.com/w/cpp/language/operator_precedence
operators = [[postfix $ member]
            ,[prefix $ choice [prefixOp "-" L.negative_, prefixOp "!" L.not_, prefixOp "~" L.bitwise_neg_, try cast]]
            ,[binary "*" Mul AssocLeft]
            ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
            ,[binary "<<" Shl AssocLeft, binary ">>" Shr AssocLeft]
            ,[binary "<" Lt AssocLeft, binary "<=" Lte AssocLeft]
            ,[binary ">" Gt AssocLeft, binary ">=" Gte AssocLeft]
            ,[binary "==" Eq AssocLeft, binary "!=" NEq AssocLeft]
            ,[binary "&" And AssocLeft]
            ,[binary "^" XOr AssocLeft]
            ,[binary "|" Or AssocLeft]
            ,[Infix ternary AssocRight]]

ternary :: Parser (CExpr -> CExpr -> CExpr)
ternary = do
    reservedOp "?"
    t_branch <- expr
    reservedOp ":"
    return $ \cond f_branch -> L.tern_ cond t_branch f_branch

binary  name fun assoc = Infix (do{ reservedOp name; return $  \l r -> L.binOp l r fun }) assoc
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))

languageDef:: LanguageDef st
languageDef =
    emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter <|> char '_'
            , Token.identLetter     = alphaNum <|> char '_' <|> char '\''
            , Token.reservedNames   = ["this", "js", "math", "if", "else", "return", "struct", "const"
                                      , "uint8_t", "uint16_t", "uint32_t", "uint64_t"
                                      , "int8_t", "int16_t", "int32_t", "int64_t"
                                      ]
            , Token.reservedOpNames = ["!", "~", "+", "-", "*", "==", "!=", "?", ":", ">>",  "<<", "<=", ">=", "+=", "-=", "*=", "|=", "&=", "^="
                                      , "<", ">", "&", "|", "->", "."
                                      ]
            }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser languageDef

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

signedNaturalOrFloat :: Parser (Either Integer Double)
signedNaturalOrFloat = do
    s <- option 1 $ char '-' >> (return  (-1))
    n <- naturalOrFloat
    return $ case n of
        Left i  -> Left $ i * s
        Right f -> Right $ f * fromIntegral s

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser String
identifier = Token.identifier lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

commaSep :: Parser t -> Parser [t]
commaSep = Token.commaSep lexer

semiSep :: Parser t -> Parser [t]
semiSep = Token.semiSep lexer

semi :: Parser String
semi = Token.semi lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Util
parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither pa pb = do
    a <- optionMaybe $ try pa
    case a of
        Just o -> return $ Left o
        Nothing -> do
            b <- pb
            return $ Right b
