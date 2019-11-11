{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.QQ (prog, progFile, func) where

import Data.List
import Generate.Lang (Program, FunctionDef)
import qualified Generate.Parser as P (Parser, program, parseProgram, func, parseFunc, whiteSpace) 
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Token


-- Reads in a file rather than an inline string
-- The argument is a filepath relative to the root of the project
-- Extra whitespace is not allowed to [progFile| foo.txt] will fail. It's dumb
progFile :: QuasiQuoter
progFile = quoteFileWithPos $ qExp P.program 'unsafeParseProgram

prog :: QuasiQuoter
prog = QuasiQuoter { quoteDec = error "undefined"
                   , quoteExp  = qExp P.program 'unsafeParseProgram location'
                   , quotePat  = error "undefined"
                   , quoteType = error "undefined"
                   }

func :: QuasiQuoter
func = QuasiQuoter { quoteDec = error "undefined"
                   , quoteExp  = qExp topLevelFunction 'unsafeParseFunction location'
                   , quotePat  = error "undefined"
                   , quoteType = error "undefined"
                   }

topLevelFunction :: P.Parser FunctionDef
topLevelFunction = P.whiteSpace *> P.func <* eof

-- Okay here's the sitch, because things are hella monadic out the other end
-- we can't just serialize our parsed output back to disk. We'd need another
-- AST for that. Ew. Gross. Maybe someday. But here's what we can do,
-- 1. Parse at compile time
-- 2. Report parse errors as compile errors
-- 3. Emit code that parses again at runtime
-- (note): if we get to step 3 the code is guaranteed to parse so we can safely unwrap
qExp :: P.Parser a -> TH.Name -> Q SourcePos -> String -> Q Exp
qExp parser uparse pos source = do
    l <- pos
    let res = parse (setPosition l *> parser) "foo" source;
    case res of
        Left err -> do
            p <- pretty source err
            reportError p
        _ -> return ()
    return $ TH.AppE (VarE uparse) $ TH.LitE $ StringL source

pretty :: String -> ParseError -> Q String
pretty src err = do
    lnum <- lineNum
    let ltext = (lines src)!!(lnum -1)
    let focus = pointTo lnum (sourceColumn pos) ltext
    return $ "Error in: " ++ sourceName pos ++ "\n\n" ++ focus ++ indented_err ++ "\n"
    where
        indented_err = intercalate "\n" $ map ("    " ++) $ lines $ show err
        lineNum = do
            l <- location'
            -- Inline usage
            if sourceName pos == sourceName l
                then return $ (sourceLine pos) - (sourceLine l) + 1
                else return $ sourceLine pos
        pos = errorPos err

pointTo :: Line -> Column -> String -> String
pointTo l c src =
    show l ++ " | " ++ src ++ "\n"
    ++ (replicate (c + 2 + (length $ show l)) ' ') ++ "^\n"

location' :: Q SourcePos
location' = aux <$> location
    where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

unsafeParseProgram :: String -> Program
unsafeParseProgram  = unwrap . P.parseProgram

unsafeParseFunction :: String -> FunctionDef 
unsafeParseFunction = unwrap . (parse topLevelFunction "")

unwrap :: Show a => Either a b -> b
unwrap (Left err) = error $ show err
unwrap (Right x) = x

quoteFileWithPos :: (Q SourcePos -> String -> Q Exp) -> QuasiQuoter
quoteFileWithPos qe = QuasiQuoter { quoteExp = get qe, quotePat = undefined, quoteType = undefined, quoteDec = undefined}
  where
   get :: (Q SourcePos -> String -> Q a) -> String -> Q a
   get old_quoter file_name = do { file_cts <- runIO (readFile file_name) 
                                 ; addDependentFile file_name
                                 ; old_quoter (pure $ initialPos file_name) file_cts }