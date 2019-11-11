{-# LANGUAGE TemplateHaskell #-}
module Generate.QQ (prog) where

import Generate.Lang (Program)
import Generate.Parser (program, parseProgram)
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax 
import Text.Parsec
import Text.Parsec.Pos

prog :: QuasiQuoter
prog = QuasiQuoter { quoteDec = error "undefined"
                   , quoteExp  = qExp
                   , quotePat  = error "undefined"
                   , quoteType = error "undefined"
                   }

-- Okay here's the sitch, because things are hella monadic out the other end
-- we can't just serialize our parsed output back to disk. We'd need another
-- AST for that. Ew. Gross. Maybe someday. But here's what we can do,
-- 1. Parse at compile time
-- 2. Report parse errors as compile errors
-- 3. Emit code that parses again at runtime
-- (note): if we get to step 3 the code is guaranteed to parse so we can safely unwrap
qExp :: String -> Q Exp
qExp source = do
    l <- location'
    let res = parse (setPosition l *> program) "foo" source;
    case res of
        Left err -> reportError $ show err
        _ -> return ()
    return $ TH.AppE (VarE 'unsafeParse) $ TH.LitE $ StringL source

location' :: Q SourcePos
location' = aux <$> location
    where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

unsafeParse :: String -> Program
unsafeParse  = unwrap . parseProgram

unwrap :: Show a => Either a b -> b
unwrap (Left err) = error $ show err
unwrap (Right x) = x