module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+" -- simbolos de intexp
                        , "-"
                        , "*"
                        , "/"
                        , "++"
                        , "--"
                        -- Simbolos de boolexp
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

-- Aclaracion: La función chainl1 está diseñada exclusivamente para operadores binarios infijos

addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
        <|> (reservedOp lis "-" >> return Minus)
   
   
-----------------------------------
--- Parser de términos (Multiplicación y División)
-----------------------------------

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor mulop

mulop :: Parser (Exp Int -> Exp Int -> Exp Int)
mulop = (reservedOp lis "*" >> return Times)
        <|> (reservedOp lis "/" >> return Div)

-----------------------------------
--- Parser de factores (Números, Variables, Paréntesis, ++, -- y Menos Unario)
-----------------------------------
intfactor :: Parser (Exp Int)
intfactor = try (do reservedOp lis "-"
                    f <- intfactor
                    return (UMinus f))
        <|> parens lis intexp
        <|> try (do v <- identifier lis
                    reservedOp lis "++"
                    return (VarInc v))
        <|> try (do v <- identifier lis
                    reservedOp lis "--"
                    return (VarDec v))
        <|> do v <- identifier lis
               return (Var v)
        <|> do n <- natural lis
               return (Const (fromIntegral n))


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orop

orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop = reservedOp lis "||" >> return Or
        
boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andop

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andop = reservedOp lis "&&" >> return And

-- Obs: no usamos chainl1 pues ! no es op binario infijo, entonces toca a mano
boolfactor :: Parser (Exp Bool)
boolfactor = try (do reservedOp lis "!"
                     b <- boolfactor
                     return (Not b))
         <|> boolatom 

-- Valores atómicos y Operaciones Relacionales
boolatom :: Parser (Exp Bool)
boolatom = parens lis boolexp
       <|> (reserved lis "true" >> return BTrue)
       <|> (reserved lis "false" >> return BFalse)
       <|> relation

-- Parser para los operadores relacionales (no asociativos)
relation :: Parser (Exp Bool)
relation = do e1 <- intexp
              op <- relop
              e2 <- intexp
              return (op e1 e2)

relop :: Parser (Exp Int -> Exp Int -> Exp Bool)
relop = (reservedOp lis "==" >> return Eq)
    <|> (reservedOp lis "!=" >> return NEq)
    <|> (reservedOp lis "<"  >> return Lt)
    <|> (reservedOp lis ">"  >> return Gt)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commterm seqop

seqop :: Parser (Comm -> Comm -> Comm )
seqop = reservedOp lis ";" >> return Seq

commterm:: Parser Comm
commterm = try (do v <- identifier lis
                   reservedOp lis "="
                   e <- intexp
                   return (Let v e))
            <|> commattom

commattom :: Parser Comm
commattom = try (do reserved lis "if"
               b <- boolexp
               c <- braces lis comm
               reserved lis "else"
               c2 <- braces lis comm
               return (IfThenElse b c c2))

            <|> do reserved lis "if"
                   b <- boolexp
                   c <- braces lis comm
                   return (IfThen b c)

            <|> do reserved lis "repeat"
                   c <- braces lis comm
                   reserved lis "until"
                   b <- boolexp
                   return (RepeatUntil c b)
                <|> do reserved lis "skip"
                       return Skip

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
