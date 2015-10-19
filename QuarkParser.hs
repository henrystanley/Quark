module QuarkParser (qParse) where

import Text.Parsec
import Text.Parsec ((<|>))
import QuarkType

-- Parsing --

qint = many1 $ oneOf ['0'..'9']

qfloat = do
  whole <- qint
  char '.'
  dec <- qint
  return $ whole ++ "." ++ dec

qnum = do
  neg <- try $ (char '-' >> return "-") <|> return ""
  num <- try qfloat <|> qint
  return $ QNum (read (neg ++ num) :: Double)

qatom = do
  value <- many1 $ noneOf (['0'..'9'] ++ "'\":\\[]|\n\t ")
  return $ QAtom value

qsym = do
  char ':'
  value <- many1 $ noneOf (['0'..'9'] ++ "'\":\\[]|\n\t ")
  return $ QSym value

qstrDouble = do
  char '"'
  text <- many $ noneOf ['"']
  char '"'
  return $ QStr text

qstrSingle = do
  char '\''
  text <- many $ noneOf ['\'']
  char '\''
  return $ QStr text

qstr = try qstrDouble <|> qstrSingle

qtoken = try qnum <|> qatom <|> qsym <|> qstr <|> qquote

qsep = many $ char ' ' <|> char '\n' <|> char '\t'

qtokens = sepEndBy qtoken qsep

qargs = do
  args <- qtokens
  char '|'
  return args

qquote = do
  char '['
  qsep
  args <- try qargs <|> return []
  qsep
  quote <- qtokens
  qsep
  char ']'
  return $ QQuote args quote

quarkParser :: Parsec String () [QItem]
quarkParser = qsep >> sepEndBy (qtoken <|> qquote) qsep

qParse = parse quarkParser ""
