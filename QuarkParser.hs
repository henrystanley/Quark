module QuarkParser (qParse) where

import Text.Parsec
import Text.Parsec ((<|>))
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import QuarkType

--- Numbers ---

-- natural number
qint = many1 $ oneOf ['0'..'9']

-- number with fractional part
qfloat = do
  whole <- qint
  char '.'
  dec <- qint
  return $ whole ++ "." ++ dec

-- number with a possible negative sign or fractional part
qnum = do
  neg <- try $ (char '-' >> return "-") <|> return ""
  num <- try qfloat <|> qint
  return $ QNum (read (neg ++ num) :: Double)


--- Tokens ---

-- plain token used for function names and variables
qatom = do
  value <- many1 $ noneOf (['0'..'9'] ++ "'\":\\[]|\n\t ")
  return $ QAtom value

-- token with a ':' in front used for symbols
qsym = do
  char ':'
  value <- many1 $ noneOf (['0'..'9'] ++ "'\":\\[]|\n\t ")
  return $ QSym value


--- Strings ---

-- string delimited by double quotes
qstrDouble = do
  char '"'
  text <- many $ noneOf ['"']
  char '"'
  return $ QStr text

-- string delimited by single quotes
qstrSingle = do
  char '\''
  text <- many $ noneOf ['\'']
  char '\''
  return $ QStr text

-- string delimited by either double or single quotes
qstr = try qstrDouble <|> qstrSingle


--- Quotes --

-- quote arguments
qargs = do
  args <- qtokens
  char '|'
  return args

-- main quote structure
qquote = do
  char '['
  qsep
  args <- try qargs <|> return []
  qsep
  quote <- qtokens
  qsep
  char ']'
  return $ QQuote (Seq.fromList args) (Seq.fromList quote) Map.empty


--- Syntatic Structure ---

-- all of the quark item types
qtoken = try qnum <|> qatom <|> qsym <|> qstr <|> qquote

-- whitespace
qsep = many $ char ' ' <|> char '\n' <|> char '\t'

-- structure for all quark code
qtokens = sepEndBy qtoken qsep


--- Parsing Functions ---

-- parsec parser used in `qParse`
quarkParser :: Parsec String () [QItem]
quarkParser = qsep >> sepEndBy (qtoken <|> qquote) qsep

-- function used to parse a string into quark types
qParse = parse quarkParser ""
