module BasicParser where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Control.Monad.Free

newtype Parser s a = Parser (s -> [(a,s)])

runParser (Parser f) = f

instance Functor (Parser s) where
  fmap f (Parser g) = Parser $ map (first f) . g

instance Applicative (Parser s) where
  pure x = Parser (\s -> [(x, s)])
  (<*>) = ap

getStream :: Parser s s
getStream = Parser (\s -> [(s, s)])

instance Alternative (Parser s) where
  empty = Parser (\x -> [])
  (Parser f) <|> (Parser g) = Parser (\x -> (f x) ++ (g x))
  many p@(Parser f) = Parser (\s -> if null $ f s
                                    then [([], s)]
                                    else runParser (do
                                      a <- p
                                      fmap (a:) (many p)) s)
                         
instance Monad (Parser s) where
  x >>= f = Parser $ concat . map (uncurry (runParser . f)) . (runParser x)

satisfy :: (c -> Bool) -> Parser [c] c
satisfy cond = Parser (\li -> case li of
                               [] -> []
                               h:rest -> if cond h then [(h, rest)] else [])

check :: (s -> Bool) -> Parser s ()
check cond = Parser (\x -> if cond x then [((), x)] else [])

eof :: Parser [c] ()
eof = check null

oneOf :: (Eq c) => [c] -> Parser [c] c
oneOf li = satisfy (`elem` li)

noneOf :: (Eq c) => [c] -> Parser [c] c
noneOf li = satisfy (not . (`elem` li))

char :: (Eq c) => c -> Parser [c] c
char x = satisfy (==x)

string :: (Eq c) => [c] -> Parser [c] [c]
string s = sequence $ map char s

spaces = many (char ' ')

genWord = some (noneOf " (),\n")

type LeafTree = Free [] 

leaf = Pure
node = Free

parseLISP :: Parser String (LeafTree String)
parseLISP = spaces >> (
  do {
    x <- genWord; --String
    return $ leaf x;
  } <|>
  do {
    char '(';
    trees <- many parseLISP;
    spaces;
    char ')';
    return $ node trees;
  })

parseTest :: (Show b) => Parser [c] b -> [c] -> IO ()
parseTest p a = putStrLn $ show $ fst ((runParser (do {x <- p; eof; return x}) a)!!0)

test = parseTest parseLISP "(f ( + 2 3 ) 1 (x (blah)) ((haha)))"
