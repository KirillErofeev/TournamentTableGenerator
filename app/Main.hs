{-#LANGUAGE NoMonomorphismRestriction#-}
{-#LANGUAGE FlexibleContexts#-}
module Main where

import Data.Monoid ((<>), mempty)
import Data.List (sort, isSuffixOf)
import System.Directory 
import Control.Applicative
import Data.Set (toList, fromList)
import Text.Parsec

import Lib

data Tournament = TT [Match]

instance Semigroup Tournament where
    (TT m0) <> (TT m1) =  TT (m0 <> m1)

instance Monoid Tournament where
    mempty = TT mempty

data Match = Match {host :: Team, guest :: Team, score :: Score} deriving (Show)

type Team = String

data Score = Score {hostScore :: Int, guestScore :: Int} deriving (Show)

data Table = Table [TeamPerformance]
newtype TeamPerformance = TP (Team,Performance)

instance Eq TeamPerformance where
    TP (t0,p0) == TP (t1,p1) = p0 == p1

instance Ord TeamPerformance where
    TP (t0,p0) <= TP (t1,p1) = p0 <= p1
    
instance Eq Performance where
     (==) (Performance w _ _ s p pts) (Performance w0 _ _ s0 p0 pts0) =
         w == w0 && pts == pts0 && s == s0 && p == p0

instance Ord Performance where
     (<=) (Performance w _ _ s p pts) (Performance w0 _ _ s0 p0 pts0) 
         | pts   /= pts0   = pts   < pts0
         | diffS /= diffS0 = diffS < diffS0
         | w     /= w0     = w     < w0
         | s     /= s0     = s     < s0
         | otherwise       = True
             where 
                 diffS  = s - p 
                 diffS0 = s0 - p0 

data Performance = Performance {wins :: Int,   defs   :: Int, draws  :: Int, 
                              scores :: Int, passes :: Int, points :: Int }


main :: IO ()
main = putStrLn ""

parserMatchs = do
    t0 <- many1 $ satisfy (\x -> x/='.') 
    _ <- char '.'
    t1 <- many1 $ satisfy (\x -> x/='.') 
    return (t0,t1)

parserMatchs0 = do
    t0 <- many1 $ satisfy (\x -> x/='.') 
    t1 <- many1 $ satisfy (\x -> x/='.') 
    return (t0,t1)

fromRight (Right a) = a
fromRight (Left _)  = error "PIZDA"

toResultsName (t0,t1) = t0 ++ "." ++ t1 ++ "." ++ "result" 

parserScore = do
    l <- many1 (satisfy (/=':')) `sepBy` (char ':')
    return $ Score (read $ l !! 1) (read $ l !! 3)

getScore rp p = do
    s <- readFile (rp ++ "/" ++ toResultsName p)
    return $ parse parserScore "" s

getPerformance :: String -> [Match] -> TeamPerformance
getPerformance t matchs = TP (t,(Performance wins defs draws scores passes points)) where
    wins = length $ filter winsPredicat matchs
    winsPredicat (Match h g (Score hg gg))  | h==t && hg > gg = True
                                            | g==t && gg > hg = True
                                            | otherwise       = False
    defs = length $ filter defsPredicat matchs
    defsPredicat (Match h g (Score hg gg))  | h==t && hg < gg = True
                                            | g==t && gg < hg = True
                                            | otherwise       = False
    draws = length $ filter drawsPredicat matchs
    drawsPredicat (Match h g (Score hg gg)) | h==t && hg == gg = True
                                            | g==t && gg == hg = True
                                            | otherwise        = False
    scores = sum $ map scored matchs
    scored (Match h g (Score hg gg))        | h==t             = hg
                                            | g==t             = gg
                                            | otherwise        = 0
    passes = sum $ map passed matchs
    passed (Match h g (Score hg gg))        | h==t             = gg
                                            | g==t             = hg
                                            | otherwise        = 0
    points = wins*2 + draws

instance Show TeamPerformance where
    show (TP (t,(Performance wins defs draws scores passes points))) =
        t++"\t"++show (wins+defs+draws)++"\t"++show wins++"\t"++show defs++"\t"++show draws++"\t"++show scores++"\t"++show passes++"\t"++(show $ scores-passes)++"\t"++show points

showRows = foldr s "" where
    s = (++) . (++"\n") . show
--resultsPath  = "../HaskellRL/codeball2018-linux/"
resultsPath  = "data/1"
resultsPath0 = "data/2"

getTT = do
    matchs <- filter (".result" `isSuffixOf`) <$> listDirectory resultsPath
    let games0 = fromRight $ sequence $  parse parserMatchs "" <$> matchs
    scores0 <- sequence $ getScore resultsPath  <$> games0
    scores1 <- sequence $ getScore resultsPath0 <$> games0
    let games = games0 ++ games0
    --let games = games0
    let scores = scores0 ++ scores1
    --let scores = scores0
    let rMatchs = getZipList $ uncurry Match <$> (ZipList games) <*> (ZipList (fromRight <$> scores))
    let teams = (toList . fromList) $ concatMap (\(a,b) -> [a,b]) games
    writeFile "CUP3" $ showRows $ (reverse . sort) $ flip getPerformance rMatchs <$> teams
