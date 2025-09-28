-- GameLogic.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GameLogic
  ( GameState(..)
  , Try(..)
  , Difficulty(..)
  , GameResult(..)
  , LeaderboardEntry(..)
  , randomVertical
  , selectHorizontals
  , newGame
  , applyTry
  , updateChar
  , updateWord
  , calculateScore
  , isGameComplete
  , initDB
  ) where

import System.Random (randomRIO)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import GHC.Generics (Generic)
import Words (Entry(..), entries)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- Estrutura base, com a palavra vertical e as palavras horizontais
data Acrostic = Acrostic
  { vertical    :: Entry
  , horizontals :: [Entry]
  } deriving (Show, Generic)

instance ToJSON Acrostic

-- Estado do jogo
data GameState = GameState
  { verticalWord :: Entry
  , horizontalsW :: [Entry]
  , progress     :: [[Maybe Char]] -- progresso de cada palavra (cada pos. pode ser a letra ou null)
  , startTime    :: UTCTime        -- tempo de início do jogo
  , gameDifficulty :: String       -- dificuldade do jogo atual
  } deriving (Generic, Show)

instance ToJSON GameState

-- Estrutura para receber dificuldade
data Difficulty = Difficulty
  { difficulty :: String -- "facil", "medio", "dificil"
  } deriving (Show, Generic)

instance FromJSON Difficulty

-- Estrutura para receber uma tentativa
data Try = Try
  { index    :: Int    -- índice da palavra horizontal
  , position :: Int    -- posição da letra
  , letter   :: Char   -- letra tentada
  } deriving (Show, Generic)

instance FromJSON Try

-- Nova estrutura para salvar resultado
data GameResult = GameResult
  { playerName :: String
  , elapsedTime :: Int
  , resultDifficulty :: String
  , wordCount :: Int
  , score :: Int
  } deriving (Show, Generic)

instance FromJSON GameResult
instance ToJSON GameResult

-- Estrutura para exibir a leaderboard
data LeaderboardEntry = LeaderboardEntry
  { leaderName :: String
  , leaderTime :: Int
  , leaderDifficulty :: String
  , leaderWordCount :: Int
  , leaderScore :: Int
  } deriving (Show, Generic)

instance ToJSON LeaderboardEntry
instance FromRow LeaderboardEntry where
  fromRow = LeaderboardEntry <$> field <*> field <*> field <*> field <*> field

-- Função que sorteia uma palavra vertical baseada na dificuldade
randomVertical :: String -> IO Entry
randomVertical diff = do
  let validEntries = filter (\e -> let firstLetter = head (word e) 
                                       wordLength = length (word e)
                                   in firstLetter /= 'K' && firstLetter /= 'W'
                                   && firstLetter /= 'Y'
                                   && case diff of
                                        "facil" -> wordLength <= 4
                                        "medio" -> wordLength >= 5 && wordLength <= 7
                                        "dificil" -> wordLength > 7
                                        _ -> True) entries
  let n = length validEntries
  idx <- randomRIO (0, n-1)
  return (validEntries !! idx)

-- Seleciona horizontais a partir das letras da vertical
selectHorizontals :: String -> Entry -> IO [Entry]
selectHorizontals verticalStr verticalEntry =
  go verticalStr []  -- começa sem palavras já "usadas"
  where
    go [] _ = return []
    go (c:cs) used = do
      let candidates =
            filter (\e -> not (null (word e))
                       && head (word e) == c
                       && word e /= word verticalEntry
                       && e `notElem` used) entries
      chosen <-
        if null candidates
          then return (Entry [c] ("Sem palavra para " ++ [c]))
          else do
            idx <- randomRIO (0, length candidates - 1)
            return (candidates !! idx)
      rest <- go cs (chosen : used)
      return (chosen : rest)

-- Cria novo jogo com a dificuldade escolhida
newGame :: String -> IO GameState
newGame diff = do
  currentTime <- getCurrentTime
  v <- randomVertical diff
  hs <- selectHorizontals (word v) v
  let prog = zipWith (\e c -> Just c : replicate (length (word e) - 1) Nothing)
                     hs
                     (word v)
  return (GameState v hs prog currentTime diff)

-- Atualiza progresso com tentativa
applyTry :: GameState -> Try -> GameState
applyTry gs t =
  let hs = horizontalsW gs
      prog = progress gs
      idx = index t
      pos = position t
      ch  = letter t
  in if idx < 0 || idx >= length hs
        then gs
        else
          let wordTarget = word (hs !! idx)
          in if pos < 0 || pos >= length wordTarget
                then gs
                else if wordTarget !! pos == ch
                        then let oldWord = prog !! idx
                                 newWord = updateChar pos ch oldWord
                                 newProg = updateWord idx newWord prog
                             in gs { progress = newProg }
                        else gs

-- Função para atualizar um caractere numa palavra
updateChar :: Int -> Char -> [Maybe Char] -> [Maybe Char]
updateChar _ _ [] = []
updateChar 0 newChar (_:xs) = Just newChar : xs
updateChar n newChar (x:xs) = x : updateChar (n-1) newChar xs -- Optei por usar pattern matching com recursão

-- Função para atualizar uma palavra na lista de palavras
updateWord :: Int -> [Maybe Char] -> [[Maybe Char]] -> [[Maybe Char]]
updateWord _ _ [] = []
updateWord 0 newWord (x:xs) = newWord : xs
updateWord n newWord (x:xs) = x : updateWord (n-1) newWord xs

-- Função para calcular pontuação
calculateScore :: String -> Int -> Int -> Int
calculateScore difficulty wordCount elapsedTime =
  let difficultyPoints = case difficulty of
        "facil" -> 100
        "medio" -> 200
        "dificil" -> 300
        _ -> 100
      wordPoints = wordCount * 50
      timePenalty = elapsedTime
      baseScore = difficultyPoints + wordPoints - timePenalty
  in max 50 baseScore -- Garante uma pontuação mínima de 50

-- Função para verificar se o jogo está completo
isGameComplete :: GameState -> Bool
isGameComplete gs = all (all (/= Nothing)) (progress gs)

-- Inicializar o banco de dados do jogo
initDB :: Connection -> IO ()
initDB conn = execute_ conn "CREATE TABLE IF NOT EXISTS game_results (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ player_name TEXT NOT NULL,\
  \ elapsed_time INTEGER NOT NULL,\
  \ difficulty TEXT NOT NULL,\
  \ word_count INTEGER NOT NULL,\
  \ score INTEGER NOT NULL,\
  \ created_at DATETIME DEFAULT CURRENT_TIMESTAMP)"