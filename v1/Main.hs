{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.IORef
import qualified Data.Text.Lazy as T
import Web.Scotty (file)
import Words (Entry(..), entries)

-- Estrutura do acróstico, com vertical e horizontais
data Acrostic = Acrostic
  { vertical    :: Entry
  , horizontals :: [Entry]
  } deriving (Show, Generic)

instance ToJSON Acrostic

-- Estado do jogo
data GameState = GameState
  { verticalWord :: Entry
  , horizontalsW :: [Entry]
  , progress     :: [[Maybe Char]] -- progresso de cada palavra
  } deriving (Generic, Show)

instance ToJSON GameState

-- Estrutura para receber uma tentativa
data Try = Try
  { index    :: Int    -- índice da palavra horizontal
  , position :: Int    -- posição da letra na palavra horizontal
  , letter   :: Char   -- letra tentada
  } deriving (Show, Generic)

instance FromJSON Try

-- Função que sorteia uma palavra vertical
randomVertical :: IO Entry
randomVertical = do
  let validEntries = filter (\e -> let firstLetter = head (word e) 
                                   in firstLetter /= 'K' && firstLetter /= 'W'
                                   && firstLetter /= 'Y') entries
  let n = length validEntries
  idx <- randomRIO (0, n-1)
  return (validEntries !! idx)

-- Seleciona horizontais a partir das letras da vertical
selectHorizontals :: String -> Entry -> IO [Entry]
selectHorizontals verticalStr verticalEntry =
  go verticalStr []  -- começa sem usados
  where
    go [] _ = return []
    go (c:cs) usados = do
      let candidates =
            filter (\e -> not (null (word e)) -- word vem do módulo Words.hs
                       && head (word e) == c
                       && word e /= word verticalEntry
                       && e `notElem` usados) entries
      chosen <-
        if null candidates
          then return (Entry [c] ("Sem palavra para " ++ [c]))
          else do
            idx <- randomRIO (0, length candidates - 1)
            return (candidates !! idx)
      rest <- go cs (chosen : usados)
      return (chosen : rest)

-- Cria novo jogo
newGame :: IO GameState
newGame = do
  v <- randomVertical
  hsw <- selectHorizontals (word v) v
  let prog = zipWith (\e c -> Just c : replicate (length (word e) - 1) Nothing) hsw (word v)
  return (GameState v hsw prog)

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
updateChar :: Int -> Char -> [Maybe Char] -> [Maybe Char] -- Optei por utilizar Pattern Matching com recursão
updateChar _ _ [] = []
updateChar 0 newChar (x:xs) = Just newChar : xs
updateChar n newChar (x:xs) = x : updateChar (n-1) newChar xs

-- Função para atualizar uma palavra na lista de palavras
updateWord :: Int -> [Maybe Char] -> [[Maybe Char]] -> [[Maybe Char]]
updateWord _ _ [] = []
updateWord 0 newWord (x:xs) = newWord : xs
updateWord n newWord (x:xs) = x : updateWord (n-1) newWord xs

-- Função principal
main :: IO ()
main = do
  game <- newGame
  stateRef <- newIORef game

  scotty 3000 $ do
    middleware logStdoutDev

    -- Serve o frontend
    get "/" $ file "index.html"

    -- API
    get "/acrostico" $ do
      gs <- liftIO (readIORef stateRef)
      json gs

    -- Tentativa
    post "/try" $ do
      t <- jsonData :: ActionM Try
      liftIO $ modifyIORef stateRef (\gs -> applyTry gs t)
      gs <- liftIO (readIORef stateRef)
      json gs

    -- Iniciar um novo jogo, com palavras aleatórias
    post "/newgame" $ do
      gs <- liftIO newGame
      liftIO (writeIORef stateRef gs)
      json gs