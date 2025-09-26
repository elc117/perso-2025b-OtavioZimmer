{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import GHC.Generics (Generic)
import Data.IORef
import qualified Data.Text.Lazy as T
import Web.Scotty (file)
import Words (Entry(..), entries)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

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
  , startTime    :: UTCTime        -- tempo de início do jogo
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
  go verticalStr []  -- começa sem palavras já usadas
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
  return (GameState v hs prog currentTime)

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
updateChar n newChar (x:xs) = x : updateChar (n-1) newChar xs

-- Função para atualizar uma palavra na lista de palavras
updateWord :: Int -> [Maybe Char] -> [[Maybe Char]] -> [[Maybe Char]]
updateWord _ _ [] = []
updateWord 0 newWord (x:xs) = newWord : xs
updateWord n newWord (x:xs) = x : updateWord (n-1) newWord xs

-- Função principal
main :: IO ()
main = do
  stateRef <- newIORef Nothing  -- inicia sem jogo
  scotty 3000 $ do
    middleware logStdoutDev

    -- Servindo o frontend
    get "/" $ file "index.html"

    -- API
    get "/acrostico" $ do
      maybeGs <- liftIO (readIORef stateRef)
      case maybeGs of
        Just gs -> json gs
        Nothing -> json (object []) -- retorna JSON vazio se não há jogo

    -- Nova rota para obter tempo elapsed
    get "/time" $ do
      maybeGs <- liftIO (readIORef stateRef)
      case maybeGs of
        Just gs -> do
          currentTime <- liftIO getCurrentTime
          let elapsed = floor $ diffUTCTime currentTime (startTime gs)
          json (object ["elapsed" .= (elapsed :: Integer)])
        Nothing -> json (object ["elapsed" .= (0 :: Integer)])

    post "/try" $ do
      t <- jsonData :: ActionM Try
      maybeGs <- liftIO (readIORef stateRef)
      case maybeGs of
        Just gs -> do
          let newGs = applyTry gs t
          liftIO (writeIORef stateRef (Just newGs))
          json newGs
        Nothing -> json (object []) -- sem jogo ativo

    post "/newgame" $ do
      d <- jsonData :: ActionM Difficulty
      gs <- liftIO (newGame (difficulty d))
      liftIO (writeIORef stateRef (Just gs))
      json gs