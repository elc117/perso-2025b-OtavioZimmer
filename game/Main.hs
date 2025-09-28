-- Main.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class ()
import Data.Aeson (object, (.=))
import Data.IORef
import Words ()
import GameLogic
import Data.Time.Clock (getCurrentTime, diffUTCTime)
--SQLite imports
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow ()

-- Função principal
main :: IO ()
main = do
  stateRef <- newIORef Nothing  -- inicia sem jogo
  conn <- open "acrostic.db"
  initDB conn
  scotty 3000 $ do
    middleware logStdoutDev

    -- Servindo o frontend
    get "/" $ file "index.html"

    -- API
    get "/acrostic" $ do
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
          json (object ["elapsed" .= (elapsed :: Int)])
        Nothing -> json (object ["elapsed" .= (0 :: Int)])

    post "/try" $ do
      t <- jsonData :: ActionM Try
      maybeGs <- liftIO (readIORef stateRef)
      case maybeGs of
        Just gs -> do
          let newGs = applyTry gs t
          liftIO (writeIORef stateRef (Just newGs))
          -- Verifica se o jogo foi completado
          if isGameComplete newGs
            then do
              currentTime <- liftIO getCurrentTime
              let elapsed = floor $ diffUTCTime currentTime (startTime newGs)
              let wordCount = length (horizontalsW newGs)
              let gameScore = calculateScore (gameDifficulty newGs) wordCount elapsed
              json (object ["gameState" .= newGs, 
                           "completed" .= True,
                           "elapsed" .= (elapsed :: Int),
                           "wordCount" .= wordCount,
                           "score" .= gameScore])
            else json (object ["gameState" .= newGs, "completed" .= False])
        Nothing -> json (object []) -- sem jogo ativo

    post "/newgame" $ do
      d <- jsonData :: ActionM Difficulty
      gs <- liftIO (newGame (difficulty d))
      liftIO (writeIORef stateRef (Just gs))
      json gs

    -- Rota para salvar resultado
    post "/saveresult" $ do
      result <- jsonData :: ActionM GameResult
      liftIO $ execute conn 
        "INSERT INTO game_results (player_name, elapsed_time, difficulty, word_count, score) VALUES (?, ?, ?, ?, ?)" 
        (playerName result, elapsedTime result, resultDifficulty result, wordCount result, score result)
      json (object ["success" .= True])
    
    -- Rota para obter leaderboard
    get "/leaderboard" $ do
      results <- liftIO (query_ conn
        "SELECT player_name, elapsed_time, difficulty, word_count, score \
        \FROM game_results ORDER BY score DESC LIMIT 10"
        :: IO [LeaderboardEntry])
      json results

    -- Rota para resetar o estado (quando volta para o Menu Principal)
    post "/reset" $ do
      liftIO (writeIORef stateRef Nothing)
      json (object ["reset" .= True])