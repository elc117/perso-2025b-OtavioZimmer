-- Tests.hs

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Data.Time.Clock (getCurrentTime)
import GameLogic
import Words (Entry(..), entries)

-- Teste para updateChar
testUpdateChar :: Test
testUpdateChar =
  TestCase $ assertEqual "updateChar deve atualizar posição 1"
                         [Just 'a', Just 'x', Just 'b']
                         (updateChar 1 'x' [Just 'a', Nothing, Just 'b'])

-- Teste para updateWord
testUpdateWord :: Test
testUpdateWord =
  TestCase $ assertEqual "updateWord deve atualizar a segunda palavra"
                         [[Just 'a'], [Just 'c', Just 'd'], [Just 'b']]
                         (updateWord 1 [Just 'c', Just 'd'] [[Just 'a'], [Nothing, Nothing], [Just 'b']])

-- Teste para applyTry
testApplyTry :: Test
testApplyTry = TestCase $ do
  currentTime <- getCurrentTime
  let entry1 = Entry "AB" "Dica1"
      entry2 = Entry "CD" "Dica2"
      gs = GameState entry1 [entry2] [[Nothing, Nothing]] currentTime "facil"
      try1 = Try 0 0 'C'
      gs' = applyTry gs try1
  assertEqual "applyTry deve atualizar corretamente o progresso"
              [[Just 'C', Nothing]] (progress gs')

-- Teste para calculateScore
testCalculateScore :: Test
testCalculateScore = TestCase $ do
  let scoreEasy = calculateScore "facil" 3 10
      scoreMedium = calculateScore "medio" 4 20
      scoreHard = calculateScore "dificil" 5 30
  assertBool "Pontuação fácil >= 50" (scoreEasy >= 50)
  assertBool "Pontuação média maior que fácil" (scoreMedium > scoreEasy)
  assertBool "Pontuação difícil maior que média" (scoreHard > scoreMedium)

-- Teste para isGameComplete
testIsGameComplete :: Test
testIsGameComplete = TestCase $ do
  currentTime <- getCurrentTime
  let gsComplete = GameState (Entry "A" "dica") [Entry "B" "dica"] [[Just 'a']] currentTime "facil"
      gsIncomplete = GameState (Entry "A" "dica") [Entry "B" "dica"] [[Nothing]] currentTime "facil"
  assertBool "isGameComplete True para jogo completo" (isGameComplete gsComplete)
  assertBool "isGameComplete False para jogo incompleto" (not $ isGameComplete gsIncomplete)

-- Teste para randomVertical
testRandomVertical :: Test
testRandomVertical = TestCase $ do
  e <- randomVertical "facil"
  assertBool "randomVertical deve retornar palavra existente" (e `elem` entries)

-- Teste para selectHorizontals
testSelectHorizontals :: Test
testSelectHorizontals = TestCase $ do
  let verticalE = Entry "AB" "Dica"
  hs <- selectHorizontals "AB" verticalE
  assertEqual "selectHorizontals deve gerar lista com mesmo tamanho da palavra vertical"
              2 (length hs)

-- Teste para newGame
testNewGame :: Test
testNewGame = TestCase $ do
  gs <- newGame "facil"
  assertEqual "newGame deve ter mesmo número de horizontais que letras da vertical"
              (length $ word $ verticalWord gs)
              (length $ horizontalsW gs)

-- Todos os testes agrupados
tests :: Test
tests = TestList
  [ TestLabel "updateChar" testUpdateChar
  , TestLabel "updateWord" testUpdateWord
  , TestLabel "applyTry" testApplyTry
  , TestLabel "calculateScore" testCalculateScore
  , TestLabel "isGameComplete" testIsGameComplete
  , TestLabel "randomVertical" testRandomVertical
  , TestLabel "selectHorizontals" testSelectHorizontals
  , TestLabel "newGame" testNewGame
  ]

main :: IO Counts
main = runTestTT tests