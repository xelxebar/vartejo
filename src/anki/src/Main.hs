{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle


main :: IO ()
main = void (shell "/usr/bin/anki -b $HOME/.anki" empty)
