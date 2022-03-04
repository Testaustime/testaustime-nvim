module Main where

import Neovim
import qualified Plugin

main :: IO ()
main = do
    neovim defaultConfig
        { plugins = plugins defaultConfig ++ [ Plugin.plugin ]
        }
