{-# LANGUAGE TemplateHaskell #-}
module Plugin (plugin) where

import Neovim
import Neovim.API.String
import Testaustime (testaustimeHeartBeat, initTestaustimeEnv)

plugin :: Neovim () NeovimPlugin
plugin = do
    _ <- nvim_exec ":autocmd CursorMoved * call TestaustimeHeartBeat()" False
    env <- initTestaustimeEnv
    wrapPlugin Plugin
        { environment = env
        , exports = [ $(function' 'testaustimeHeartBeat) Async]
        }
