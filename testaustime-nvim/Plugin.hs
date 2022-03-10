{-# LANGUAGE TemplateHaskell #-}
module Plugin (plugin) where

import Neovim
import Neovim.API.String
import Testaustime (testaustimeHeartBeat, initTestaustimeEnv, testaustimeFlush)

plugin :: Neovim () NeovimPlugin
plugin = do
    _ <- nvim_exec ":autocmd CursorMoved * call TestaustimeHeartBeat()" False
    _ <- nvim_exec ":autocmd ExitPre * call TestaustimeFlush()" False
    env <- initTestaustimeEnv
    wrapPlugin Plugin
        { environment = env
        , exports = [ $(function' 'testaustimeHeartBeat) Async
        , $(function' 'testaustimeFlush) Sync ]
        }
