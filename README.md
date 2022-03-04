# testaustime-nvim

The official NeoVim client for Testaustime written in Haskell using nvim-hs

## Installation

1. Run `cabal install` and make sure `~/.cabal/bin` is in the `$PATH`
2. Copy [plugin/testaustime.vim](/plugin/testaustime.vim) somewhere where it's sourced by NeoVim
3. Add the following lines to your NeoVim config:
    ```vim
    let g:testaustime_url = "https://your.testaustimeinstance.com/activity/update"
    let g:testaustime_token = "YourVerySecretTestaustimeAuthenticationToken"
    call testaustime#start() " Must be sourced after /plugin/testaustime.vim
    ```

After these steps you should be good to go
