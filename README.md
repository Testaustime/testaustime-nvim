# testaustime-nvim

The official NeoVim client for Testaustime written in Haskell using nvim-hs

## Installation

1. Run `cabal install` and make sure `~/.cabal/bin` is in the `$PATH`
2. Install the vim plugin:
    You can use your famourite plugin manager. The example uses packer.nvim
    ```vim
    use 'lajp/testaustime-nvim'
    ```
3. Add the following lines to your NeoVim config:
    ```vim
    let g:testaustime_url = "https://your.testaustimeinstance.com/activity/update"
    let g:testaustime_token = "YourVerySecretTestaustimeAuthenticationToken"
    ```

After these steps you should be good to go
