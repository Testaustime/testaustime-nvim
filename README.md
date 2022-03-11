# testaustime-nvim

The official NeoVim client for Testaustime written in Haskell using nvim-hs

## Installation

1. Install the vim plugin:
    You can use your favourite plugin manager. The example uses packer.nvim
    ```vim
    use {'lajp/testaustime-nvim', run = 'cabal install --overwrite-policy=always' }
    ```
    If you're having some issues (arch users) make sure you have cabal configured for dynamic builds.
    More information [here](https://wiki.archlinux.org/title/Haskell#Configuring_Cabal_for_dynamic_linking)
    If installation fails, try running `cabal update` first
2. Add the following lines to your NeoVim config:
    ```vim
    let g:testaustime_url = "https://your.testaustimeinstance.com"
    let g:testaustime_token = "YourVerySecretTestaustimeAuthenticationToken"
    ```
3. Additionally you can configure these values:
    ```vim
    let g:testaustime_ignore = "packer netrw help qf TelescopePrompt" " A space-separated list of filetypes that are ignored
    let g:testaustime_editor_name = "NeoVim" " The value of the editor_name field sent in the heartbeat
    ```

After these steps you should be good to go
