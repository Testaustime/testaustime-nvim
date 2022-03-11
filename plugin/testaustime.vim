" This is a very naive implementation of
" starting a built-and-installed plugin
" For now, the functino takes no arguments
function! testaustime#start()
    try
        let l:chan = remote#host#Require('testaustime-nvim')
        try
            call rpcrequest(l:chan, 'Ping', [])
            return l:chan
        catch '.*No provider for:.*'
            return l:chan
        catch
        endtry
    catch
    endtry
    call remote#host#Register('testaustime-nvim', '*',  jobstart(['testaustime-nvim'], {'rpc': v:true, }))
    return remote#host#Require('testaustime-nvim')
endfunction

try
    call testaustime#start() " Autostart
catch
endtry
