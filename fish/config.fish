if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting
    starship init fish | source
    #atuin init fish | source
    direnv hook fish | source

     # Aliases, also known as abbreviations
     abbr l eza
     abbr ll eza --group --header --group-directories-first --long --icons=always
end
