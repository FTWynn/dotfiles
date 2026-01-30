if status is-interactive
    # Commands to run in interactive sessions can go here
    set fish_greeting
    starship init fish | source
    #atuin init fish | source
    direnv hook fish | source

    # Aliases, also known as abbreviations
    abbr l eza
    abbr ll eza --group --header --group-directories-first --long --icons=always --all --time-style=long-iso
    abbr cat bat
    # This let's me ssh into devcontainers without adding te ever changing hosts to a hosts file
    abbr sd ssh -p 2222 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no node@localhost

    # Nix exports and abbreviations
    # set -gx NH_FLAKE /home/david/dotfiles/nix

    abbr nhs 'sudo nixos-rebuild switch --option substituters ''https://cache.nixos.org'''
    abbr nhu 'sudo nixos-rebuild switch --option substituters ''https://cache.nixos.org'''
    abbr nhc 'nh clean all --keep 5'
    abbr lg lazygit

    # Add the different global npm path for NixOS
    fish_add_path ~/.npm-global/bin
    # ... and for uv tools
    fish_add_path ~/.local/bin

    # Setup zoxide
    zoxide init fish | source

    function y
        set tmp (mktemp -t "yazi-cwd.XXXXXX")
        yazi $argv --cwd-file="$tmp"
        if read -z cwd <"$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
            builtin cd -- "$cwd"
        end
        rm -f -- "$tmp"
    end
end
