
# Enable startship prompt
mkdir ($nu.data-dir | path join "vendor/autoload")
starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")

# Enable direnv
$env.config = {
  hooks: {
    pre_prompt: [{ ||
      if (which direnv | is-empty) {
        return
      }

      direnv export json | from json | default {} | load-env
      if 'ENV_CONVERSIONS' in $env and 'PATH' in $env.ENV_CONVERSIONS {
        $env.PATH = do $env.ENV_CONVERSIONS.PATH.from_string $env.PATH
      }
    }]
  }
}

# Enable zoxide
# source "~/.zoxide.nu"

# Alises
alias ll = ls -l -a
alias y = yazi
alias cat = bat
alias lg = lazygit
alias lzd = lazydocker
alias nhc = nh clean all --keep 5
alias nhu = sudo nixos-rebuild switch --option substituters 'https://cache.nixos.org'

# Using helix
$env.config.buffer_editor = "hx"

# Setting externally resolved commands to a new color set above. Might be very slow on WSL and non-Linux
$env.config.highlight_resolved_externals = true
