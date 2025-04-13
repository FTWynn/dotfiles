# Add starship prompt
eval "$(starship init bash)"

# Set Environment variable for FLAKE and nix-helper (nh)
# May not need this given config
export FLAKE="/home/david/dotfiles/nix"

alias nhs='nh os switch -- --impure --option substitutors ''https://cache.nixos.org'''
alias nhu='nh os switch -u -- --impure --option substitutors ''https://cache.nixos.org'''
alias nhc='nh os clean all --keep 5'

# Add zoxide
eval "$(zoxide init bash)"
