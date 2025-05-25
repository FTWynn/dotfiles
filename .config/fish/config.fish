if status is-interactive
	# Commands to run in interactive sessions can go here
	set fish_greeting
	starship init fish | source
	#atuin init fish | source
	direnv hook fish | source

	# Aliases, also known as abbreviations
	abbr l eza
	abbr ll eza --group --header --group-directories-first --long --icons=always --all --time-style=long-iso

	# Nix exports and abbreviations
	set -gx FLAKE "/home/david/dotfiles/nix"

	abbr nhs 'nh os switch -- --impure --option substitutors ''https://cache.nixos.org'''
	abbr nhu 'nh os switch -u -- --impure --option substitutors ''https://cache.nixos.org'''
	abbr nhc 'nh os clean all --keep 5'

	# Setup zoxide
	zoxide init fish | source
end
