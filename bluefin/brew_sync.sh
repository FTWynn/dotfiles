#!/bin/bash
#
# brew-sync.sh
#
# A script to synchronize installed Homebrew formulae with a list from a file.
# This script is idempotent and safe to run multiple times.
#
# - Installs formulae from the list that are not currently installed.
# - Removes installed formulae that are not in the list, unless they are
#   required as dependencies by a formula on the list.
#

# Treat unset variables as an error and exit on first error.
set -o nounset
set -o errexit

# --- Functions ---

# Displays usage information for the script.
usage() {
  echo "Usage: $0 <path_to_formula_list_file>"
  echo
  echo "  Synchronizes installed Homebrew formulae with a list of formula names."
  echo "  The file should contain one formula name per line."
  echo "  Lines starting with '#' or '//' and empty lines are ignored."
}

# --- Main Script ---

# 1. Validate Input and Environment
if [[ $# -ne 1 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
  usage
  exit 1
fi

FORMULA_LIST_FILE="$1"

if [[ ! -f "$FORMULA_LIST_FILE" ]]; then
  echo "Error: File not found at '$FORMULA_LIST_FILE'"
  exit 1
fi

if ! command -v brew &> /dev/null; then
    echo "Error: 'brew' command not found. Please ensure Homebrew is installed."
    exit 1
fi

echo "--- Starting Homebrew synchronization ---"
echo "Using formula list: $FORMULA_LIST_FILE"

# 2. Read Target and Currently Installed Formulae
# Read the target formula names from the file into an array.
# - Ignores lines that are empty or start with '#' or '//'.
# - The `sed` command removes potential Windows-style carriage returns (^M).
mapfile -t TARGET_FORMULAE < <(grep -vE '^\s*(#|//|$)' "$FORMULA_LIST_FILE" | sed 's/\r$//')

# Get a list of currently installed formulae for efficient checking.
# The `-1` flag ensures one formula per line.
INSTALLED_FORMULAE_LIST=$(brew list --formula -1)

# 3. Install Missing Formulae
echo
echo "--- Phase 1: Installing missing formulae... ---"
for formula_name in "${TARGET_FORMULAE[@]}"; do
  # Check if the formula is already in the list of installed formulae.
  if ! echo "$INSTALLED_FORMULAE_LIST" | grep -qxF "$formula_name"; then
    echo "-> Found missing formula: '$formula_name'. Attempting installation..."
    brew install "$formula_name"
  else
    echo "-> '$formula_name' is already installed."
  fi
done

# 4. Remove Unlisted Formulae and Orphaned Dependencies
echo
echo "--- Phase 2: Removing unlisted formulae... ---"
# To correctly handle dependencies, we first calculate the complete set of
# formulae that should remain: the target list plus all of their dependencies.
echo "-> Calculating full dependency tree for target formulae..."
# `brew deps --union` gets all recursive dependencies for the entire list.
# We add the target list itself to get the complete set of required packages.
REQUIRED_PACKAGES=$( (brew deps --formula --union "${TARGET_FORMULAE[@]}"; printf "%s\n" "${TARGET_FORMULAE[@]}") | sort -u )

# Get a fresh list of all installed formulae.
mapfile -t CURRENT_INSTALLED_FORMULAE < <(brew list --formula -1)

for installed_formula in "${CURRENT_INSTALLED_FORMULAE[@]}"; do
  # Check if the currently installed formula is in our calculated list of required packages.
  if ! echo "$REQUIRED_PACKAGES" | grep -qxF "$installed_formula"; then
    echo "-> Found unlisted or orphaned formula: '$installed_formula'. Removing..."
    # The '|| true' ensures the script continues if a formula can't be
    # uninstalled for some reason (e.g., permissions).
    brew uninstall "$installed_formula" || true
  fi
done

echo
echo "--- Homebrew synchronization complete. ---"
# You might want to run `brew autoremove` afterwards to clean up any other
# potential leftovers, but this script handles the main logic.

