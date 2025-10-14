#!/bin/bash
#
# brew-sync.sh
#
# A script to synchronize installed Homebrew formulae with a list from a file.
# This script is idempotent and safe to run multiple times.
#
# - Calculates which formulae need to be installed or removed.
# - Displays a summary of the planned changes.
# - Asks for user confirmation before applying any changes.
#

# Treat unset variables as an error.
set -o nounset

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

# 2. Plan Changes (Calculate Installs and Removals)
echo
echo "--- Phase 1: Planning changes... ---"

# Read the target formula names from the file into an array.
mapfile -t TARGET_FORMULAE < <(grep -vE '^\s*(#|//|$)' "$FORMULA_LIST_FILE" | sed 's/\r$//')

# Get currently installed formulae for efficient checking.
INSTALLED_FORMULAE_LIST=$(brew list --formula -1)

# --- Calculate formulae to INSTALL ---
declare -a FORMULAE_TO_INSTALL=()
for formula_name in "${TARGET_FORMULAE[@]}"; do
  if ! echo "$INSTALLED_FORMULAE_LIST" | grep -qxF "$formula_name"; then
    FORMULAE_TO_INSTALL+=("$formula_name")
  fi
done

# --- Calculate formulae to REMOVE ---
declare -a FORMULAE_TO_REMOVE=()
# Calculate the complete set of required packages (targets + all dependencies).
echo "-> Calculating full dependency tree for target formulae..."
# `brew deps --union` gets all recursive dependencies for the entire list.
# We add the target list itself to get the complete set of required packages.
# The error redirect `2>/dev/null` handles cases where a formula in the list doesn't exist.
REQUIRED_PACKAGES=$( (brew deps --formula --union "${TARGET_FORMULAE[@]}" 2>/dev/null; printf "%s\n" "${TARGET_FORMULAE[@]}") | sort -u )

mapfile -t CURRENT_INSTALLED_FORMULAE < <(echo "$INSTALLED_FORMULAE_LIST")

for installed_formula in "${CURRENT_INSTALLED_FORMULAE[@]}"; do
  # If an installed formula is not in our calculated "required" list, mark it for removal.
  if ! echo "$REQUIRED_PACKAGES" | grep -qxF "$installed_formula"; then
    FORMULAE_TO_REMOVE+=("$installed_formula")
  fi
done

# 3. Confirm Changes
echo
echo "--- Phase 2: Summary of planned changes ---"

# Define colors for output
COLOR_GREEN='\033[0;32m'
COLOR_RED='\033[0;31m'
COLOR_NONE='\033[0m'

# Check if there's anything to do.
if [[ ${#FORMULAE_TO_INSTALL[@]} -eq 0 ]] && [[ ${#FORMULAE_TO_REMOVE[@]} -eq 0 ]]; then
  echo -e "${COLOR_GREEN}✔ Your system is already in sync. No changes needed.${COLOR_NONE}"
  exit 0
fi

# Print formulae to be installed.
if [[ ${#FORMULAE_TO_INSTALL[@]} -gt 0 ]]; then
  echo -e "${COLOR_GREEN}The following formulae will be INSTALLED:${COLOR_NONE}"
  printf "  + %s\n" "${FORMULAE_TO_INSTALL[@]}"
fi

# Print formulae to be removed.
if [[ ${#FORMULAE_TO_REMOVE[@]} -gt 0 ]]; then
  echo -e "${COLOR_RED}The following formulae will be REMOVED:${COLOR_NONE}"
  printf "  - %s\n" "${FORMULAE_TO_REMOVE[@]}"
fi

echo
# Prompt the user for confirmation.
read -p "Apply these changes? (y/N): " confirm
if [[ ! "$confirm" =~ ^[yY]([eE][sS])?$ ]]; then
    echo "Aborted by user."
    exit 1
fi

# 4. Apply Changes
echo
echo "--- Phase 3: Applying changes... ---"

# Perform installations
if [[ ${#FORMULAE_TO_INSTALL[@]} -gt 0 ]]; then
  echo "-> Installing ${#FORMULAE_TO_INSTALL[@]} formula(e)..."
  brew install "${FORMULAE_TO_INSTALL[@]}"
fi

# Perform removals
if [[ ${#FORMULAE_TO_REMOVE[@]} -gt 0 ]]; then
  echo "-> Removing ${#FORMULAE_TO_REMOVE[@]} formula(e)..."
  brew uninstall "${FORMULAE_TO_REMOVE[@]}"
fi

echo
echo -e "${COLOR_GREEN}--- Homebrew synchronization complete. ---${COLOR_NONE}"
