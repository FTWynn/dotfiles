#!/bin/bash
#
# flatpak-sync.sh
#
# A script to synchronize installed Flatpak applications with a list from a file.
# This script is idempotent and safe to run multiple times.
#
# - Calculates which applications need to be installed or removed.
# - Displays a summary of the planned changes.
# - Asks for user confirmation before applying any changes.
#

# Treat unset variables as an error.
set -o nounset

# --- Functions ---

# Displays usage information for the script.
usage() {
  echo "Usage: $0 <path_to_app_list_file>"
  echo
  echo "  Synchronizes installed Flatpak apps with a list of application IDs."
  echo "  The file should contain one Flatpak application ID per line."
  echo "  Lines starting with '#' or '//' and empty lines are ignored."
}

# --- Main Script ---

# 1. Validate Input and Environment
if [[ $# -ne 1 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
  usage
  exit 1
fi

APP_LIST_FILE="$1"

if [[ ! -f "$APP_LIST_FILE" ]]; then
  echo "Error: File not found at '$APP_LIST_FILE'"
  exit 1
fi

if ! command -v flatpak &> /dev/null; then
    echo "Error: 'flatpak' command not found. Please ensure Flatpak is installed."
    exit 1
fi

echo "--- Starting Flatpak synchronization ---"
echo "Using application list: $APP_LIST_FILE"

# 2. Plan Changes (Calculate Installs and Removals)
echo
echo "--- Phase 1: Planning changes... ---"

# Read the target app IDs from the file into an array.
mapfile -t TARGET_APPS < <(grep -vE '^\s*(#|//|$)' "$APP_LIST_FILE" | sed 's/\r$//')
TARGET_APPS_STRING=$(printf "%s\n" "${TARGET_APPS[@]}")

# Get currently installed applications.
INSTALLED_APPS_LIST=$(flatpak list --app --columns=application)

# --- Calculate apps to INSTALL ---
declare -a APPS_TO_INSTALL=()
for app_id in "${TARGET_APPS[@]}"; do
  if ! echo "$INSTALLED_APPS_LIST" | grep -qxF "$app_id"; then
    APPS_TO_INSTALL+=("$app_id")
  fi
done

# --- Calculate apps to REMOVE ---
declare -a APPS_TO_REMOVE=()
mapfile -t CURRENT_INSTALLED_APPS < <(echo "$INSTALLED_APPS_LIST")

for installed_app in "${CURRENT_INSTALLED_APPS[@]}"; do
  # If an installed app is not in our target list, mark it for removal.
  if ! echo "$TARGET_APPS_STRING" | grep -qxF "$installed_app"; then
    APPS_TO_REMOVE+=("$installed_app")
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
if [[ ${#APPS_TO_INSTALL[@]} -eq 0 ]] && [[ ${#APPS_TO_REMOVE[@]} -eq 0 ]]; then
  echo -e "${COLOR_GREEN}✔ Your system is already in sync. No changes needed.${COLOR_NONE}"
  exit 0
fi

# Print applications to be installed.
if [[ ${#APPS_TO_INSTALL[@]} -gt 0 ]]; then
  echo -e "${COLOR_GREEN}The following applications will be INSTALLED:${COLOR_NONE}"
  printf "  + %s\n" "${APPS_TO_INSTALL[@]}"
fi

# Print applications to be removed.
if [[ ${#APPS_TO_REMOVE[@]} -gt 0 ]]; then
  echo -e "${COLOR_RED}The following applications will be REMOVED (if not required by others):${COLOR_NONE}"
  printf "  - %s\n" "${APPS_TO_REMOVE[@]}"
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
if [[ ${#APPS_TO_INSTALL[@]} -gt 0 ]]; then
  echo "-> Installing ${#APPS_TO_INSTALL[@]} application(s)..."
  for app_id in "${APPS_TO_INSTALL[@]}"; do
      echo "--> Attempting to install '$app_id' from flathub..."
      # Check if the application exists in the flathub remote before trying to install.
      if flatpak remote-info flathub "$app_id" &>/dev/null; then
        flatpak install --noninteractive --assumeyes flathub "$app_id"
      else
        echo "    Warning: Application '$app_id' not found in 'flathub'. Skipping."
      fi
  done
fi

# Perform removals
if [[ ${#APPS_TO_REMOVE[@]} -gt 0 ]]; then
  echo "-> Removing ${#APPS_TO_REMOVE[@]} application(s)..."
  # Uninstall one-by-one to allow Flatpak to safely skip removals of dependencies.
  for app_id in "${APPS_TO_REMOVE[@]}"; do
    echo "--> Attempting to remove '$app_id'..."
    flatpak uninstall --noninteractive --assumeyes "$app_id" || true
  done
fi

echo
echo -e "${COLOR_GREEN}--- Flatpak synchronization complete. ---${COLOR_NONE}"
