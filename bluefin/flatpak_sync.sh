#!/bin/bash
#
# flatpak-sync.sh
#
# A script to synchronize installed Flatpak applications with a list from a file.
# This script is idempotent and safe to run multiple times.
#
# - Installs applications from the list that are not currently installed.
# - Removes installed applications that are not in the list, unless they are
#   required as dependencies by an application on the list.
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

# 2. Read Target and Currently Installed Applications
# Read the target app IDs from the file into an array.
# - Ignores lines that are empty or start with '#' or '//'.
# - The `sed` command removes potential Windows-style carriage returns (^M).
mapfile -t TARGET_APPS < <(grep -vE '^\s*(#|//|$)' "$APP_LIST_FILE" | sed 's/\r$//')

# Get a list of currently installed applications for efficient checking.
INSTALLED_APPS_LIST=$(flatpak list --app --columns=application)

# 3. Install Missing Applications
echo
echo "--- Phase 1: Installing missing applications... ---"
for app_id in "${TARGET_APPS[@]}"; do
  # Check if the app is already in the list of installed applications.
  if ! echo "$INSTALLED_APPS_LIST" | grep -qxF "$app_id"; then
    echo "-> Found missing application: '$app_id'. Attempting installation from flathub..."
    # Check if the application exists in the flathub remote before trying to install.
    if flatpak remote-info flathub "$app_id" &>/dev/null; then
      flatpak install --noninteractive --assumeyes flathub "$app_id"
    else
      echo "   Warning: Application '$app_id' not found in the 'flathub' remote. Skipping."
    fi
  else
    echo "-> '$app_id' is already installed."
  fi
done

# 4. Remove Unlisted Applications
echo
echo "--- Phase 2: Removing unlisted applications... ---"
# We need a fresh list of installed apps as Phase 1 may have changed it.
mapfile -t CURRENT_INSTALLED_APPS < <(flatpak list --app --columns=application)
TARGET_APPS_STRING=$(printf "%s\n" "${TARGET_APPS[@]}")

for installed_app in "${CURRENT_INSTALLED_APPS[@]}"; do
  # Check if the currently installed app is in our target list.
  if ! echo "$TARGET_APPS_STRING" | grep -qxF "$installed_app"; then
    echo "-> Found unlisted application: '$installed_app'. Attempting removal..."
    # Attempt to uninstall. If the app is a dependency of another app, this
    # command will fail, and Flatpak will print an informative error.
    # The '|| true' ensures the script continues running even if an uninstall fails.
    flatpak uninstall --noninteractive --assumeyes "$installed_app" || true
  fi
done

echo
echo "--- Flatpak synchronization complete. ---"

