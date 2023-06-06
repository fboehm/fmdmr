#!/bin/bash

# Set the directory path where your files are located
directory="/net/mulan/home/fredboe/research/fmdmr/analysis/data/ukb_dm2_downloads_for_ldsc/"

# Set the known pattern you want to match
pattern="?dl=1"

# Change to the directory
cd "$directory"

# Loop through each file in the directory
for file in *; do
  # Check if the file is a regular file
  if [ -f "$file" ]; then
    # Extract the last five characters of the file name
    last_five="${file: -5}"

    # Check if the last five characters match the known pattern
    if [ "$last_five" = "$pattern" ]; then
      # Extract the new file name by removing the last five characters
      new_name="${file::-5}"

      # Rename the file
      mv "$file" "$new_name"
    fi
  fi
done
