#!/bin/bash

# Create temporary file for concatenated content
TMPFILE=$(mktemp)
trap "rm -f $TMPFILE" EXIT

# Concatenate all markdown files in sorted order
# Custom sort: 0.0.0 (header) first, then 1- (main title), then 1.x (sections)
# We need to handle the fact that "1-transit" should come before "1.1"
for file in $(ls -1 docs/readme/*.md | sort -V); do
    if [ -f "$file" ]; then
        cat "$file" >> "$TMPFILE"
        echo "" >> "$TMPFILE"
    fi
done

# Run patchdown once on the concatenated result
echo "Processing README with patchdown..."
PATCHDOWN_FILE_PATH="$TMPFILE" npx spago run -m Docs.Main >&2 || {
    echo "Warning: Patchdown failed, using unprocessed content" >&2
}

# Copy the processed result to README.md
cp "$TMPFILE" README.md

echo "README.md generated successfully!"
