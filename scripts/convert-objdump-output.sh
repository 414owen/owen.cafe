#!/usr/bin/env bash

if (($# < 1)); then
  echo "Please pass me a file"
  exit 1
fi

fname="$1"

if [ ! -f "$fname" ]; then
  echo "\"$fname\" is not a valid file"
  exit 1
fi

tmp="$(mktemp)"

cat "$fname" \
  | awk 'match($0, /^(([a-z]+:)?[+/>\|\\ ]+)(-[+>/\|\\ -]*)(.*)/, a) { gsub("-", "─", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[+/>─\\ ]+)(\|[+>/\|\\ ─]*)(.*)/, a) { gsub("\\|", "│", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[+│>─\\ ]+)(\/[+>│/\\ ─]*)(.*)/, a) { gsub("/", "╭", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[+│>─╭ ]+)(\\[+>│╭\\ ─]*)(.*)/, a) { gsub("\\\\", "╰", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[╰│>─╭ ]*)(+[+>│╰╭ ─]*)(.*)/, a) { gsub("+", "├", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[╰│├─╭ ]*)(>[├>│╰╭ ─]*)(>.*)/, a) { gsub(">", "├", a[3]); print a[1] a[3] a[4]; next } // {print}' \
  | awk 'match($0, /^(([a-z]+:)?[╰│├─╭ ]*)(> [├>│╰╭─]*)(.*)/, a) { gsub(">", "➤", a[3]); print a[1] a[3] a[4]; next } // {print}' > "$tmp"

mv "$tmp" "$fname"
