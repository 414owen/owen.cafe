#!/usr/bin/env -S awk -f
# Replace margin arrows with box-drawing characters.
# Use on the output of:
# objdump --visualize-jumps --no-show-raw-insn --no-addresses -d <FILE>

BEGIN { IGNORECASE = 1 }

# Map ASCII diagram chars to Unicode box-drawing.
function boxify(s, i, c, out) {
  out = ""
  for (i = 1; i <= length(s); i++) {
    c = substr(s, i, 1)
    if      (c == "-") c = "─"
    else if (c == "|") c = "│"
    else if (c == "+") c = "├"
    else if (c == "," || c == "/") c = "╭"
    else if (c == "'" || c == "`" || c == "\\") c = "╰"
    else if (c == ">") c = "➤"
    out = out c
  }
  return out
}

{
  line = $0

  if (match(line, /^[[:space:]]*[,`'+|\\/\->< ]+[[:space:]]/)) {
    pre  = substr(line, 9, RLENGTH)
    post = substr(line, RLENGTH + 9)
    print boxify(pre) post
    next
  }

  # Otherwise, print as-is.
  print line
}

