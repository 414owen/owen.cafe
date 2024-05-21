---
title: "GHC symbol demangler"
date: 2023-05-22T21:09:57+02:00
layout: "blank"
summary: "Owen's contact page"
weight: 7
---

This tool demangles symbol names produced by GHC, as per [this spec](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names).
This can be useful if you're inspecting haskell object files, etc.

<div class="demangle-ghc io-tool">
  <div class="tool-input-container">
    <textarea id="ghc-input" class="tool-input">multizmexceptzm2zi0zi0zminplace_ControlziApplicativeziMultiExcept_zdtczqSuccess3_bytes</textarea>
  </div>
  <div class="tool-output-container">
    <div id="ghc-output" class="tool-output"></div>
  </div>
  <div class="tool-arrow" style="font-size: 140px">&#10552;</div>
</div>

Examples: <select id="demangle-ghc-examples">
  <option value="multizmexceptzm2zi0zi0zminplace_ControlziApplicativeziMultiExcept_zdtczqSuccess3_bytes">multi-except</option>
  <option value="ghczmprim_GHCziTypes_ZMZN_closure">[ listy ]</option>
  <option value="base_GHCziBase_ZCzb_con_info">non :| empty</option>
  <option value="ghczmprim_GHCziTupleziPrim_Z10T_con_info">Big ( tuple )</option>
  <option value="ghczmprim_GHCziTupleziPrim_Z10H_con_info">Big (# tuple #)</option>
  <option value="abcdefghijklmnopqrstuvwxyzz\nABCDEFGHIJKLMNOPQRSTUVWXYZZ\nz03bbU z03a0U\nza zb zc zd ze zg zh zi zl zm zn zp zq zr zs zt zu zv\nZL ZR ZM ZN ZC\nZ0T Z3T\nZ1H Z3H\nZ9H">Kitchen sink</option>
</select>

I find a good way of getting some test data is by using something like this:

```
readelf --wide -a SOME_HS_LIB | grep -E '^ +[0-9]+' | grep -E -v '\.' | awk '{print $8}'
```

[<img height="40px" src="/img/github-d.svg" alt="source code">](https://github.com/414owen/demangle-ghc)

<script src="https://414owen.github.io/demangle-ghc/demangle.js"></script>
