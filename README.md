# buss - Bottom Up Static Sites

### For people who

* Want the full abstractive power of Haskell
* Like having a little server that reloads
* Prefer functions to templating systems
* Use nix

### Run dev server:

```bash
nix-shell live.nix --run ./live.sh
```

### Generate static site:

```bash
nix-shell live.nix --run 'cd src; runhaskell Main.hs'
```
