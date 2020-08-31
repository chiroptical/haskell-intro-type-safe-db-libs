# Presentation for "An Introduction to Type-Safe Database Libraries in Haskell"

To compile the code examples you can `stack install && stack test`. For the
Opaleye example, you will need `postgres` and `libpq-dev`.

To compile the presentation, you'll need minted and LaTeX.
`pdflatex -shell-escape presentation.tex; pdflatex -shell-escape presentation.tex; pdflatex -shell-escape presentation.tex`

#### Notes

- To make a new release,

```
git tag -a v0.1a0 -m "Release v0.1a0"
git push origin v0.1a0
# Update default.nix
nix-build # build the project, need to update sha256
nix-store --query --references $(nix-instantiate shell.nix) \
  | xargs nix-store --realise \
  | xargs nix-store --query --requisites \
  | cachix push chiroptical # cache the shell environment
git commit -am "Release v0.1a0"
git push
```
