# Delete the _build at every compile.
rm -r -f _build
dune build
dune test

