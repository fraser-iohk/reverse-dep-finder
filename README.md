# `reverse-dep-finder`

`reverse-dep-finder` is a tool for cataloguing all of the symbols depended upon by a given Cabal
project.

## Quick-start

Add the following to the target project's `cabal.project` (or `cabal.project.local`):
```
package *
  ghc-options: -fwrite-ide-info -hiedir $SOME_HIE_OUTPUT_DIRECTORY
```

Build the project normally (you may need to `cabal clean` beforehand).

Run `reverse-dep-finder -h $SOME_HIE_OUTPUT_DIRECTORY -p $PLAN_JSON_LOCATION` to generate `exports.csv`, `imports.csv` and `usages.csv`. (The usual location for the cabal `plan.json` file is `dist-newstyle/cache/plan.json`.)
