default:
  cabal run

build:
  cabal build

test:
  watchexec -e hs cabal test --test-show-details=direct