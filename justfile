default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid
run:
    ghcid -c "cabal repl" --warnings


# Run tests (with auto-reload / recompile)
test:
    ghcid -c "cabal repl test:test --flags=ghcid" --warnings -T :main