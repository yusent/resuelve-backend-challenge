# Resuelve's Backend Challenge

For more information see [challenge description](https://github.com/resuelve/prueba-ing-backend).

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

## Development

Start a development server with:

```
stack exec -- yesod devel
```

## Tests

Run the test suit with:

```
stack test
```
