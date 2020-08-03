[![Build Status](https://travis-ci.org/yusent/resuelve-backend-challenge.svg?branch=master)](https://travis-ci.org/yusent/resuelve-backend-challenge)

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

Run the test suite with:

```
stack test
```

## Deployment

### Heroku

First install the [Heroku CLI](https://devcenter.heroku.com/articles/heroku-cli) if you haven't already. Then run the following:

```
heroku create -b https://github.com/mfine/heroku-buildpack-stack
git push heroku master
```

## Usage

An instance of the API is already available at [https://resuelve-backend-challenge.herokuapp.com](https://resuelve-backend-challenge.herokuapp.com)

Using curl:
```
  curl -H "Content-Type: application/json" \
    -H "Accept: application/json" \
    -d '[{ "nombre": "Juan Perez", "nivel": "C", "goles": 10, "sueldo": 50000, "bono": 25000, "sueldo_completo": null, "equipo": "rojo" }]' \
    https://resuelve-backend-challenge.herokuapp.com/api/v1/calculate_salaries
```
