# Stats API

A plumber example for Launchpad: R's base statistics behind an HTTP API, so a
service written in something else can ask for a trend line, a t-test or a
boxplot's outliers without reimplementing any of it.

Five routes, all JSON except the last one, which returns a PNG. Nothing is
stored and nothing is fetched — every response is computed from the request body.

## The routes

| Method | Path | What it answers |
|---|---|---|
| `GET` | `/` | An HTML index of these routes. |
| `GET` | `/health` | Liveness, plus the R version and the input cap. |
| `POST` | `/summary` | `n`, mean, sd, quartiles, IQR and the values outside Tukey's fences. |
| `POST` | `/trend` | A least-squares trend through an evenly spaced series, and a forecast with prediction intervals. |
| `POST` | `/compare` | Welch's t-test and Mann-Whitney on two samples, side by side. |
| `POST` | `/histogram` | A PNG histogram with the mean and median marked. |

```bash
curl -X POST "$APP_URL/summary" \
  -H 'content-type: application/json' \
  -d '{"values": [12, 15, 9, 22, 14, 61, 13, 16]}'
```

## Three things worth copying

**The file is called `plumber.R`, and that is the whole of the configuration.**
It is the name `plumber::plumb()` finds with no argument, and it is what
Launchpad detects. Detection is a *line scan* for the `#*` annotations — never a
parse — so a file that merely mentions plumber in a comment is not mistaken for
an API. Launchpad starts it by evaluating
`plumber::pr_run(plumber::plumb(entrypoint), host, port)`; nothing in this
repository reads `PORT` or binds an address.

**Routes are mounted at the root.** plumber is a prefix-stripping framework
here: the platform serves the app at `/apps/{slug}` and forwards `/summary`, not
`/apps/{slug}/summary`. So annotate `#* @post /summary` and never bake the mount
point into a path. If a route sends a `Location` back, send the bare path — the
platform re-adds the prefix on the way out.

**A request body is server-side input.** `MAX_VALUES` caps how many numbers one
request may carry, because plumber, like Shiny, is one R process: a caller who
posts a million values would otherwise decide how long this app is busy for.
The same idea covers the error handler — one `pr_set_error` for the whole API
turns a validation failure into a 400 with a sentence in it, instead of
plumber's default 500 and a stack trace.

## Configuration

| Variable | Required | Default | Meaning |
|---|---|---|---|
| `API_TITLE` | no | `Stats API` | Heading on the index page and in `/health`. |
| `MAX_VALUES` | no | `10000` | Most values one request may carry. Anything below 10 falls back to the default. |

## Dependencies

`renv.lock` is committed, which is what makes this deployable. Without one
Launchpad installs nothing, says so in the build log, and the app fails at its
first `library(plumber)` call.

Only `plumber` is declared. Every computation in the file is `stats::`, which
ships with R.

## Local development

```bash
Rscript -e 'renv::restore()'
Rscript -e 'plumber::pr_run(plumber::plumb("plumber.R"), host = "127.0.0.1", port = 8000)'
```

## A note on the interactive docs

plumber generates an OpenAPI spec from the annotations and serves a Swagger UI
at `__docs__/`. It works, but the spec it advertises describes the routes as
plumber sees them — at the root — while your browser is at `/apps/{slug}/`. Use
it to read the shapes; send real requests with `curl` against the app's own URL.

## The house style, and the console

The look is the [Launchpad Example Kit](https://github.com/gopanair/launchpad-example-kit) —
`www/`, served by `pr_static("/static", "www")` in the `@plumber` block. An API is
still a page when somebody opens its root in a browser, and this is that page.

`GET /` is a full console: the five routes, and a box that **calls them**. Every
request it makes is a `fetch` relative to the document, which is how it keeps
working under `/apps/{slug}/` — the browser half of the rule the server half
never has to think about, because the proxy strips the prefix before plumber
sees it. An API you can try in the tab you found it in is one somebody will still
be using on Friday.

MIT.
