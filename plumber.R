# Stats API — a plumber example for Launchpad.
#
# `plumber.R` is the conventional name and the one `plumber::plumb()` finds with
# no argument, so Launchpad detects this repository without a launchpad.toml.
# Detection is a line scan for the `#*` annotations below — never a parse — so
# an R file that merely mentions plumber in prose is not mistaken for an API.
#
# Launchpad starts this by evaluating
# `plumber::pr_run(plumber::plumb(entrypoint), host, port)`. Nothing here reads
# PORT or binds an address, and nothing builds a URL: plumber is a
# prefix-stripping framework here, so every route below is mounted at the root
# and the platform re-adds /apps/{slug} to any Location it sends back.
#
# What this is for: the statistics R has in its base library are a nuisance to
# reimplement in a service language, and this is the shape that lets a Node or
# Go service ask for them over HTTP instead. Only `plumber` is used — every
# computation below is stats::, which ships with R.

library(plumber)

api_title <- {
  t <- trimws(Sys.getenv("API_TITLE", ""))
  if (nzchar(t)) t else "Stats API"
}

# The one cap in the file, and it is a real one: a request body is server-side
# input, and a caller who posts a million values would otherwise decide how long
# this process is busy for. plumber, like Shiny, is a single R process.
max_values <- {
  raw <- suppressWarnings(as.integer(Sys.getenv("MAX_VALUES", "10000")))
  if (is.na(raw) || raw < 10) 10000L else raw
}

# numbers turns whatever arrived in the JSON body into a numeric vector, or
# stops with a message the error handler below turns into a 400. jsonlite gives
# a list when the array is ragged and a vector when it is clean, so both are
# handled rather than assumed.
numbers <- function(x, field = "values", min_length = 1L) {
  if (is.null(x)) {
    stop(sprintf("`%s` is required: a JSON array of numbers.", field))
  }
  if (is.list(x)) {
    if (!all(vapply(x, function(e) length(e) == 1L && is.numeric(e), logical(1)))) {
      stop(sprintf("`%s` must contain numbers only.", field))
    }
    x <- unlist(x)
  }
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) < min_length) {
    stop(sprintf("`%s` needs at least %d numeric value%s.",
                 field, min_length, if (min_length == 1L) "" else "s"))
  }
  if (length(x) > max_values) {
    stop(sprintf("`%s` holds %d values; this API accepts at most %d.",
                 field, length(x), max_values))
  }
  x
}

# One error handler for the whole API, so a validation failure is a 400 with a
# sentence in it rather than plumber's default 500 and a stack trace. R's
# condition messages are written for people, which is exactly what a client
# debugging a request wants to read.
#* @plumber
function(pr) {
  pr %>% pr_set_error(function(req, res, err) {
    res$status <- 400
    list(error = conditionMessage(err))
  })
}

#* Say what this API is and what it answers.
#* @serializer html
#* @get /
function() {
  rows <- rbind(
    c("GET",  "/health",    "Liveness. Reports the R version this is running on."),
    c("POST", "/summary",   "{\"values\": [...]} — n, mean, sd, quartiles, and Tukey outliers."),
    c("POST", "/trend",     "{\"values\": [...], \"ahead\": 3} — a least-squares trend and a forecast with prediction intervals."),
    c("POST", "/compare",   "{\"a\": [...], \"b\": [...]} — Welch t-test and Mann-Whitney, side by side."),
    c("POST", "/histogram", "{\"values\": [...]} — a PNG histogram, for pasting into a ticket.")
  )
  body <- paste(apply(rows, 1, function(r) {
    sprintf("<tr><td class=\"m\">%s</td><td class=\"m\">%s</td><td>%s</td></tr>",
            r[1], r[2], r[3])
  }), collapse = "\n")

  sprintf('<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>%s</title>
<style>
 body { font: 15px/1.55 -apple-system, "Segoe UI", Roboto, sans-serif; color: #22303f;
        background: #fbfbfd; margin: 0; padding: 40px 24px; }
 main { max-width: 780px; margin: 0 auto; }
 h1 { font-size: 22px; font-weight: 600; margin: 0 0 6px; }
 p.sub { color: #666; font-size: 14px; margin: 0 0 24px; }
 table { width: 100%%; border-collapse: collapse; font-size: 14px; background: #fff;
         border: 1px solid #e6e6ef; border-radius: 6px; }
 td { padding: 9px 12px; border-bottom: 1px solid #f0f0f5; vertical-align: top; }
 tr:last-child td { border-bottom: none; }
 .m { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; white-space: nowrap; color: #3b5f9e; }
 pre { background: #fff; border: 1px solid #e6e6ef; border-radius: 6px; padding: 12px 14px;
       font-size: 13px; overflow-x: auto; }
 footer { color: #8a8a99; font-size: 13px; margin-top: 24px; }
</style></head><body><main>
<h1>%s</h1>
<p class="sub">R\'s base statistics over HTTP. Every route takes and returns JSON, except the last one, which returns a PNG.</p>
<table>%s</table>
<p class="sub" style="margin-top:24px">Try it:</p>
<pre>curl -X POST "$APP_URL/summary" \\
  -H "content-type: application/json" \\
  -d \'{"values": [12, 15, 9, 22, 14, 61, 13, 16]}\'</pre>
<footer>Running on R %s. Interactive documentation is at <span class="m">__docs__/</span>.</footer>
</main></body></html>',
    api_title, api_title, body, paste(R.version$major, R.version$minor, sep = "."))
}

#* Liveness.
#* @get /health
function() {
  list(status = "ok",
       api = api_title,
       r = paste(R.version$major, R.version$minor, sep = "."),
       max_values = max_values)
}

#* Descriptive statistics, plus the outliers a boxplot would draw.
#* @param values:[numeric] The sample.
#* @post /summary
function(values = NULL) {
  x <- numbers(values)
  q <- as.numeric(stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE))
  iqr <- q[3] - q[1]
  # Tukey's rule, which is what makes a boxplot's dots dots. Stated here rather
  # than left to the caller because "which of these is odd" is the question
  # somebody posting a sample actually has.
  lower <- q[1] - 1.5 * iqr
  upper <- q[3] + 1.5 * iqr

  list(
    n = length(x),
    mean = mean(x),
    sd = if (length(x) > 1) stats::sd(x) else NULL,
    min = min(x),
    q1 = q[1],
    median = q[2],
    q3 = q[3],
    max = max(x),
    iqr = iqr,
    outliers = list(
      rule = "tukey_1.5_iqr",
      below = lower,
      above = upper,
      values = unname(x[x < lower | x > upper])
    )
  )
}

#* A least-squares trend through an evenly spaced series, and where it goes next.
#* @param values:[numeric] The series, oldest first.
#* @param ahead:int How many periods to forecast. 1 to 24.
#* @post /trend
function(values = NULL, ahead = 3) {
  y <- numbers(values, min_length = 3L)
  ahead <- suppressWarnings(as.integer(ahead))
  if (is.na(ahead) || ahead < 1L) ahead <- 3L
  if (ahead > 24L) ahead <- 24L

  t <- seq_along(y)
  fit <- stats::lm(y ~ t)
  future <- data.frame(t = seq(length(y) + 1L, length(y) + ahead))
  # An interval for a *new observation*, not for the fitted line: somebody
  # forecasting next month wants the range next month could land in.
  pred <- stats::predict(fit, newdata = future, interval = "prediction", level = 0.95)
  co <- stats::coef(fit)
  s <- summary(fit)

  list(
    n = length(y),
    slope_per_period = unname(co[2]),
    intercept = unname(co[1]),
    r_squared = s$r.squared,
    p_value = unname(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
                               lower.tail = FALSE)),
    forecast = lapply(seq_len(ahead), function(i) {
      list(period = future$t[i], fit = unname(pred[i, "fit"]),
           low = unname(pred[i, "lwr"]), high = unname(pred[i, "upr"]))
    })
  )
}

#* Two samples, tested both ways: Welch's t-test and Mann-Whitney.
#*
#* Both, rather than one, because the parametric answer is the one people expect
#* and the rank-based one is the one that survives a skewed sample. When they
#* disagree, that disagreement is the finding.
#* @param a:[numeric] The first sample.
#* @param b:[numeric] The second sample.
#* @post /compare
function(a = NULL, b = NULL) {
  xa <- numbers(a, "a", min_length = 2L)
  xb <- numbers(b, "b", min_length = 2L)

  tt <- stats::t.test(xa, xb)
  wt <- suppressWarnings(stats::wilcox.test(xa, xb, exact = FALSE))

  list(
    a = list(n = length(xa), mean = mean(xa), median = stats::median(xa), sd = stats::sd(xa)),
    b = list(n = length(xb), mean = mean(xb), median = stats::median(xb), sd = stats::sd(xb)),
    welch_t = list(
      difference_in_means = unname(mean(xa) - mean(xb)),
      conf_low = tt$conf.int[1],
      conf_high = tt$conf.int[2],
      p_value = tt$p.value
    ),
    mann_whitney = list(p_value = wt$p.value),
    agree = (tt$p.value < 0.05) == (wt$p.value < 0.05)
  )
}

#* A histogram as a PNG.
#* @param values:[numeric] The sample.
#* @param title:str Heading drawn above the plot.
#* @serializer png list(width = 760, height = 420, res = 110)
#* @post /histogram
function(values = NULL, title = "") {
  x <- numbers(values, min_length = 2L)
  heading <- trimws(as.character(title)[1])
  if (is.na(heading) || !nzchar(heading)) heading <- "Distribution"

  op <- par(mar = c(4, 4.5, 2.5, 1), bg = "white")
  on.exit(par(op))
  hist(x, breaks = "Sturges", col = "#c9d1e3", border = "white",
       main = heading, xlab = "Value", las = 1)
  abline(v = mean(x), col = "#3b5f9e", lwd = 2)
  abline(v = stats::median(x), col = "#1a7f5a", lwd = 2, lty = 2)
  legend("topright", legend = c("mean", "median"), bty = "n",
         col = c("#3b5f9e", "#1a7f5a"), lwd = 2, lty = c(1, 2), cex = 0.85)
}
