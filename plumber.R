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
  pr %>%
    # The house style, served from www/. Every example in the Launchpad gallery
    # carries a byte-identical copy of these three files; an API is still a page
    # when somebody opens its root in a browser, and this is the one that says
    # what it answers.
    pr_static("/static", "www") %>%
    pr_set_error(function(req, res, err) {
      res$status <- 400
      list(error = conditionMessage(err))
    })
}

#* Say what this API is and what it answers.
#*
#* An API is still a page when somebody opens its root in a browser, and this is
#* the page: five routes, what each takes, and a console that actually calls
#* them. The console is the point — an API you can try in the tab you found it
#* in is one somebody will still be using on Friday.
#* @serializer html
#* @get /
function() {
  route <- function(method, path, takes, what) {
    tone <- if (method == "GET") "tone-info" else "tone-brand"
    sprintf(paste0(
      '<tr>',
      '<td style="width:5rem"><span class="badge %s">%s</span></td>',
      '<td class="mono small nowrap">%s</td>',
      '<td class="mono small muted">%s</td>',
      '<td class="small muted">%s</td>',
      '</tr>'), tone, method, path, takes, what)
  }

  rows <- paste0(
    route("GET", "/health", "&mdash;",
          "Liveness, and the R version this is running on."),
    route("POST", "/summary", '{"values": [\u2026]}',
          "n, mean, sd, quartiles, and the outliers a boxplot would draw."),
    route("POST", "/trend", '{"values": [\u2026], "ahead": 3}',
          "A least-squares trend and a forecast with prediction intervals."),
    route("POST", "/compare", '{"a": [\u2026], "b": [\u2026]}',
          "Welch&rsquo;s t-test and Mann-Whitney, side by side. When they disagree, that disagreement is the finding."),
    route("POST", "/histogram", '{"values": [\u2026], "title": "\u2026"}',
          "A PNG histogram, for pasting into a ticket."),
    collapse = "")

  cap <- function(label, on, note = "") sprintf(
    '<span class="cap %s" title="%s"><b>%s</b></span>',
    if (isTRUE(on)) "on" else "off", note, label)

  on_platform <- nzchar(Sys.getenv("LAUNCHPAD_APP_TOKEN"))
  caps <- paste0(
    cap("plumber", TRUE,
        "Detected from plumber.R, or any root .R file carrying #* @get / #* @post annotations \u2014 found by a line scan, never a parse."),
    cap("Prefix stripped", TRUE,
        "Started by evaluating plumber::pr_run with the host and port as arguments. R has no --port flag, so nothing from this repository is ever pasted into R source the platform evaluates."),
    cap("renv.lock", file.exists("renv.lock"),
        "Committed, so the Dependencies tab can enumerate this app."),
    cap("Base R only", TRUE,
        "No modelling package. t.test, wilcox.test, lm and quantile have been in R since before most of the alternatives existed."),
    cap("One error handler", TRUE,
        "A validation failure is a 400 with a sentence in it, not a 500 and a stack trace."),
    cap("Launchpad workload", on_platform,
        if (on_platform) "Running as a Launchpad workload." else "This is a local run."),
    collapse = "")

  sprintf('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>%s</title>
<meta name="description" content="R&rsquo;s base statistics behind five HTTP routes.">
<link rel="icon" href="static/favicon.svg" type="image/svg+xml">
<link rel="stylesheet" href="static/launchpad-kit.css">
</head>
<body>
<a class="lp-skip" href="#main">Skip to content</a>

<div class="masthead"><div class="masthead-in">
<div>
<div class="wordmark"><span class="mark"></span><span class="wordmark-text">Launchpad example</span></div>
<h1>%s</h1>
<p class="standfirst">R&rsquo;s base statistics behind five HTTP routes, for a service that
would rather not reimplement them. Every route takes and returns JSON, except the
last, which returns a PNG.</p>
</div>
<div class="masthead-aside">
<span class="chip chip-lang">R %s &middot; plumber</span>
<span class="chip">Five routes</span>
</div>
</div></div>

<div class="rail"><div class="rail-in"><span class="rail-label">Launchpad</span>%s</div></div>

<main class="shell" id="main">

<div class="card">
  <div class="card-hd"><h2>Routes</h2>
    <span class="muted small">Interactive documentation is at <code>__docs__/</code></span></div>
  <div class="card-bd" style="padding:0">
    <div class="tbl-wrap"><table class="tbl"><tbody>%s</tbody></table></div>
  </div>
  <div class="card-ft">
    One error handler for the whole API, so a validation failure is a
    <code>400</code> with a sentence in it rather than plumber&rsquo;s default 500 and a
    stack trace. R&rsquo;s condition messages are written for people, which is exactly
    what a client debugging a request wants to read.
  </div>
</div>

<div class="section">
  <div class="card">
    <div class="card-hd"><h2>Console</h2>
      <span class="muted small">It calls this API, from this page, right now</span></div>
    <div class="card-bd">
      <div class="controls">
        <div class="field">
          <label for="route">Route</label>
          <select id="route">
            <option value="health">GET /health</option>
            <option value="summary" selected>POST /summary</option>
            <option value="trend">POST /trend</option>
            <option value="compare">POST /compare</option>
            <option value="histogram">POST /histogram</option>
          </select>
        </div>
        <div class="field field-wide">
          <label for="body">Request body</label>
          <textarea class="textarea" id="body" spellcheck="false" style="min-height:5.5rem"></textarea>
        </div>
        <div><button class="btn btn-primary" id="send" type="button">Send</button></div>
      </div>
      <div id="out-wrap" style="margin-top:1rem">
        <h4>Response</h4>
        <pre class="term" id="out">Press &ldquo;Send&rdquo;.</pre>
      </div>
      <img id="png" alt="Histogram" hidden
           style="margin-top:1rem;border:1px solid var(--lp-rule);border-radius:var(--lp-r);background:#fff">
    </div>
    <div class="card-ft">
      Every request below is a <code>fetch</code> relative to this document, which is how
      it keeps working under <code>/apps/{slug}/</code>. The equivalent
      <code>curl</code> is under the box.
    </div>
  </div>
</div>

<div class="section">
  <div class="cards">
    <div class="card">
      <div class="card-hd"><h2>From a shell</h2></div>
      <div class="card-bd">
        <pre class="term">curl -X POST "$APP_URL/summary" \\
  -H "content-type: application/json" \\
  -d \'{"values": [12, 15, 9, 22, 14, 61, 13, 16]}\'</pre>
        <div class="note">
          On a Launchpad install <code>$APP_URL</code> is
          <code>https://&lt;install&gt;/apps/&lt;slug&gt;</code>, and an
          <code>lp_</code> app key in an <code>Authorization: Bearer</code> header is how a
          machine reaches it. The key carries a role of its own, so a client described as
          <code>viewer</code> can read and a client described as <code>editor</code> can
          write &mdash; on an API that had anything to write.
        </div>
      </div>
    </div>
    <div class="card">
      <div class="card-hd"><h2>What an R app has to know</h2></div>
      <div class="card-bd">
        <dl class="kv">
          <dt>Detected by</dt><dd><code>plumber.R</code>, or any root <code>.R</code> file
            carrying <code>#* @get</code> / <code>#* @post</code> annotations &mdash;
            found by a line scan, never a parse</dd>
          <dt>Started by</dt><dd>evaluating <code>plumber::pr_run(plumber::plumb(...))</code>
            with the host and port as <em>arguments</em>. R has no <code>--port</code> flag,
            and nothing from this repository is ever pasted into R source the platform
            evaluates</dd>
          <dt>Packages</dt><dd>from <code>renv.lock</code>, installed as <strong>binaries</strong>
            from the repository the operator names. Without a lockfile nothing is installed:
            the deploy succeeds and the app fails at its first <code>library()</code></dd>
          <dt>Pin R with</dt><dd><code>[runtime] r</code> &mdash; comparators only, major.minor,
            and a patch pin is refused rather than truncated</dd>
        </dl>
        <div class="note tone-att">
          <strong>There is no generic R framework, deliberately.</strong>
          <code>Rscript app.R</code> runs to the end and exits, and a workload that exits is
          a crash loop. A repository that is R and matches none of the four shapes is
          refused with the four named.
        </div>
      </div>
    </div>
  </div>
</div>

</main>

<div class="foot"><div class="foot-in">
<span>An example app from the <strong>Launchpad</strong> gallery.</span>
<span>R %s &middot; plumber &middot; base statistics &middot; no modelling package</span>
</div></div>

<script src="static/launchpad-kit.js"></script>
<script>
(function () {
  // Relative to the document, which is how this keeps working under a base
  // path. `new URL(path, base)` with a base that ends in a slash is the whole
  // trick, and it is the browser half of the rule the server half never has to
  // think about — the proxy strips the prefix before plumber sees it.
  var base = new URL(".", location.href);
  var samples = {
    health: "",
    summary: JSON.stringify({ values: [12, 15, 9, 22, 14, 61, 13, 16] }, null, 2),
    trend: JSON.stringify({ values: [41, 44, 39, 47, 52, 49, 58, 61], ahead: 3 }, null, 2),
    compare: JSON.stringify({ a: [12, 15, 9, 22, 14, 13, 16], b: [19, 21, 17, 25, 20, 24, 18] }, null, 2),
    histogram: JSON.stringify({ values: [12, 15, 9, 22, 14, 61, 13, 16, 18, 11, 20, 17], title: "Sample" }, null, 2)
  };
  var routeEl = document.getElementById("route");
  var bodyEl = document.getElementById("body");
  var outEl = document.getElementById("out");
  var pngEl = document.getElementById("png");

  function fill() {
    bodyEl.value = samples[routeEl.value];
    bodyEl.disabled = routeEl.value === "health";
  }
  routeEl.addEventListener("change", fill);
  fill();

  document.getElementById("send").addEventListener("click", function () {
    var route = routeEl.value;
    var url = new URL(route, base);
    outEl.textContent = "\u2026";
    pngEl.hidden = true;
    var init = route === "health"
      ? { method: "GET" }
      : { method: "POST", headers: { "content-type": "application/json" }, body: bodyEl.value };

    fetch(url, init).then(function (res) {
      var type = res.headers.get("content-type") || "";
      if (type.indexOf("image") !== -1) {
        return res.blob().then(function (b) {
          pngEl.src = URL.createObjectURL(b);
          pngEl.hidden = false;
          outEl.textContent = res.status + " " + res.statusText + " \u00b7 " + type;
        });
      }
      return res.text().then(function (t) {
        try { t = JSON.stringify(JSON.parse(t), null, 2); } catch (e) { /* leave it */ }
        outEl.textContent = t;
        outEl.className = res.ok ? "term" : "term dan";
      });
    }).catch(function (err) { outEl.textContent = "error: " + err.message; });
  });
})();
</script>
</body>
</html>',
    api_title, api_title,
    paste(R.version$major, R.version$minor, sep = "."),
    caps, rows,
    paste(R.version$major, R.version$minor, sep = "."))
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
