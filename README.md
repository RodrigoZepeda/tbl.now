
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tibble now (tbl.now) <a href="https://rodrigozepeda.github.io/tbl.now/"><img src="man/figures/logo.png" align="right" height="139" alt="tbl.now website" /></a>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RodrigoZepeda/tbl.now/graph/badge.svg)](https://app.codecov.io/gh/RodrigoZepeda/tbl.now)
<!-- CRAN status badge removed until the package is accepted: its target,
     the canonical CRAN page for this package, 404s until then and
     `R CMD check --as-cran` reports it as a possibly invalid URL in
     README.md. Restore with usethis::use_cran_badge() after acceptance. -->
[![R-CMD-check](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RodrigoZepeda/tbl.now/actions/workflows/R-CMD-check.yaml)
[![R-universe
version](https://RodrigoZepeda.r-universe.dev/tbl.now/badges/version)](https://RodrigoZepeda.r-universe.dev/tbl.now)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

[`tbl.now`](https://rodrigozepeda.github.io/tbl.now/) extends
[`tibble()`](https://tibble.tidyverse.org/) for storing, validating, and
manipulating epidemiological nowcasting data. It standardizes event
dates, report dates, strata, temporal covariates, and related metadata
in a shape compatible with many frameworks, including
[diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/),
[epinowcast](https://package.epinowcast.org/),
[NobBS](https://CRAN.R-project.org/package=NobBS),
[surveillance](https://CRAN.R-project.org/package=surveillance),
[EpiNow2](https://epiforecasts.io/EpiNow2/), and more. Finally, it also
standardizes the prediction engines and their results for plotting,
scoring, comparing, and ensembling models.

A `tbl_now` keeps track of the attributes needed for a nowcasting
exercise, so `dplyr` transformations preserve the relevant nowcasting
variables:

<table>

<thead>

<tr>

<th align="center">

 
</th>

<th align="left">

Argument
</th>

<th align="left">

What it records
</th>

</tr>

</thead>

<tbody>

<tr>

<td align="center">

<img src="man/figures/event_date.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="event_date">
</td>

<td align="left">

<code>event_date</code>
</td>

<td align="left">

The column storing <strong>event dates</strong>; i.e. when the
epidemiological phenomenon of interest happened (symptom onset,
hospitalisation, death, …). <strong>Required.</strong>
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/report_date.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="report_date">
</td>

<td align="left">

<code>report_date</code>
</td>

<td align="left">

The column storing <strong>report dates</strong>; i.e. when that event
became known to the surveillance system. <strong>Required</strong>,
unless it is reconstructed from <code>delay</code>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/revision_date.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="revision">
</td>

<td align="left">

<code>revision_date</code>
</td>

<td align="left">

An optional third date indicating when the report was resolved (see
<code>revision_type</code>). <em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/revision_type.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="revision">
</td>

<td align="left">

<code>revision_type</code>, <code>revision_levels</code>
</td>

<td align="left">

What the revision date resolved to. Only <code>confirmed</code>,
<code>retracted</code>, <code>pending</code> or <code>NA</code> are ever
stored; set <code>revision_levels</code> as a named dictionary mapping
the data’s labels into those four ( e.g. <code>c(positive =
“confirmed”)</code>). <em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/now.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="now">
</td>

<td align="left">

<code>now</code>
</td>

<td align="left">

The date the nowcast is anchored to — “today” from the model’s point of
view. <em>Optional</em>; defaults to the latest date.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/strata.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="strata">
</td>

<td align="left">

<code>strata</code>
</td>

<td align="left">

Columns you want a separate nowcast for (e.g. gender, region).
<em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/covariates.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="covariates">
</td>

<td align="left">

<code>covariates</code>
</td>

<td align="left">

Columns that inform the nowcast but that you do <em>not</em> want it
broken down by (e.g. temperature or precipitation). <em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/case_count.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="case_count">
</td>

<td align="left">

<code>case_count</code>
</td>

<td align="left">

The column holding the counts when the data is given as aggregated
(rather than line-list). <em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/datatype.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="data_type">
</td>

<td align="left">

<code>data_type</code>
</td>

<td align="left">

Whether the data represents a <code>linelist</code> (each row is a
case), <code>count-incidence</code>(each row is a collection of cases
per event-report date) or <code>count-cumulative</code>(each row is the
cummulative number cases for that event accumulating in the report
axis). <em>Optional</em>; inferred by default.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/units.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="units">
</td>

<td align="left">

<code>event_units</code>, <code>report_units</code>,
<code>revision_units</code>
</td>

<td align="left">

The time grid each date lives on: <code>days</code>, <code>weeks</code>,
<code>months</code>, <code>years</code> or <code>numeric</code>.
<em>Optional</em>; inferred (<code>“auto”</code>) by default.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/censoring.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="is_censored_report">
</td>

<td align="left">

<code>is_censored_report</code>,<code>is_censored_revision</code>
</td>

<td align="left">

Flags dates from either the report or the revision axis that are only an
upper bound, i.e. the true report happened <i>before</i> the date given
in the database. <em>Optional</em>.
</td>

</tr>

<tr>

<td align="center">

<img src="man/figures/temporal_effects.svg" height="80" style="height:80px;width:auto;max-width:80px;" alt="temporal_effects">
</td>

<td align="left">

<code>t_effects</code>
</td>

<td align="left">

Columns holding temporal effects (day of week, holidays, …) that some
models can use. <em>Optional</em>.
</td>

</tr>

</tbody>

</table>

You can specify an object as a `tbl.now` with the `tbl_now` command:

``` r
library(dplyr)
library(tbl.now)
data(denguedat)

#Here we use just a few dates for the example
denguedat <- denguedat |> 
  filter(onset_week >= as.Date("2005/01/01"),
         report_week <= as.Date("2005/10/01")) 

#And we specify as a tbl_now:
denguedat <- denguedat |> 
  tbl_now(
    report_date = report_week,
    event_date = onset_week,
    strata = gender
  ) 

#Which is just a tibble with extra attributes
denguedat
#> # A tibble:  1,652 × 6
#> # Data type: "linelist"
#> # Frequency: Event: `weeks` | Report: `weeks`
#>   onset_week   report_week   gender   .event_num .report_num .delay
#>   <date>       <date>        <chr>         <dbl>       <dbl>  <dbl>
#>   [event_date] [report_date] [strata]      [...]       [...]  [...]
#> 1 2005-01-03   2005-01-17    Male              0           2      2
#> 2 2005-01-03   2005-01-10    Female            0           1      1
#> 3 2005-01-03   2005-01-10    Female            0           1      1
#> 4 2005-01-03   2005-01-10    Male              0           1      1
#> 5 2005-01-03   2005-01-10    Male              0           1      1
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # Now: 2005-09-26 | Event date: "onset_week" | Report date: "report_week"
#> # Strata: "gender"
#> # ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> # ℹ 1,647 more rows
```

Once transformed, it can help you diagnose data problems (see [this
article](https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html))
or modeling requirements with your database:

``` r
autoplot(denguedat)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" alt="" width="100%" class="r-plt" />

And it can be used to run any of multiple nowcast libraries through the
`engine()` and `run_nowcast` specifications (see [this
article](https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html)).
For example, [baselinenowcast](https://baselinenowcast.epinowcast.org/):

``` r
dengue_nowcast_1 <- denguedat |> 
  run_nowcast(engine = engine_baselinenowcast())
```

``` r
autoplot(dengue_nowcast_1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="100%" class="r-plt" />

or
[diseasenowcasting](https://rodrigozepeda.github.io/diseasenowcasting/):

``` r
dengue_nowcast_2 <- denguedat |> 
  run_nowcast(engine = engine_diseasenowcasting())
```

``` r
autoplot(dengue_nowcast_2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" alt="" width="100%" class="r-plt" />

It can also generate ensemble nowcasts combining multiple engines or
multiple realizations from the same engine as you can see [in this
article](https://rodrigozepeda.github.io/tbl.now/articles/ensemble-nowcasting.html):

``` r
dengue_ensemble <- nowcast_ensemble(
  baselinenowcast   = dengue_nowcast_1,
  diseasenowcasting = dengue_nowcast_2
)
```

``` r
autoplot(dengue_ensemble)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" alt="" width="100%" class="r-plt" />

If this seems as exciting to you as it is to us, install the development
version from [R universe](https://rodrigozepeda.r-universe.dev/tbl.now):

``` r
install.packages("tbl.now", repos = c("https://rodrigozepeda.r-universe.dev", getOption("repos")))
```

and checkout our articles starting with the [Get started
guide](https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html):

<!-- Single source for "Learning more"; pulled in as a knitr child by README.Rmd and every article. Edit on `learning.more.Rmd`.-->

<div class="alert alert-info">

If you have any questions or comments regarding the contents of this
article please [open an issue on
Github](https://github.com/RodrigoZepeda/tbl.now/issues/new).

</div>

## Learning more

- A **tutorial** on real life surveillance data. Takes you from cleaning
  to diagnosing errors in the data to nowcasting:
  <https://rodrigozepeda.github.io/tbl.now/articles/example.html>
- The **second part of the tutorial** with a revision process: the
  optional third date, where a reported case is later confirmed,
  retracted or left pending:
  <https://rodrigozepeda.github.io/tbl.now/articles/example_revisions.html>
- The **Get started vignette**: the whole workflow, from a raw line list
  to a scored nowcast, in five minutes:
  <https://rodrigozepeda.github.io/tbl.now/articles/tbl.now.html>.
- **More on the `tbl_now` object**: every attribute, the three data
  types, the revision process, temporal effects and the `dplyr` methods:
  <https://rodrigozepeda.github.io/tbl.now/articles/more-on-tbl-now.html>
- More thoughts on **diagnosing your dataset** with `tbl.now`
  <https://rodrigozepeda.github.io/tbl.now/articles/diagnosing-a-tbl-now.html>
- Detecting reporting **batches** with `tbl.now`
  <https://rodrigozepeda.github.io/tbl.now/articles/batches.html>
- How to use different nowcasting engines from `tbl.now`: here you can
  learn **how it connects to the other nowcasting packages**.
  <https://rodrigozepeda.github.io/tbl.now/articles/nowcasting-models.html>
- How to **nowcast with multiple engines, backtest and ensemble**
  nowcasts.
  <https://rodrigozepeda.github.io/tbl.now/articles/ensemble-nowcasting.html>
- Adding your own **custom nowcasting model**
  <https://rodrigozepeda.github.io/tbl.now/articles/custom-nowcast-models.html>
- Package reference:
  <https://rodrigozepeda.github.io/tbl.now/reference/>
