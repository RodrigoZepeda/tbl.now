# Marginal model fails to compile: `primarycensored_lpmf` called with 8 arguments, signature takes 9

**Package:** epidist 0.4.0
**Blocking:** any `as_epidist_marginal_model()` fit
**Not affected:** `as_epidist_latent_model()` works fine

## Summary

With epidist 0.4.0 and primarycensored 1.5.1, every marginal-model fit fails at
Stan compilation. The Stan code epidist generates calls `primarycensored_lpmf`
with **8** arguments, but primarycensored's current signature takes **9** — it
gained an `L` (lower truncation) parameter that epidist's generated call does not
supply.

## Reproducible example

```r
library(epidist)
library(dplyr)

set.seed(1)
obs <- sierra_leone_ebola_data |>
  slice_sample(n = 500) |>
  as_epidist_linelist_data(
    pdate_lwr = "date_of_symptom_onset",
    sdate_lwr = "date_of_sample_tested"
  )

# Fails at compilation
fit <- obs |>
  as_epidist_marginal_model() |>
  epidist(chains = 2, iter = 500, seed = 1)

# Works
fit_latent <- obs |>
  as_epidist_latent_model() |>
  epidist(chains = 2, iter = 500, seed = 1)
```

## Error

```
Semantic error in 'string', line 27, column 9 to line 30, column 5:
   -------------------------------------------------
    25:                              data real pwindow_width, data real swindow_width,
    26:                              data real y_upper, array[] real primary_params) {
    27:    return primarycensored_lpmf(
                  ^
    28:        y | 1, {mu, sigma}, pwindow_width, y_upper, relative_obs_t,
    29:        1, primary_params
   -------------------------------------------------

Ill-typed arguments supplied to function "primarycensored_lpmf":
(int, int, array[] real, real, real, real, int, array[] real)
Available signatures:
(data int, data int, array[] real, data real, data real, data real,
 data real, data int, array[] real) => real
  Expected 9 arguments but found 8 arguments.
```

## Cause

**epidist 0.4.0**, `stan/marginal_model/functions.stan` (lines 25–28) passes 8
arguments:

```stan
return primarycensored_lpmf(
    y | dist_id, {dpars_B}, pwindow_width, y_upper, relative_obs_t,
    primary_id, primary_params
  );
```

i.e. `d | dist_id, params, pwindow, d_upper, D, primary_id, primary_params`.

**primarycensored 1.5.1**, `stan/functions/primarycensored.stan` (line 293)
declares 9:

```stan
real primarycensored_lpmf(data int d, data int dist_id, array[] real params,
                                data real pwindow, data real d_upper,
                                data real L, data real D, data int primary_id,
                                array[] real primary_params) {
```

i.e. `d | dist_id, params, pwindow, d_upper, L, D, primary_id, primary_params`.

The `L` argument (minimum delay / lower truncation point) sits between `d_upper`
and `D`. epidist's call supplies `relative_obs_t` in the `L` position and then
runs out of arguments, so `primary_id` and `primary_params` land one slot early.

## Confirmation

Inserting `L = 0` at the right position makes the model compile and sample. Patching
only the generated stanvars, leaving everything else untouched:

```r
sv <- epidist_stancode(data = tdat, family = fam, formula = form)
sv[[2]]$scode <- sub(
  "pwindow_width, y_upper, relative_obs_t,",
  "pwindow_width, y_upper, 0, relative_obs_t,",
  sv[[2]]$scode, fixed = TRUE
)
```

The model then compiles and samples normally. On a weekly dengue line list
(n = 500) the patched marginal model gives a lognormal delay with
mean 12.58 days (95% CrI 11.88–13.29); the latent model on the same data gives
12.97 (12.11–13.91), so the patched marginal model agrees with the unaffected
model — the fix looks correct, not merely compilable.

## Suggested fix

In `stan/marginal_model/functions.stan`, pass the lower truncation point
explicitly:

```stan
return primarycensored_lpmf(
    y | dist_id, {dpars_B}, pwindow_width, y_upper, 0, relative_obs_t,
    primary_id, primary_params
  );
```

If the marginal model is ever meant to support a non-zero lower truncation, `L`
would want to be threaded through as a data argument rather than hard-coded, but
`0` restores the previous behaviour.

A minimum version constraint on primarycensored in `DESCRIPTION` would also stop
the two packages drifting apart silently.

## Session info

```
R version 4.5.3 (2026-03-11)
Platform: x86_64-apple-darwin20

epidist         0.4.0
primarycensored 1.5.1
brms            2.23.1
rstan           2.36.0.9000
StanHeaders     2.36.0.9000
```
