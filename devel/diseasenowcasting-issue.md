# `nowcast()` fails to converge on `count-cumulative` input that fits instantly as `count-incidence`

**Package:** diseasenowcasting 2.1.0 · **R:** 4.5 (x86_64-apple-darwin)
**Reported from:** `tbl.now` 0.16.0, while building a converter-compatibility matrix

---

## Summary

Given one `tbl_now` object, `nowcast()` **fails to converge after ~25 minutes**
when the object's `data_type` is `"count-cumulative"`, and **succeeds in ~1
second** when the same data is passed as `"count-incidence"`.

The de-accumulated counts are byte-identical between the two paths — I checked
all 2276 cells. The difference is that `prepare_from_tbl_now()` switches the Stan
model into confirmation mode for cumulative input, and that mode does not
converge on this series.

```
Error: Joint fit failed to converge for all init attempts.
```

## Reproducible example

Uses `tbl.now::flusight` (FluSight weekly influenza hospitalisations, California).

```r
library(dplyr)
library(tbl.now)
library(diseasenowcasting)

cumulative <- flusight |>
  filter(location_name == "California", !is.na(observation),
         target_end_date >= as.Date("2024-01-01")) |>
  tbl_now(event_date = target_end_date, report_date = as_of,
          case_count = observation, data_type = "count-cumulative",
          align_weeks = TRUE, verbose = FALSE)

incidence <- to_count(cumulative, to = "count-incidence")

nowcast(incidence)    # ok, ~1.2 s
nowcast(cumulative)   # Error after ~25 min: Joint fit failed to converge
```

## What I ruled out

| | | |
|---|---|---|
| **`type = "one_stage"`** | not it | `nowcast()` already defaults to `type = "two_stage"` |
| **The delay axis being long** | not it | this series spans 0–97 weeks of delay, but capping it changes nothing — see the table |
| **The seed** | not it | deterministic across runs; and the difference is visible in the prepared Stan data before any sampling |
| **De-accumulation producing different counts** | not it | the two branches produce identical increments — see below |

Four variants, same object, delay axis capped with `filter(.delay <= n)`:

| variant | rows | max delay | result |
|---|---|---|---|
| cumulative, full | 2276 | 97 | **fail** (1512 s) |
| incidence, full | 2276 | 97 | ok (1.2 s) |
| cumulative, `delay <= 12` | 555 | 12 | **fail** (1464 s) |
| incidence, `delay <= 12` | 555 | 12 | ok (0.8 s) |

Capping the delay axis from 97 weeks to 12 makes no difference at all. The split
is entirely on `data_type`.

## The counts are the same

`prepare_from_tbl_now()` has two branches:

```r
incidence <- if (is_cumulative) {
  .deaccumulate_to_increments(as_of, event_col, report_col, event_unit, min_event, strata_cols)
} else {
  as.data.frame(tbl.now::to_count(as_of, to = "count-incidence"))
}
```

Both were run on this object and compared cell by cell:

```
.deaccumulate_to_increments : 2276 rows, sum 57156, range -54..3969, 45 negatives
tbl.now::to_count           : 2276 rows, sum 57156, range -54..3969, 45 negatives

common cells: 2276    disagreeing: 0
```

Identical, including the delay-0 column that carries the epidemic
(sum 40034, max 3969 in both). So the de-accumulation is not the problem.

## Where the two paths actually diverge

Comparing the full prepared Stan data (`prepare_from_tbl_now(x, model())$data`,
37 elements) for cumulative versus incidence input — everything matches except
three elements, all downstream of one flag:

| element | cumulative | incidence |
|---|---|---|
| `is_confirmation` | `1` | `0` |
| `max_conf_delay` | `98` | `0` |
| `increment_array` | `98 x 98 x 1` array | `NULL` |

and in `prepare_from_tbl_now()`:

```r
is_confirmation = is_cumulative,
```

So cumulative input does not merely get de-accumulated — it selects a different
model. That confirmation model is what fails to converge here, and it keeps
failing when the delay axis is capped (`increment_array` shrinks to `98 x 13 x 1`
and the fit still does not converge, still after ~24 minutes).

`max_conf_delay` does track the observed delay correctly (98 / 25 / 13 / 5 for
caps of 97 / 24 / 12 / 4), so that is not the issue.

## What might matter about this series

Offered as context, not as diagnosis:

* **Weekly**, 97 event weeks x 49 report dates, after `align_weeks = TRUE`
  (FluSight's `as_of` lands on both Saturdays and Wednesdays while
  `target_end_date` is always Saturday).
* The delay axis is really a **revision history** — every event date reappears in
  every later `as_of` snapshot — so the delay range equals the series length.
* **45 of 2276 increments are negative** (2%), the largest `-54`, from downward
  revisions. FluSight revises.
* Counts span three orders of magnitude, `-54` to `3969`.

## Two things that would help downstream

1. **A faster failure.** 25 minutes to report a convergence failure is expensive
   in any automated comparison. If the confirmation model can detect this earlier,
   or expose a max-iterations/timeout knob, that would help a lot.
2. **A documented escape hatch.** `is_confirmation` is hard-wired to
   `is_cumulative` inside `prepare_from_tbl_now()`, so from the outside the only
   way to avoid the confirmation model is to convert first:

   ```r
   nowcast(to_count(x, to = "count-incidence"))
   ```

   If that is a reasonable thing for users to do when the confirmation model
   struggles, saying so in `?nowcast` would be useful. If it is *not* — i.e. the
   confirmation model is doing something the incidence path cannot — then the
   ~1 s incidence fit above is quietly answering a different question, and that
   is worth a warning.

## Session info

```
diseasenowcasting 2.1.0
tbl.now           0.16.0
R                 4.5, x86_64-apple-darwin
```
