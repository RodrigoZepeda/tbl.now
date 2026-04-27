# tbl.now — Package Summary for Claude

## Purpose

`tbl.now` provides the `tbl_now` S3 class — an extension of `tibble` designed for
epidemiological nowcasting.  It stores **two time indices simultaneously**
(`event_date` and `report_date`) together with modelling metadata (strata,
covariates, temporal effects, etc.) and integrates seamlessly with dplyr.

The main downstream consumer is `diseasenowcasting`.

---

## Core Class: `tbl_now`

### Creating a `tbl_now`

```r
tbl_now(
  data,
  event_date,       # <tidy-select> column of event dates (Date or integer)
  report_date,      # <tidy-select> column of report dates
  strata    = NULL, # <tidy-select> stratification variables
  covariates = NULL,# <tidy-select> covariate columns
  case_count = NULL,# <tidy-select> counts column (count data types only)
  is_censored = NULL,
  now       = NULL, # Date; inferred from max(report_date) if NULL
  event_units = "auto",   # "days","weeks","months","years","numeric"
  report_units = "auto",
  data_type = "auto",     # "linelist","count-incidence","count-cumulative"
  t_effects = character(0), # temporal_effects() object — stored LAZILY
  verbose   = TRUE,
  force     = FALSE,
  warn_non_uniqueness = TRUE,
  align_weeks = FALSE
)
```

**Auto-generated columns** (protected — removing them downgrades to tibble):

| Column | Description |
|--------|-------------|
| `.event_num`  | Numeric version of `event_date` anchored at `min(event_date)` |
| `.report_num` | Numeric version of `report_date` anchored at same anchor |
| `.delay`      | `.report_num - .event_num` |

### Key validators / predicates

```r
is_tbl_now(x)                  # TRUE if valid tbl_now
validate_tbl_now(x)            # throws on invalid
tbl_now_can_reconstruct(x)     # safe wrapper returning TRUE/FALSE
tbl_now_attributes(x)          # list of tbl_now-specific attributes
```

---

## Attributes & Getters

Every attribute has a `get_*()` getter:

```r
get_event_date(x)      # character(1) — column name
get_report_date(x)     # character(1) — column name
get_strata(x)          # character or NULL
get_num_strata(x)      # integer
get_covariates(x)      # character or NULL
get_num_covariates(x)  # integer
get_now(x)             # Date — the nowcast reference date
get_event_units(x)     # "days"|"weeks"|"months"|"years"|"numeric"
get_report_units(x)
get_data_type(x)       # "linelist"|"count-incidence"|"count-cumulative"
get_case_count(x)      # character(1) or NULL
get_is_censored(x)     # character(1) or NULL
get_temporal_effects(x)      # list of lazy specs (see below)
get_temporal_effect_cols(x)  # character — computed column names (after compute_temporal_effects())
```

---

## Data Types

| Type | Description |
|------|-------------|
| `"linelist"` | Each row = one individual observation |
| `"count-incidence"` | Each row = count reported **exactly** on `report_date` for `event_date` |
| `"count-cumulative"` | Each row = **cumulative** count up to `report_date` for `event_date` |

Convert between types with `to_count(x, to = "count-incidence")`.

---

## Temporal Effects — Lazy Evaluation

Temporal effects follow a **two-step lazy pattern**:

### Step 1: Attach the spec (no columns added)

```r
t_eff <- temporal_effects(
  day_of_week   = FALSE,
  weekend       = FALSE,
  day_of_month  = FALSE,
  month_of_year = FALSE,
  week_of_year  = FALSE,
  seasons       = integer(0),   # Fourier: c(7, 52, 365) for weekly+annual
  holidays      = NULL          # almanac::rcalendar() or NULL
)

# attach to tbl_now — NO columns created yet
df_now <- df_now |> add_temporal_effects(t_eff, date_type = "event_date")
# date_type: "event_date" (default) or "report_date"
```

`add_temporal_effects()` can be called multiple times; each call appends a new
spec to the list.  Multiple calls with different `date_type` values are
supported.

`tbl_now(data, ..., t_effects = t_eff)` also stores the spec lazily.

### Step 2: Materialise the columns

```r
df_computed <- compute_temporal_effects(df_now, overwrite = FALSE)
```

This calls the underlying `add_temporal_effects.data.frame()` for each stored
spec and records the created column names in the `computed_temporal_effect_cols`
attribute.

### Inspecting state

```r
get_temporal_effects(df_now)       # list of specs; length > 0 → spec attached
get_temporal_effect_cols(df_now)   # character(0) before compute; col names after
```

### Printing

The `tbl_now` footer shows:
- `T. effects (lazy): [event_date] week_of_year, month_of_year` — before computation
- `T. effects: [event_date] week_of_year, month_of_year` + `T. effect cols: ".event_week_of_year" ...` — after computation

Computed columns are annotated `[t_effect]` in the pillar output.

### Key invariant: dplyr operations preserve the spec

`filter()`, `select()`, `mutate()`, `group_by()`, `arrange()` and all other
dplyr verbs **preserve the `temporal_effects` spec unchanged**.  They never
trigger computation.  Only `compute_temporal_effects()` adds columns.

If a **computed** column is removed via `select()`, it is dropped from
`computed_temporal_effect_cols` but the spec in `temporal_effects` is preserved.

### Removing / replacing effects

```r
remove_temporal_effects(x)               # clears spec AND removes computed cols
replace_temporal_effects(x, t_effects)   # replaces with new spec (and drops old cols)
```

---

## Changers, Adders, Removers

### Changers (replace the attribute)

```r
change_now(x, now)
change_event_date(x, event_date)
change_report_date(x, report_date)
change_strata(x, ...)
change_covariates(x, ...)
change_case_count(x, case_count)
change_is_censored(x, is_censored)
```

### Adders (append to existing)

```r
add_strata(x, ...)
add_covariates(x, ...)
add_is_censored(x, is_censored)
add_temporal_effects(x, t_effects, date_type = "event_date", weekend_days = c("Sat","Sun"))
```

### Removers

```r
remove_strata(x, ...)
remove_all_strata(x)
remove_covariates(x, ...)
remove_all_covariates(x)
remove_is_censored(x)
remove_temporal_effects(x)
replace_temporal_effects(x, t_effects)
```

---

## dplyr Integration

`tbl_now` implements all major dplyr generics:

| Generic | Behaviour |
|---------|-----------|
| `filter` / `slice` | `dplyr_row_slice` → reconstructs tbl_now |
| `select` / `mutate` | `dplyr_col_modify` → reconstructs; drops class if protected col removed |
| `group_by` | returns `grouped_tbl_now`; attributes preserved |
| `ungroup` | reconstructs tbl_now |
| `summarise` / `reframe` | attempts reconstruction; falls back to tibble |
| `rename` / `rename_with` | updates attribute references automatically |

Removing a **protected** column (`.event_num`, `.report_num`, `.delay`,
`event_date`, `report_date`, or `case_count` for count types) downgrades the
object to a plain tibble with a warning.

Removing a **strata** or **covariate** column silently drops that column from
the attribute.

---

## Utility Functions

```r
to_count(x, to)            # convert data type
complete_zeroes(x, ...)    # fill missing event/report/strata combos with 0
update(x, new_data, ...)   # bind new data, preserving attributes
align_weeks(x, date_col)   # align dates to consistent day within epiweek
week_2_date(x, week_col, year_col)  # epiweek + year → Date
get_initial_reported_cases(x)       # first-reported counts per event_date
get_latest_reported_cases(x)        # most-recently-reported counts
is_weekday(date, weekend_days)      # TRUE if weekday, FALSE if weekend
tbl_now_attributes(x)               # list of tbl_now-specific attrs only
```

---

## Package Data

```r
data(denguedat)   # dengue surveillance linelist (weekly, Brazil)
data(mpoxdat)     # mpox count-incidence data
```

---

## Typical Workflow

```r
library(tbl.now)
library(dplyr)

# 1. Create
df_now <- denguedat |>
  tbl_now(event_date = onset_week, report_date = report_week,
          strata = gender, verbose = FALSE)

# 2. Optionally attach temporal-effects spec (lazy)
df_now <- df_now |>
  add_temporal_effects(temporal_effects(week_of_year = TRUE, month_of_year = TRUE))

# 3. Apply dplyr freely — spec is untouched
df_filtered <- df_now |> filter(gender == "Male")

# 4. Materialise when ready for modelling
df_ready <- compute_temporal_effects(df_filtered)

# 5. Inspect
get_temporal_effect_cols(df_ready)
```

---

## Common Pitfalls

- **Do not call `compute_temporal_effects()` before `add_temporal_effects()`** — if no spec is stored the call is a no-op.
- **`get_temporal_effects()` returns a list of specs** (not column names). Use `get_temporal_effect_cols()` for column names.
- **Reconstruction via `tbl_now()` inside group_by/ungroup/summarise** does NOT pass `t_effects`; the spec is copied manually afterward. This is intentional.
- **`replace_temporal_effects(x, t_effects)`** removes computed columns before storing the new spec — call `compute_temporal_effects()` again afterward.
- **Week-aligned data**: use `align_weeks = TRUE` in `tbl_now()` or call `align_weeks()` explicitly to ensure integer `.delay` values for weekly data.
