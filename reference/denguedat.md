# denguedat: Dengue fever individual-level reporting data from Puerto Rico

Surveillance data from CDC Division of Vector-Borne Diseases. 1990-2010
case reporting data included.

## Usage

``` r
data(denguedat)
```

## Format

A data frame.

## Details

Each row represents a case with the columns indicating the following:

- `onset_week`: the week of symptom onset.

- `report_week`: the week of case report.

- `gender`: the gender of the infected individual (randomly assigned
  with 0.5:0.5 probability of "Male"/"Female").

## Note

Data originally from the `NobBS` package. While `onset_week` and
`report_week` correspond to actual observed data the `gender` was
constructed exclusively for the examples of `NobBS`. Its a synthetic
(simulated) variable and does not correspond to any reality.

## References

MCGOUGH, Sarah F., et al. Nowcasting by Bayesian Smoothing: A flexible,
generalizable model for real-time epidemic tracking. PLoS computational
biology, 2020, vol. 16, no 4, p. e1007735.

## Examples

``` r
data(denguedat)
```
