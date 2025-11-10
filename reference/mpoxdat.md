# mpoxdat: Mpox reporting data from the 2022 New York City outbreak

Surveillance line list data provided by the New York City (NYC) Health
Department at https://github.com/nychealth/mpox_nowcast_eval, to
accompany a nowcasting performance evaluation (doi: 10.2196/56495).
Patients with a confirmed or probable mpox diagnosis or illness onset
from July 8 through September 30, 2022 were included. The original
dataset was aggregated and pre-processed as described in the note below.

## Usage

``` r
data(mpoxdat)
```

## Format

A data frame.

## Details

This is line-list data with each row representing case counts. The
columns are as follows:

- `dx_date`: is the specimen collection date of the first positive mpox
  laboratory result,

- `dx_report_date`: is the date the report of first positive mpox
  laboratory result was received by the NYC Health Department,

- `n`: the case count of individuals within those dates.

- `race`: the race corresponding to those cases. Race was randomly
  assigned with probabilities "Non-Hispanic White" = 0.309, "Hispanic" =
  0.283, "Black" = 0.202, "Asian" = 0.156, and "Other" = 0.05 which
  follow what has been reported for the US Census.

## Note

While `dx_date`, `dx_report_date` and `n` correspond to actual observed
data the `race` was constructed exclusively for the examples of this
package. Its a synthetic (simulated) variable and does not correspond to
any reality.

## References

ROHRER, Rebecca, et al. Nowcasting to Monitor Real-Time Mpox Trends
During the 2022 Outbreak in New York City: Evaluation Using Reportable
Disease Data Stratified by Race or Ethnicity. Online Journal of Public
Health Informatics, 2025, vol. 17, no 1, p. e56495.

## Examples

``` r
data(mpoxdat)
```
