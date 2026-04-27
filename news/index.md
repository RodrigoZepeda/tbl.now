# Changelog

## tbl.now 0.6.0

- Changed temporal effects to be lazy (as required by \#17) so that now
  its easier to use `dplyr` functions without compromising them.
- Bumped the deprecated dplyr’s `*_at` functions to use
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
- Fixed to no warnings during test.
- Users can now pass the `.delay` column directly (#6) and it will
  recalculate the missing column (i.e. event or report)
- Added `complete_zeroes` to vignette (#13).
