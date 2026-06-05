# tbl.now 0.6.4

* Fixed dependency on R >= 4.2.0
* Update function now defaults the censoring to FALSE if the update
is censored but the original is not. 

# tbl.now 0.6.3

* Added season length to seasons so we can get weekly seasonality. 

# tbl.now 0.6.2

* Removed warning when using columns for temporal effects that cascaded into `to_count`.
* Changed DESCRIPTION to fix ortographic error and trigger less messages of unknown words. 

# tbl.now 0.6.1

* Changed links in description of `tidy-select`

# tbl.now 0.6.0

* Changed temporal effects to be lazy (as required by #17) so that now its
easier to use `dplyr`
functions without compromising them. 
* Bumped the deprecated dplyr's `*_at` functions to use `all_of()`
* Fixed to no warnings during test. 
* Users can now pass the `.delay` column directly (#6) and it will recalculate 
the missing column (i.e. event or report)
* Added `complete_zeroes` to vignette (#13).
