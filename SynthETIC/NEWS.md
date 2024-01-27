# SynthETIC

# SynthETIC 1.1.0

## Inclusion of `claim_covariates` sub-module

* Extends the package by including support for covariates and their impact on 
claim sizes.

* Includes one new vignette, demonstrating the sub-module and its use-cases.

* Includes five new data objects. The three existing datasets 
`test_claims_object`, `test_claim_dataset` and `test_transaction_dataset` now 
also have a version where outputs are impacted by the effect of covariates. A
`test_covariates_obj` and `test_covariates_dataset` have been included which
provide information regarding the specific factors and levels of covariates used
in relation to each claim.

## Minor improvements and fixes 

* Updates citation

# SynthETIC 1.0.5

## Minor improvements and fixes 

* Removes dependency (`Suggests`) on the `poisson` package

# SynthETIC 1.0.4

## Minor improvements and fixes 

* Removes dependency (`Suggests`) on the `ChainLadder` package

# SynthETIC 1.0.3

## New features {#new-features-1.0.3}

* Adds `plot_transaction_dataset()` function for plotting claim development from
a payment-level dataset, in addition to the `plot.claims` method that works only
with a `claims` object.

# SynthETIC 1.0.2

## Minor improvements and fixes 

* Updates citation

* Fixes a bug in the default sampling function of `claim_payment_no()`

* Fixes the treatment of `params_split` to allow for trivial params

# SynthETIC 1.0.1

## New features {#new-features-1.0.1}

* Updates `claim_output()` to allow an option to `adjust` the treatment of
out-of-bound transactions in tabulation. The previous version (`1.0.0`) 
automatically forced all out-of-bound transactions to be paid at the exact end 
of the maximum development period allowed. This is now set as the default
behaviour, but the user can also set `adjust = FALSE` to see the proportion of
payments projected to fall beyond the maximum development period (the "tail").

## Minor improvements and fixes 

* Updates `claim_output()` to force `adjust` for higher `aggregate_level`. The
issue with previous function was that it ignored payments beyond the max
development period after aggregation. This is now fixed - either merged to the
end DQ, or in a separate `tail` cell (if `adjust = FALSE`).


# SynthETIC 1.0.0

## New features {#new-features-1.0.0}

* Both `claim_frequency()` and `claim_size()` now allow users to input a random
generation function (by specifying a `simfun` and setting `type = "r"`), in 
addition to cdf inputs (`type = "p"`) allowed by the predecessor version.
  - Also improves the simulation efficiency. When a cdf is given, the function
  will first search for the existence of the corresponding `r`-function and if
  it finds one, apply the `r`-function directly instead of inverse sampling from
  the cdf.

* Relaxes assumptions for `claim_notification()` and `claim_closure()`.
  - `SynthETIC 0.1.0` assumes Weibull and only allows changes to mean and cv.
  - The new version relaxes this assumption and allows user to input any
  sampling distribution through `rfun` and `paramfun`.

* Sets the `benchmark`s in `claim_payment_no()` function optional (and will only
be read if the default simulation function is used).

* `claim_payment_no()` is now also implemented through `rfun` and `paramfun`,
instead of the original `simulate_no_pmt_function` (which works in the same way
as `rfun` but without the functionality to change parameters).

* `claim_payment_size()` is also implemented through `rfun` and `paramfun`, 
replacing the original `simulate_amt_pmt_function`.

* New `to_SynthETIC()` function helps the conversion from externally simulated
objects to `SynthETIC` format for easy integration with other simulation 
functions/modules.

## Minor improvements and fixes 

* Fixed a few typos in function documentation.

* `claim_output()` function is updated to correctly calculate triangles on a
higher aggregate level (from square to parallelogram-shaped).

* The package vignette has been updated to reflect all the changes listed above
and show more examples where the quantities are simulated from distributions
other than the default.

* The GitHub repo address is added to the `DESCRIPTION` file.

* The following don't affect the users of the package:
  - the `test_data.R` file under `data-raw` has been updated to reflect the 
  changes listed under the heading ["new features"](#new-features-1.0.0).
  - the `DESCRIPTION` file has been updated to include the new reference made
  in the vignette (e.g. `actuar`).


