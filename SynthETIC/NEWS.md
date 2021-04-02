# SynthETIC

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


