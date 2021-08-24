# SynthETIC [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/SynthETIC)](https://CRAN.R-project.org/package=SynthETIC)
SynthETIC Claim Simulator

## Introduction
[`SynthETIC`](https://CRAN.R-project.org/package=SynthETIC) is an individual claims simulator which generates various features of non-life insurance claims. An initial set of test parameters, designed to mirror the experience of an Auto Liability portfolio, were set up and applied by default to generate a realistic test data set of individual claims (see vignette). The simulated data set then allows practitioners to back-test the validity of various reserving models and to prove and/or disprove certain actuarial assumptions made in claims modelling. The distributional assumptions used to generate this data set can be easily modified by users to match their experiences. Reference: Avanzi B, Taylor G, Wang M, Wong B (2020) "SynthETIC: an individual insurance claim simulator with feature control" <[arXiv:2008.05693](https://arxiv.org/abs/2008.05693)>.

## Reference
For a full description of `SythETIC`'s structure and test parameters, readers should refer to:

Avanzi, B., Taylor, G., Wang, M., Wong, B., 2021. `SynthETIC`: An individual insurance claim simulator with feature control. *Insurance: Mathematics and Economics* 100, 296–308. https://doi.org/10.1016/j.insmatheco.2021.06.004

A free copy is accessible via [arXiv:2008.05693](https://arxiv.org/abs/2008.05693).

## Install Package
To install the [CRAN version of the package](https://CRAN.R-project.org/package=SynthETIC), do

`install.packages("SynthETIC")`

To install the development version of the package from this GitHub repository, do

```
if (!require(remotes)) install.packages("remotes")
remotes::install_github("agi-lab/SynthETIC/SynthETIC", build_vignettes = TRUE)
```

After the installation, run

`library(SynthETIC)`

as you would normally do will load the package. View a [full demonstration](https://cran.r-project.org/web/packages/SynthETIC/vignettes/SynthETIC-demo.html) of the package (which is used to generate the built-in test datasets discussed in the paper) by running

`RShowDoc("SynthETIC-demo", package = "SynthETIC")`

## Additional Resources
* [Chain ladder analysis for the test data set](https://github.com/agi-lab/SynthETIC/blob/master/CL_Test_Dataset.xlsx)
* [`SynthETIC` reference manual (pdf)](https://github.com/agi-lab/SynthETIC/blob/master/SynthETIC-manual.pdf). Alternatively, view it on [CRAN](https://cran.r-project.org/web/packages/SynthETIC/SynthETIC.pdf) or use the help documentation once the package is installed and loaded.
