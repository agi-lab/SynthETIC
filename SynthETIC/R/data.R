#############################################################################
##                    Datasets Included in This Package                    ##
#############################################################################

#' Claims Data in List Format
#'
#' A list containing a sample output from each of the simulation modules, in
#' sequential order of the running of the modules. This is the test data
#' generated when run with seed `20200131` at the top of the code.
#'
#' @format A `claims` object with 10 components:
#' \describe{
#'   \item{frequency_vector}{vector; number of claims for each occurrence period, see also `claim_frequency()`.}
#'   \item{occurrence_list}{list; claim occurrence times for all claims that occurred in each of the period, see also `claim_occurrence()`.}
#'   \item{claim_size_list}{list; claim sizes for all claims that occurred in each of the period, see also `claim_size()`.}
#'   \item{notification_list}{list; notification delays for all claims that occurred in each of the period, see also `claim_notification()`.}
#'   \item{settlement_list}{list; settlement delays for all claims that occurred in each of the period, see also `claim_closure()`.}
#'   \item{no_payments_list}{list; number of partial payments for all claims that occurred in each of the period, see also `claim_payment_no()`.}
#'   \item{payment_size_list}{(compound) list; sizes of partial payments (without inflation) for all claims that occurred in each of the period, see also `claim_payment_size()`.}
#'   \item{payment_delay_list}{(compound) list; inter partial delays for all claims that occurred in each of the period, see also `claim_payment_delay()`.}
#'   \item{payment_time_list}{(compound) list; payment times (on a continuous time scale) for all claims that occurred in each of the period, see also `claim_payment_time()`.}
#'   \item{payment_inflated_list}{(compound) list; sizes of partial payments (with inflation) for all claims that occurred in each of the period, see also `claim_payment_inflation()`.}
#' }
#' @seealso \enumerate{
#'   \item Claim occurrence: \code{\link{claim_frequency}}, \code{\link{claim_occurrence}}
#'   \item Claim size: \code{\link{claim_size}}
#'   \item Claim notification: \code{\link{claim_notification}}
#'   \item Claim closure: \code{\link{claim_closure}}
#'   \item Claim payment count: \code{\link{claim_payment_no}}
#'   \item Claim payment size (without inflation): \code{\link{claim_payment_size}}
#'   \item Claim payment time: \code{\link{claim_payment_delay}}, \code{\link{claim_payment_time}}
#'   \item Claim inflation: \code{\link{claim_payment_inflation}}
#' }
#' @examples
#' test_claims_object$frequency_vector
"test_claims_object"


#' Claims Dataset
#'
#' A dataset of 3,624 rows containing individual claims features.
#'
#' @format A data frame with 3,624 rows and 7 variables:
#' \describe{
#'   \item{claim_no}{claim number, which uniquely characterises each claim.}
#'   \item{occurrence_period}{integer; period of ocurrence of the claim.}
#'   \item{occurrence_time}{double; time of occurrence of the claim.}
#'   \item{claim_size}{size of the claim (in constant dollar values).}
#'   \item{notidel}{notification delay of the claim, i.e. time from occurrence to notification.}
#'   \item{setldel}{settlement delay of the claim, i.e. time from notification to settlement.}
#'   \item{no_payment}{number of partial payments required for the claim.}
#' }
#' @examples
#' # see a distribution of payment counts
#' table(test_claim_dataset$no_payment)
"test_claim_dataset"


#' Transactions Dataset
#'
#' A dataset of 18,983 records of partial payments associated with the 3,624
#' claims in `test_claim_dataset`.
#'
#' @format A data frame with 18,983 rows and 12 variables:
#' \describe{
#'   \item{claim_no}{claim number, which uniquely characterises each claim.}
#'   \item{pmt_no}{payment number, identification number of partial payments in respect of a particular `claim_no`.}
#'   \item{occurrence_period}{integer; period of ocurrence of the claim.}
#'   \item{occurrence_time}{double; time of occurrence of the claim.}
#'   \item{claim_size}{size of the claim (in constant dollar values).}
#'   \item{notidel}{notification delay of the claim, i.e. time from occurrence to notification.}
#'   \item{setldel}{settlement delay of the claim, i.e. time from notification to settlement.}
#'   \item{payment_time}{double; time of payment (on a continuous time scale).}
#'   \item{payment_period}{integer; time of payment (in calendar period).}
#'   \item{payment_size}{size of the payment in constant dollar terms.}
#'   \item{payment_inflated}{actual size of the payment (i.e. with inflation).}
#'   \item{payment_delay}{inter partial delay associated with the payment.}
#' }
"test_transaction_dataset"

#' Covariates Object
#'
#' A list containing the frequency and severity relativities for three factors.
#'
#' @format A `covariates` object with 3 components:
#' \describe{
#'   \item{factors}{list; levels within each factor.}
#'   \item{relativity_freq}{data.frame; first and second order frequency relativities between all the levels of each factor}
#'   \item{relativity_sev}{data.frame; first and second order severity relativities between all the levels of each factor}
#' }
#' @examples
#' test_covariates_obj$factors
"test_covariates_obj"


#' Covariates Data Object
#'
#' An object detailing the set of covariates for each claim in the default setting of `SynthETIC`.
#'
#' @format A `covariates_data` object with 3 components:
#' \describe{
#'   \item{data}{data.frame; a dataset of covariate levels}
#'   \item{covariates}{covariates; a `covariates` object}
#'   \item{ids}{list; indices of the covariate-level dataset for each claim}
#' }
#' @examples
#' test_covariates_dataset$data
"test_covariates_dataset"

#' Claims Data in List Format
#'
#' The `test_claims_object` where the default set of covariates have been applied to adjust claim sizes.
#'
#' @examples
#' test_claims_object$frequency_vector
"test_claims_object_cov"


#' Claims Dataset
#'
#' The `test_claim_dataset` where the default set of covariates have been applied to adjust claim sizes.
#'
#' @examples
#' # see a distribution of payment counts
#' table(test_claim_dataset_cov$no_payment)
"test_claim_dataset_cov"


#' Transactions Dataset
#'
#' The `test_transaction_dataset` where the default set of covariates have been applied to adjust claim sizes.
#'
"test_transaction_dataset_cov"
