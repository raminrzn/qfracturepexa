.qf_vars <- c("sex", "age", "bmi", "ethnicity", "smoking", "alcohol", "diabetes",
              "antidepressants", "any_cancer", "asthma_copd", "corticosteroids", "cvd",
              "dementia", "endocrine", "epilepsy", "falls", "prior_fracture", "hrt_oestrogen",
              "liver_disease", "malabsorption", "parkinsons", "ra_sle", "renal_disease",
              "care_home", "parental_history", "years")
.qf_alias <- c(ethrisk = "ethnicity", smoke_cat = "smoking", alcohol_cat6 = "alcohol",
               surv = "years", b_antidepressant = "antidepressants", b_anycancer = "any_cancer",
               b_asthmacopd = "asthma_copd", b_corticosteroids = "corticosteroids",
               b_cvd = "cvd", b_dementia = "dementia", b_endocrine = "endocrine",
               b_epilepsy2 = "epilepsy", b_falls = "falls", b_fracture4 = "prior_fracture",
               b_hrt_oest = "hrt_oestrogen", b_liver = "liver_disease",
               b_malabsorption = "malabsorption", b_parkinsons = "parkinsons",
               b_ra_sle = "ra_sle", b_renal = "renal_disease", b_carehome = "care_home",
               fh_osteoporosis = "parental_history")

#' QFracture-2012 (ModelsCloud entry point)
#' @param model_input Named list of inputs; see [qfracture_risk()].
#' @param ... The same fields as named arguments.
#' @return See [qfracture_risk()].
#' @export
model_run <- function(model_input = NULL, ...) {
  args <- .qf_normalize(model_input, list(...), .qf_vars, .qf_alias)
  if (is.null(args)) stop("No input supplied.", call. = FALSE)
  do.call(qfracture_risk, args)
}

#' Example QFracture-2012 inputs
#' @param n Optional number of rows.
#' @param ... Ignored.
#' @export
get_sample_input <- function(n = NULL, ...) {
  rows <- list(get_default_input(),
               list(sex = "male", age = 70, bmi = 22, ethnicity = "white_or_not_recorded",
                    smoking = "heavy", alcohol = "none", any_cancer = "yes", falls = "yes",
                    antidepressants = "yes", years = 10))
  if (!is.null(n)) rows <- utils::head(rows, n)
  rows
}

#' Default QFracture-2012 input
#'
#' The first clinical example in the paper: a 64-year-old white woman, a
#' light smoker with type 1 diabetes and epilepsy, BMI 22 (alcohol is not
#' stated in the paper; "none" here).
#' @param ... Ignored.
#' @export
get_default_input <- function(...) {
  list(sex = "female", age = 64, bmi = 22, ethnicity = "white_or_not_recorded",
       smoking = "light", alcohol = "none", diabetes = "type1", epilepsy = "yes", years = 10)
}
