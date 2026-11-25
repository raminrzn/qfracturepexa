#' QFracture-2012: risk of osteoporotic and hip fracture
#'
#' Risk of a major osteoporotic fracture (hip, wrist, shoulder or spine) and
#' of a hip fracture over 1 to 18 years (default 10), from the QFracture-2012
#' equations (Hippisley-Cox and Coupland, BMJ 2012). The equations are
#' translated from ClinRisk's QFracture-2012 C code (LGPL-3), with the same
#' input ranges, and match the compiled C code to within 1e-12 (see README).
#'
#' Conditions are as recorded in primary care; see the README for
#' definitions. Some predictors are in only some equations: endocrine
#' disorders and HRT only for women, care home only for men, malabsorption
#' only for major osteoporotic fracture, and parental history in both male
#' equations and the female major-fracture equation. A "yes" for a predictor
#' that an equation does not use is ignored, with a note.
#'
#' @param sex "female" or "male".
#' @param age Age in whole years, 30 to 100.
#' @param bmi Body mass index, kg/m2, 20 to 40.
#' @param ethnicity Code 1-9 or label: `white_or_not_recorded` (1),
#'   `indian` (2), `pakistani` (3), `bangladeshi` (4), `other_asian` (5),
#'   `black_caribbean` (6), `black_african` (7), `chinese` (8), `other` (9,
#'   other ethnic group, including mixed).
#' @param smoking Code 0-4 or label: `non` (0), `ex` (1), `light` (2, under
#'   10 cigarettes a day), `moderate` (3, 10-19), `heavy` (4, 20 or more).
#' @param alcohol Code 0-5 or label: `none` (0), `trivial` (1, under 1 unit
#'   a day), `light` (2, 1-2 units), `moderate` (3, 3-6 units), `heavy` (4,
#'   7-9 units), `very_heavy` (5, more than 9 units).
#' @param diabetes "none", "type1" or "type2".
#' @param antidepressants,any_cancer,asthma_copd,corticosteroids,cvd,dementia,endocrine,epilepsy,falls,prior_fracture,hrt_oestrogen,liver_disease,malabsorption,parkinsons,ra_sle,renal_disease,care_home,parental_history
#'   yes/no (default no). See README for definitions.
#' @param years Time horizon, whole years 1 to 18 (default 10).
#' @return A list: `model`, `years`, `risk_percent` and `risk` (major
#'   osteoporotic and hip fracture, unrounded), `outcomes`, `inputs_coded`
#'   (the values passed to each equation), `disclaimer` (ClinRisk's required
#'   disclaimer, which must be shown with the scores) and `notes`.
#' @export
qfracture_risk <- function(sex, age, bmi, ethnicity, smoking, alcohol, diabetes = "none",
                           antidepressants = "no", any_cancer = "no", asthma_copd = "no",
                           corticosteroids = "no", cvd = "no", dementia = "no",
                           endocrine = "no", epilepsy = "no", falls = "no",
                           prior_fracture = "no", hrt_oestrogen = "no", liver_disease = "no",
                           malabsorption = "no", parkinsons = "no", ra_sle = "no",
                           renal_disease = "no", care_home = "no", parental_history = "no",
                           years = 10) {
  sex <- .qf_choice(sex, "sex", c(female = "female", f = "female", male = "male", m = "male"))
  dm <- .qf_choice(diabetes, "diabetes", c(none = "none", no = "none", type1 = "type1",
                                            t1 = "type1", `1` = "type1", type2 = "type2",
                                            t2 = "type2", `2` = "type2"))
  x <- list(
    age = .qf_int(age, "age", 30, 100),
    bmi = .qf_num(bmi, "bmi", 20, 40),
    ethrisk = .qf_code(ethnicity, "ethnicity", .qf_ethnicity),
    smoke_cat = .qf_code(smoking, "smoking", .qf_smoking),
    alcohol_cat6 = .qf_code(alcohol, "alcohol", .qf_alcohol),
    surv = .qf_int(years, "years", 1, 18),
    b_type1 = as.integer(dm == "type1"),
    b_type2 = as.integer(dm == "type2"))
  flags <- list(antidepressants = antidepressants, any_cancer = any_cancer,
                asthma_copd = asthma_copd, corticosteroids = corticosteroids, cvd = cvd,
                dementia = dementia, endocrine = endocrine, epilepsy = epilepsy, falls = falls,
                prior_fracture = prior_fracture, hrt_oestrogen = hrt_oestrogen,
                liver_disease = liver_disease, malabsorption = malabsorption,
                parkinsons = parkinsons, ra_sle = ra_sle, renal_disease = renal_disease,
                care_home = care_home, parental_history = parental_history)
  for (f in names(flags)) x[[.qf_flag_param[[f]]]] <- .qf_yesno(flags[[f]], f)

  eq <- if (sex == "female") c(major = "fracture4_female", hip = "nof_female") else
    c(major = "fracture4_male", hip = "nof_male")
  notes <- character(0)
  for (f in names(flags)) {
    p <- .qf_flag_param[[f]]
    used <- vapply(eq, function(e) p %in% .qf_params[[e]], logical(1))
    if (x[[p]] == 1L && !all(used)) {
      notes <- c(notes, if (!any(used)) {
        sprintf("`%s` is not a predictor for %s and was ignored.", f,
                if (sex == "female") "women" else "men")
      } else {
        sprintf("`%s` is not in the %s %s-fracture equation, so it affects only the %s risk.",
                f, if (sex == "female") "female" else "male",
                if (used[["major"]]) "hip" else "major osteoporotic",
                if (used[["major"]]) "major osteoporotic fracture" else "hip fracture")
      })
    }
  }

  coded <- lapply(eq, function(e) x[.qf_params[[e]]])
  pct <- vapply(names(eq), function(k) do.call(get(paste0(eq[[k]], "_raw")), coded[[k]]),
                numeric(1))
  names(pct) <- c("major_osteoporotic_fracture", "hip_fracture")
  names(coded) <- names(pct)

  list(model = "QFracture-2012",
       years = x$surv,
       risk_percent = as.list(pct),
       risk = as.list(pct / 100),
       outcomes = .qf_outcomes,
       inputs_coded = coded,
       disclaimer = .qf_disclaimer,
       notes = notes)
}

.qf_flag_param <- list(
  antidepressants = "b_antidepressant", any_cancer = "b_anycancer",
  asthma_copd = "b_asthmacopd", corticosteroids = "b_corticosteroids", cvd = "b_cvd",
  dementia = "b_dementia", endocrine = "b_endocrine", epilepsy = "b_epilepsy2",
  falls = "b_falls", prior_fracture = "b_fracture4", hrt_oestrogen = "b_hrt_oest",
  liver_disease = "b_liver", malabsorption = "b_malabsorption", parkinsons = "b_parkinsons",
  ra_sle = "b_ra_sle", renal_disease = "b_renal", care_home = "b_carehome",
  parental_history = "fh_osteoporosis")

.qf_ethnicity <- c(white_or_not_recorded = 1L, white = 1L, not_recorded = 1L, indian = 2L,
                   pakistani = 3L, bangladeshi = 4L, other_asian = 5L,
                   black_caribbean = 6L, black_african = 7L, chinese = 8L, other = 9L)
.qf_smoking <- c(non = 0L, never = 0L, non_smoker = 0L, ex = 1L, ex_smoker = 1L, light = 2L,
                 moderate = 3L, heavy = 4L)
.qf_alcohol <- c(none = 0L, trivial = 1L, light = 2L, moderate = 3L, heavy = 4L,
                 very_heavy = 5L)

.qf_outcomes <- list(
  major_osteoporotic_fracture = "Hip, wrist (distal radius), shoulder (proximal humerus) or spine (vertebral) fracture",
  hip_fracture = "Hip fracture")

.qf_disclaimer <- paste(
  "The initial version of this file, to be found at http://svn.clinrisk.co.uk/qfracture, faithfully implements QFracture-2012.",
  "We have released this code under the GNU Lesser General Public License to enable others to implement the algorithm faithfully.",
  "However, the nature of the GNU Lesser General Public License is such that we cannot prevent, for example, someone altering the coefficients.",
  "We stress, therefore, that it is the responsibility of the end user to check that the source that they receive produces the same results as the original code posted at http://svn.clinrisk.co.uk/qfracture.",
  "Inaccurate implementations of risk scores can lead to wrong patients being given the wrong treatment.")

.qf_num <- function(x, field, lo, hi) {
  if (is.character(x)) x <- suppressWarnings(as.numeric(x))
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < lo || x > hi) {
    stop("`", field, "` must be a number from ", lo, " to ", hi, ".", call. = FALSE)
  }
  as.numeric(x)
}

.qf_int <- function(x, field, lo, hi) {
  v <- .qf_num(x, field, lo, hi)
  if (v != round(v)) stop("`", field, "` must be a whole number.", call. = FALSE)
  as.integer(v)
}

.qf_key <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(as.character(x))))

.qf_choice <- function(x, field, map) {
  key <- gsub("_", "", .qf_key(x))
  names(map) <- gsub("_", "", names(map))
  if (length(key) != 1L || !key %in% names(map)) {
    stop("Unrecognised `", field, "`: \"", paste(x, collapse = ","), "\".", call. = FALSE)
  }
  map[[key]]
}

.qf_code <- function(x, field, map) {
  codes <- sort(unique(map))
  if (is.numeric(x) || grepl("^\\s*-?[0-9]+(\\.0+)?\\s*$", as.character(x)[1])) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1L || is.na(v) || !v %in% codes) {
      stop("`", field, "` code must be one of ", paste(codes, collapse = ", "), ".", call. = FALSE)
    }
    return(as.integer(v))
  }
  .qf_choice(x, field, map)
}

.qf_yesno <- function(x, field) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(as.integer(x))
  if (is.numeric(x)) {
    if (length(x) != 1L || is.na(x) || !x %in% c(0, 1)) {
      stop("`", field, "` must be yes/no (or 1/0).", call. = FALSE)
    }
    return(as.integer(x))
  }
  .qf_choice(x, field, c(yes = 1L, y = 1L, true = 1L, `1` = 1L, no = 0L, n = 0L,
                         false = 0L, `0` = 0L))
}
