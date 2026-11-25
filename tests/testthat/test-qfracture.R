fx <- utils::read.csv(test_path("c_reference_fixtures.csv"), colClasses = c(score = "character"))
fx$score <- as.numeric(fx$score)
params <- qfracturepexa:::.qf_params

row_args <- function(r) {
  p <- params[[r$equation]]
  x <- as.list(r[p])
  x[p != "bmi"] <- lapply(x[p != "bmi"], as.integer)
  x
}

public_args <- function(r) {
  x <- row_args(r)
  out <- list(sex = sub(".*_", "", r$equation), diabetes = "none")
  if (x$b_type1 == 1L) out$diabetes <- "type1"
  if (x$b_type2 == 1L) out$diabetes <- "type2"
  if (!x$b_type1 %in% 0:1 || !x$b_type2 %in% 0:1) out$diabetes <- "invalid"
  x$b_type1 <- x$b_type2 <- NULL
  names(x) <- ifelse(names(x) %in% names(qfracturepexa:::.qf_alias),
                     qfracturepexa:::.qf_alias[names(x)], names(x))
  c(out, x)
}

test_that("the translated equations reproduce the compiled ClinRisk C code", {
  ok <- fx[fx$c_error == 0, ]
  expect_gt(nrow(ok), 900)
  r_score <- vapply(seq_len(nrow(ok)), function(i) {
    do.call(get(paste0(ok$equation[i], "_raw"), asNamespace("qfracturepexa")),
            row_args(ok[i, ]))
  }, numeric(1))
  expect_lt(max(abs(r_score - ok$score) / pmax(ok$score, 1e-300)), 1e-12)
  expect_true(all(table(ok$equation) > 200))
})

test_that("qfracture_risk() matches the C code and rejects what the C code rejects", {
  ok <- fx[fx$c_error == 0 & !(fx$b_type1 %in% 1 & fx$b_type2 %in% 1), ]
  for (i in seq_len(nrow(ok))) {
    res <- do.call(qfracture_risk, public_args(ok[i, ]))
    out <- if (grepl("^fracture4", ok$equation[i])) "major_osteoporotic_fracture" else "hip_fracture"
    expect_equal(res$risk_percent[[out]], ok$score[i], tolerance = 1e-12)
  }
  bad <- fx[fx$c_error == 1, ]
  expect_equal(nrow(bad), 56)
  for (i in seq_len(nrow(bad))) {
    expect_error(do.call(qfracture_risk, public_args(bad[i, ])))
  }
})

test_that("the paper's clinical examples agree at whole-percent precision", {
  # Hippisley-Cox & Coupland, BMJ 2012, "Clinical examples". Alcohol is not
  # stated in the paper; "none" is used. Each row: inputs, printed major
  # osteoporotic and hip fracture risks (%).
  ex1 <- list(sex = "female", age = 64, bmi = 22, ethnicity = 1, smoking = "light",
              alcohol = "none", diabetes = "type1", epilepsy = "yes")
  ex2 <- list(sex = "male", age = 70, bmi = 22, ethnicity = 1, smoking = "heavy",
              alcohol = "none", any_cancer = "yes", falls = "yes")
  ex3 <- list(sex = "male", age = 88, bmi = 22, ethnicity = 1, smoking = "ex",
              alcohol = "none", renal_disease = "yes", antidepressants = "yes",
              corticosteroids = "yes", years = 5)
  cases <- list(
    list(ex1, c(15, 13)),
    list(modifyList(ex1, list(ethnicity = "pakistani")), c(7, 6)),
    list(ex2, c(8, 6)),
    list(modifyList(ex2, list(antidepressants = "yes")), c(12, 9)),
    list(modifyList(ex2, list(antidepressants = "yes", care_home = "yes")), c(19, 18)),
    list(ex3, c(12, 9)),
    list(modifyList(ex3, list(antidepressants = "no")), c(7, 6)))
  got <- t(vapply(cases, function(k) unlist(do.call(qfracture_risk, k[[1]])$risk_percent),
                  numeric(2)))
  printed <- t(vapply(cases, `[[`, numeric(2), 2))
  # Every value is within rounding distance of the printed figure ...
  expect_true(all(abs(got - printed) <= 0.51))
  # ... and 12 of the 14 round to it. The other two are the hip-fracture
  # risks in examples 2 and 3b: 5.498% and 5.493%, printed as 6%.
  expect_equal(sum(round(got) == printed), 12)
  expect_equal(round(got[c(3, 7), 2], 3), c(5.498, 5.493))
})

test_that("every result carries ClinRisk's disclaimer and the version label", {
  r <- model_run(get_default_input())
  expect_equal(r$model, "QFracture-2012")
  expect_match(r$disclaimer, "faithfully implements QFracture-2012", fixed = TRUE)
  expect_match(r$disclaimer, "wrong patients being given the wrong treatment.", fixed = TRUE)
})

test_that("predictors outside an equation are ignored with a note", {
  base <- list(age = 70, bmi = 25, ethnicity = 1, smoking = 0, alcohol = 0)
  m0 <- do.call(qfracture_risk, c(base, sex = "male"))
  m1 <- do.call(qfracture_risk, c(base, sex = "male", hrt_oestrogen = "yes", endocrine = "yes"))
  expect_equal(m1$risk, m0$risk)
  expect_length(m1$notes, 2)
  f1 <- do.call(qfracture_risk, c(base, sex = "female", parental_history = "yes"))
  f0 <- do.call(qfracture_risk, c(base, sex = "female"))
  expect_equal(f1$risk$hip_fracture, f0$risk$hip_fracture)
  expect_gt(f1$risk$major_osteoporotic_fracture, f0$risk$major_osteoporotic_fracture)
  expect_match(f1$notes, "affects only the major osteoporotic fracture risk")
})

test_that("inputs are validated", {
  base <- list(sex = "female", age = 64, bmi = 22, ethnicity = 1, smoking = 0, alcohol = 0)
  expect_error(do.call(qfracture_risk, modifyList(base, list(age = 64.5))), "whole number")
  expect_error(do.call(qfracture_risk, modifyList(base, list(bmi = 19.9))), "20 to 40")
  expect_error(do.call(qfracture_risk, modifyList(base, list(ethnicity = "x"))), "ethnicity")
  expect_error(do.call(qfracture_risk, modifyList(base, list(years = 19))), "1 to 18")
  expect_equal(do.call(qfracture_risk, modifyList(base, list(ethnicity = "Black Caribbean")))$inputs_coded$hip_fracture$ethrisk, 6L)
})

test_that("gateway returns full precision", {
  g <- jsonlite::fromJSON(gateway(model_input = get_default_input()))
  r <- model_run(get_default_input())
  expect_equal(g$risk_percent$hip_fracture, r$risk_percent$hip_fracture, tolerance = 1e-14)
  expect_equal(g$model, "QFracture-2012")
  expect_true(nzchar(g$disclaimer))
  expect_error(gateway(), "No input")
})
