# Reference outputs from the compiled QFracture-2012 C code (ClinRisk, LGPL-3).
#
# usage: Rscript data-raw/c_reference.R <package dir> <empty build dir>
#
# Copies the pinned C source (data-raw/qfracture-2012-c, from
# github.com/nhsland/clinrisk-modules commit 4e7cb71e) into the build
# directory, adds a .C() wrapper generated from the header prototypes,
# compiles it with R CMD SHLIB and evaluates the four public C functions
# (with their own input validation) on a set of cases. Results are written
# with 17 significant digits to tests/testthat/c_reference_fixtures.csv.

args <- commandArgs(TRUE)
pkg <- normalizePath(args[1])
build <- normalizePath(args[2])
src <- file.path(pkg, "data-raw", "qfracture-2012-c")
file.copy(file.path(src, c("c", "include")), build, recursive = TRUE)

eqs <- c(fracture4_female = "Q74_qfracture4_2012_2_0", fracture4_male = "Q74_qfracture4_2012_2_1",
         nof_female = "Q74_qnof_2012_2_0", nof_male = "Q74_qnof_2012_2_1")

# Parameter lists from the header prototypes.
proto <- lapply(names(eqs), function(fn) {
  h <- paste(readLines(file.path(src, "include", "clinrisk", paste0(eqs[[fn]], ".h"))), collapse = " ")
  m <- regmatches(h, regexec(paste0("double ", fn, "\\(\\s*(.*?)\\)\\s*;"), h))[[1]][2]
  p <- trimws(strsplit(m, ",")[[1]])
  p <- p[!grepl("\\*|errorBufSize", p)]
  data.frame(type = sub(" .*", "", p), name = sub(".* ", "", p))
})
names(proto) <- names(eqs)

wrapper <- c("#include <clinrisk/utils.h>",
             paste0("#include <clinrisk/", eqs, ".h>"))
for (fn in names(eqs)) {
  p <- proto[[fn]]
  ni <- sum(p$type == "int"); nd <- sum(p$type == "double")
  ii <- cumsum(p$type == "int") - 1; di <- cumsum(p$type == "double") - 1
  call <- ifelse(p$type == "int", sprintf("ia[i*%d+%d]", ni, ii), sprintf("da[i*%d+%d]", nd, di))
  wrapper <- c(wrapper,
    sprintf("void w_%s(int *n, int *ia, double *da, double *out, int *err) {", fn),
    "  char buf[2048];",
    "  for (int i = 0; i < *n; i++) {",
    sprintf("    out[i] = %s(%s, &err[i], buf, sizeof(buf));", fn, paste(call, collapse = ", ")),
    "  }", "}")
}
writeLines(wrapper, file.path(build, "wrapper.c"))

old <- setwd(build)
csrc <- c("wrapper.c", file.path("c", list.files("c", "\\.c$")))
status <- system2(file.path(R.home("bin"), "R"), c("CMD", "SHLIB", "-o", "qfc.so", csrc),
                  env = "PKG_CPPFLAGS=-Iinclude")
stopifnot(status == 0)
dyn.load(file.path(build, "qfc.so"))
setwd(old)

run_c <- function(fn, cases) {
  p <- proto[[fn]]
  ia <- as.integer(t(as.matrix(cases[p$name[p$type == "int"]])))
  da <- as.double(t(as.matrix(cases[p$name[p$type == "double"]])))
  r <- .C(paste0("w_", fn), n = nrow(cases), ia = ia, da = da,
          out = double(nrow(cases)), err = integer(nrow(cases)))
  data.frame(score = r$out, c_error = r$err)
}

# Cases: one-at-a-time variation around a base profile, every boolean on
# singly and all together, boundaries, random profiles and invalid inputs.
make_cases <- function(fn) {
  p <- proto[[fn]]$name
  bools <- p[grepl("^b_|^fh_", p)]
  base <- setNames(rep(0, length(p)), p)
  base[c("age", "bmi", "ethrisk", "surv")] <- c(65, 25, 1, 10)
  one <- function(...) { x <- base; v <- c(...); x[names(v)] <- v; x }
  rows <- list(base)
  for (a in c(30, 31, 40, 50, 55, 60, 64, 70, 75, 80, 85, 88, 90, 95, 99, 100)) rows[[length(rows) + 1]] <- one(age = a)
  for (b in c(20, 20.01, 21.5, 22, 24.9, 27.3, 30, 32.75, 35, 38, 39.99, 40)) rows[[length(rows) + 1]] <- one(bmi = b)
  for (e in 2:9) rows[[length(rows) + 1]] <- one(ethrisk = e)
  for (s in 1:4) rows[[length(rows) + 1]] <- one(smoke_cat = s)
  for (a in 1:5) rows[[length(rows) + 1]] <- one(alcohol_cat6 = a)
  for (t in c(1:9, 11:18)) rows[[length(rows) + 1]] <- one(surv = t)
  for (b in bools) rows[[length(rows) + 1]] <- one(setNames(1, b))
  rows[[length(rows) + 1]] <- one(setNames(rep(1, length(bools)), bools))
  rows[[length(rows) + 1]] <- one(c(setNames(rep(1, length(bools)), bools), age = 100, bmi = 20,
                                    smoke_cat = 4, alcohol_cat6 = 5, surv = 18))
  rows[[length(rows) + 1]] <- one(age = 30, bmi = 40, surv = 1)
  set.seed(2012)
  for (i in 1:150) {
    x <- base
    x["age"] <- sample(30:100, 1); x["bmi"] <- round(runif(1, 20, 40), 2)
    x["ethrisk"] <- sample(1:9, 1); x["smoke_cat"] <- sample(0:4, 1)
    x["alcohol_cat6"] <- sample(0:5, 1); x["surv"] <- sample(1:18, 1)
    x[bools] <- rbinom(length(bools), 1, 0.25)
    rows[[length(rows) + 1]] <- x
  }
  invalid <- list(c(age = 29), c(age = 101), c(bmi = 19.999), c(bmi = 40.001),
                  c(ethrisk = 0), c(ethrisk = 10), c(smoke_cat = -1), c(smoke_cat = 5),
                  c(alcohol_cat6 = -1), c(alcohol_cat6 = 6), c(surv = 0), c(surv = 19),
                  setNames(2, bools[1]), setNames(-1, bools[length(bools)]))
  for (v in invalid) rows[[length(rows) + 1]] <- one(v)
  as.data.frame(do.call(rbind, rows))
}

all_params <- unique(unlist(lapply(proto, `[[`, "name")))
out <- do.call(rbind, lapply(names(eqs), function(fn) {
  cases <- make_cases(fn)
  res <- run_c(fn, cases)
  full <- as.data.frame(matrix(NA_real_, nrow(cases), length(all_params),
                               dimnames = list(NULL, all_params)))
  full[names(cases)] <- cases
  data.frame(equation = fn, case = seq_len(nrow(cases)), full,
             score = ifelse(res$c_error == 1, NA, sprintf("%.17g", res$score)),
             c_error = res$c_error)
}))
dest <- file.path(pkg, "tests", "testthat", "c_reference_fixtures.csv")
write.csv(out, dest, row.names = FALSE, na = "")
cat("wrote", nrow(out), "rows;", sum(out$c_error), "rejected by the C validation\n")
print(table(out$equation, out$c_error))
