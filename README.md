# qfracturepexa

**QFracture-2012** gives the risk of a major osteoporotic fracture (hip,
wrist, shoulder or spine) and of a hip fracture, over 1 to 18 years (default
10), for people aged 30 to 100.

Hippisley-Cox J, Coupland C. Derivation and validation of updated QFracture
algorithm to predict risk of osteoporotic fracture in primary care in the
United Kingdom: prospective open cohort study. *BMJ.* 2012;344:e3427.
[doi:10.1136/bmj.e3427](https://doi.org/10.1136/bmj.e3427)

> **Disclaimer (ClinRisk Ltd; required with every score).** The initial
> version of this file, to be found at http://svn.clinrisk.co.uk/qfracture,
> faithfully implements QFracture-2012. We have released this code under the
> GNU Lesser General Public License to enable others to implement the
> algorithm faithfully. However, the nature of the GNU Lesser General Public
> License is such that we cannot prevent, for example, someone altering the
> coefficients. We stress, therefore, that it is the responsibility of the
> end user to check that the source that they receive produces the same
> results as the original code posted at http://svn.clinrisk.co.uk/qfracture.
> Inaccurate implementations of risk scores can lead to wrong patients being
> given the wrong treatment.

The service returns this text in the `disclaimer` field of every result.

## Version

This is **QFracture-2012**, the version described in the BMJ 2012 paper
and released by ClinRisk as source code.

- ClinRisk generated the C code on 5 March 2012 from a Stata dataset
  time-stamped 1 March 2012.
- The paper was published on 22 May 2012.
- Other versions of QFracture, such as the one on ClinRisk's website, may
  give different results. This package does not attempt to match them.

## Source

ClinRisk's QFracture-2012 C code (LGPL-3), from the mirror
[nhsland/clinrisk-modules](https://github.com/nhsland/clinrisk-modules),
commit `4e7cb71e`, folder `qFracture/`, a copy of ClinRisk's repository
`svn.clinrisk.co.uk/qfracture`.

- The four equations: `Q74_qfracture4_2012_2_0.c` (women) and `_2_1.c` (men)
  for major osteoporotic fracture, and `Q74_qnof_2012_2_0.c` and `_2_1.c` for
  hip fracture.
- `data-raw/translate_c.py` translates them into R (`R/equations.R`)
  statement by statement, with the numbers as written. The unchanged C
  source is kept in `data-raw/qfracture-2012-c/`.
- Each risk is 100 × (1 − S₀(t)^exp(Σβx)), with the C code's fractional
  polynomial terms for age and BMI.

## Inputs

| Field | Values |
|---|---|
| `sex` | female, male |
| `age` | whole years, 30–100 |
| `bmi` | kg/m², 20–40 (values outside are rejected, as in the C code) |
| `ethnicity` | 1 white or not recorded, 2 Indian, 3 Pakistani, 4 Bangladeshi, 5 other Asian, 6 black Caribbean, 7 black African, 8 Chinese, 9 other (including mixed); codes or labels such as `black_caribbean` |
| `smoking` | 0 non, 1 ex, 2 light (under 10 a day), 3 moderate (10–19), 4 heavy (20 or more) |
| `alcohol` | 0 none, 1 trivial (under 1 unit a day), 2 light (1–2), 3 moderate (3–6), 4 heavy (7–9), 5 very heavy (more than 9) |
| `diabetes` | none, type1, type2 |
| `years` | whole years, 1–18 (default 10) |

The yes/no fields all default to no. The definitions are from the paper's
Box 2:

| Field | Meaning | Used for |
|---|---|---|
| `antidepressants` | any antidepressant prescribed | all |
| `any_cancer` | any cancer | all |
| `asthma_copd` | asthma or COPD | all |
| `corticosteroids` | at least two prescriptions of oral corticosteroids in 6 months | all |
| `cvd` | cardiovascular disease | all |
| `dementia` | dementia | all |
| `epilepsy` | epilepsy diagnosis or anticonvulsant treatment | all |
| `falls` | history of falls | all |
| `prior_fracture` | prior hip, vertebral, shoulder or wrist fracture | all |
| `liver_disease` | chronic liver disease | all |
| `parkinsons` | Parkinson's disease | all |
| `ra_sle` | rheumatoid arthritis or systemic lupus erythematosus | all |
| `renal_disease` | chronic renal disease | all |
| `endocrine` | thyrotoxicosis, hyperparathyroidism or Cushing's syndrome | women |
| `hrt_oestrogen` | hormone replacement therapy with unopposed oestrogen | women |
| `care_home` | lives in a care or nursing home | men |
| `malabsorption` | Crohn's disease, ulcerative colitis, coeliac disease, steatorrhoea or blind loop syndrome | major fracture only |
| `parental_history` | parental history of osteoporosis or hip fracture | men; women's major fracture only |

A "yes" for a predictor that an equation does not use is ignored, and the
result includes a note.

## Outputs

- **`risk_percent`:** major osteoporotic and hip fracture risks, in percent,
  unrounded.
- **`risk`:** the same risks as probabilities.
- **`inputs_coded`:** the values passed to each equation.
- **`disclaimer`:** ClinRisk's text above.
- **`notes`**.

## Validation

- **The compiled ClinRisk C code.** `data-raw/c_reference.R` compiles the
  pinned C source unchanged and runs it on 991 cases across the four
  equations (`tests/testthat/c_reference_fixtures.csv`). The cases include
  every category, ages 30–100, BMI 20–40, horizons 1–18 years, each
  condition alone and together, and random profiles.
  - All 935 valid cases agree to a relative difference below 10⁻¹².
  - The C code rejects 56 out-of-range cases, and this package rejects the
    same cases.
- **The paper's clinical examples** (alcohol is not stated; "none" used).
  12 of the 14 printed risks agree at whole-percent precision. The other
  two are hip-fracture risks that compute to 5.498% and 5.493% but are
  printed as 6%.
- **The QFracture website is not used.** Its terms do not allow it to be
  used for developing or testing software.

## Licence

LGPL-3 or later, as the ClinRisk code, with ClinRisk's additional term
that the disclaimer above be displayed alongside any risk score (`LICENSE`,
`inst/COPYING.LESSER`, `inst/COPYING`). Not a substitute for clinical
judgement.
