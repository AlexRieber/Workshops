# Next-of-Kin analysis (Table D1)
# How framing affects NOK donation decisions

library(haven)
library(dplyr)
library(marginaleffects)

nok <- read_dta("2_nextofkin_clean.dta")

# Create indicators
nok$unregistered_hyp <- 1 - nok$registered_hyp

#--- Model 1: first choice by frame and status (LPM) ---
m1 <- lm(first ~ yesno * unregistered_hyp, data = nok)
summary(m1)

#--- Model 2: probit ---
m2 <- glm(first ~ yesno * unregistered_hyp,
           data = nok, family = binomial(link = "probit"))
summary(m2)
# marginal effects
avg_slopes(m2)

#--- Model 3: confidence index (first question only) ---
# Build the index: +confidence if donating, -confidence if not
# confidence runs 1 (very confident) to 4 (not confident)
# so we invert: 5 - conf = {1->4, 2->3, 3->2, 4->1}
# then subtract 0.5 to center, and negate if choosing NOT to donate
nok$conf_first <- NA
for (i in 1:nrow(nok)) {
  if (nok$order[i] <= 25) {
    nok$conf_first[i] <- nok$yesno_reg_conf[i]
  } else if (nok$order[i] <= 50) {
    nok$conf_first[i] <- nok$yesno_unreg_conf[i]
  } else if (nok$order[i] <= 75) {
    nok$conf_first[i] <- nok$optin_reg_conf[i]
  } else {
    nok$conf_first[i] <- nok$optin_unreg_conf[i]
  }
}

nok$index_first <- (5 - nok$conf_first) - 0.5
nok$index_first[nok$first == 0] <- -1 * nok$index_first[nok$first == 0]

m3 <- lm(index_first ~ yesno * unregistered_hyp, data = nok)
summary(m3)

cat("\n\n=== RESULTS ===\n")
cat("Model 1 (LPM, first decision):\n")
print(coef(m1))
cat("\nModel 2 (Probit, first decision):\n")
print(coef(m2))
cat("\nModel 3 (Confidence index):\n")
print(coef(m3))
