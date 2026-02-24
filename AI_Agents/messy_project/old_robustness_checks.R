# Robustness checks — Table B1 (with controls) + Table B2 (full sample)
# Some of these might not be in the final paper
# ask Alvin which ones to keep

library(haven)
library(fixest)

d <- read_dta("1_experiment_clean.dta")

observables <- c("somecollege","has_kids","nonwhite","never_married",
                 "female","religious","repub","soc_conserv","age","is_student")

d$wave2 <- as.integer(d$wave == 2)
d$wave3 <- as.integer(d$wave == 3)
d$date_fe <- as.factor(d$date)

##### TABLE B1: Unregistered with controls #####
unreg <- d[d$before == 0, ]

rb1 <- feols(after ~ yesno + list + .[observables], data = unreg[unreg$wave == 1, ])
rb2 <- feols(after ~ yesno + list + .[observables], data = unreg[unreg$wave == 2, ])
rb3 <- feols(after ~ yesno + list + .[observables], data = unreg[unreg$wave == 3, ])
rb4 <- feols(after ~ yesno + list + wave2 + wave3 + .[observables], data = unreg)
rb5 <- feols(after ~ yesno + list + wave2 + wave3 + .[observables] | date_fe, data = unreg)

cat("\n======= TABLE B1: Unregistered + Controls =======\n")
etable(rb1, rb2, rb3, rb4, rb5)

##### TABLE B2: Full sample with interactions #####
d$prev_reg <- as.integer(d$before == 1)
d$list[is.na(d$list)] <- 0

fb1 <- feols(after ~ yesno * prev_reg + list * prev_reg, data = d[d$wave == 1, ])
fb2 <- feols(after ~ yesno * prev_reg + list * prev_reg, data = d[d$wave == 2, ])
fb3 <- feols(after ~ yesno * prev_reg + list * prev_reg, data = d[d$wave == 3, ])
fb4 <- feols(after ~ yesno * prev_reg + list * prev_reg + wave2 + wave3, data = d)
fb5 <- feols(after ~ yesno * prev_reg + list * prev_reg + wave2 + wave3 | date_fe, data = d)

cat("\n======= TABLE B2: Full Sample =======\n")
etable(fb1, fb2, fb3, fb4, fb5)

# Try winsorized version too? not sure if needed
# also maybe cluster at session level for in-person?
