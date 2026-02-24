###############################################
# Main regressions — Table 2
# organ donor registration by treatment
# Initially Unregistered subsample
# FINAL VERSION (use this one, not analysis.R)
###############################################

library(haven)
library(fixest)

setwd("C:/Users/judd/Desktop/replication")

d <- read_dta("1_experiment_clean.dta")

# keep unregistered
d <- d[d$before == 0, ]

# wave dummies
d$wave2 <- as.integer(d$wave == 2)
d$wave3 <- as.integer(d$wave == 3)

# date fixed effects
d$date_fe <- as.factor(d$date)

### Column 1: Wave 1
m1 <- feols(after ~ yesno + list, data = d[d$wave == 1, ])
### Column 2: Wave 2
m2 <- feols(after ~ yesno + list, data = d[d$wave == 2, ])
### Column 3: Wave 3
m3 <- feols(after ~ yesno + list, data = d[d$wave == 3, ])
### Column 4: Pooled
m4 <- feols(after ~ yesno + list + wave2 + wave3, data = d)
### Column 5: Pooled + date FE
m5 <- feols(after ~ yesno + list + wave2 + wave3 | date_fe, data = d)

etable(m1, m2, m3, m4, m5, se = "hetero")

# TODO: also need to do the version with controls (Table B1)
# ask Alvin about clustering

#sink("output_table2.txt")
#etable(m1, m2, m3, m4, m5, se = "hetero")
#sink()
