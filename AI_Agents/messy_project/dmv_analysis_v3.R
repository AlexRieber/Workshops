# Difference-in-Differences analysis of DMV data — TABLE 3
# State-level organ donor registration rates
# v3: fixed the quarter coding issue from v2

library(haven)
library(fixest)
library(dplyr)

#setwd("C:/Users/judd/Desktop/replication")

dmv <- read_dta("3_dmv_quarterly_clean.dta")
dmv <- dmv %>% filter(!is.na(PercentDonor))

# Create year-quarter identifier
dmv$yq <- (dmv$Year - 2010) * 4 + dmv$Quarter
dmv$state_num <- as.integer(factor(dmv$State))

###############################################
# California — reform in Q2 2011
###############################################
ca <- dmv %>%
  filter(Year >= 2010 & Year <= 2012) %>%
  mutate(treat = as.integer(State == "California"),
         post  = as.integer(yq > 6),  # after Q2 2011
         posttreat = treat * post)

# col 1: basic DiD
m_ca1 <- feols(PercentDonor ~ treat + post + posttreat,
               data = ca, vcov = ~State)
# col 2: state + time FE
m_ca2 <- feols(PercentDonor ~ posttreat | state_num + yq,
               data = ca, vcov = ~State)

###############################################
# New York — reform in Q3 2013
###############################################
ny <- dmv %>%
  filter(yq >= 10 & yq <= 22) %>%
  mutate(treat = as.integer(State == "New York"),
         post  = as.integer(yq > 15),
         posttreat = treat * post)

m_ny1 <- feols(PercentDonor ~ treat + post + posttreat,
               data = ny, vcov = ~State)
m_ny2 <- feols(PercentDonor ~ posttreat | state_num + yq,
               data = ny, vcov = ~State)

###############################################
# Hawaii — reform in Q3 2014
###############################################
hi <- dmv %>%
  filter(yq >= 13 & yq <= 23) %>%
  mutate(treat = as.integer(State == "Hawaii"),
         post  = as.integer(yq > 18),
         posttreat = treat * post)

m_hi1 <- feols(PercentDonor ~ treat + post + posttreat,
               data = hi, vcov = ~State)
m_hi2 <- feols(PercentDonor ~ posttreat | state_num + yq,
               data = hi, vcov = ~State)

# Print all results
etable(m_ca1, m_ca2, m_ny1, m_ny2, m_hi1, m_hi2, se = "cluster")

## TODO: stacked DiD (column 7) — need to figure out the cohort dummies
## also need event study (Figure 7) — see figure_eventstudy.R... wait where is that file
