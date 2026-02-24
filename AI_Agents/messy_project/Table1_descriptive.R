#-------------------------------------------------
# Summary stats for Table 1
# based on Kessler & Roth (2025)
# ashley wrote this, modified by judd 09/2024
#-------------------------------------------------

library(haven)
library(dplyr)

# load data
dat <- read_dta("C:/Users/judd/Desktop/replication/1_experiment_clean.dta")
dat$in_person <- as.integer(dat$wave %in% c(1, 2))

observables <- c("somecollege","has_kids","nonwhite","never_married",
                 "female","religious","repub","soc_conserv","age","is_student","before")

# Loop through observable vars and print means
for (v in observables) {
  for (ip in 0:1) {
    for (yn in 0:1) {
      sub <- dat[dat$in_person == ip & dat$yesno == yn, ]
      m <- mean(sub[[v]], na.rm=TRUE)
      s <- sd(sub[[v]], na.rm=TRUE)
      n <- nrow(sub)
      cat(sprintf("%-20s  in_person=%d  yesno=%d  mean=%.4f  sd=%.4f  N=%d\n",
                  v, ip, yn, m, s, n))
    }
  }
}

# Joint significance tests
# waves 1-3
mod_all <- lm(yesno ~ somecollege + has_kids + nonwhite + never_married +
                female + religious + repub + soc_conserv + age + is_student + before,
              data = dat)
cat("\n--- Joint F-test, Waves 1-3 ---\n")
summary(mod_all)

# in-person only
mod_ip <- lm(yesno ~ somecollege + has_kids + nonwhite + never_married +
               female + religious + repub + soc_conserv + age + is_student + before,
             data = dat[dat$in_person == 1, ])
cat("\n--- Joint F-test, In-Person ---\n")
summary(mod_ip)

# online only
mod_online <- lm(yesno ~ somecollege + has_kids + nonwhite + never_married +
                   female + religious + repub + soc_conserv + age + is_student + before,
                 data = dat[dat$wave == 3, ])
cat("\n--- Joint F-test, Online ---\n")
summary(mod_online)
