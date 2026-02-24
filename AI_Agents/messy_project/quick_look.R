# quick look at the data — DELETE THIS LATER

library(haven)
d <- read_dta("1_experiment_clean.dta")

# how many per wave?
table(d$wave)

# registration rates
tapply(d$after, d$yesno, mean)
tapply(d$after, list(d$yesno, d$before), mean)

# what's in the DMV data?
dmv <- read_dta("3_dmv_quarterly_clean.dta")
str(dmv)
unique(dmv$State)
range(dmv$Year)
hist(dmv$PercentDonor)

# NOK data
nok <- read_dta("2_nextofkin_clean.dta")
dim(nok)
table(nok$yesno)
mean(nok$first, na.rm=T)

# weird: some people have NA for "first"
sum(is.na(nok$first))
nok[is.na(nok$first), c("track","yesno","registered_hyp")]

cat("looks ok i think\n")
