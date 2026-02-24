# Figure 8 — Next of Kin: Should NOK donate?
# Bar chart by frame (yes/no vs opt-in) and deceased registration status

library(haven)
library(ggplot2)
library(dplyr)

nok <- read_dta("2_nextofkin_clean.dta")

# make nicer labels
fig8 <- nok %>%
  mutate(
    frame = ifelse(yesno == 1, "Yes/No Frame", "Opt-In Frame"),
    status = ifelse(registered_hyp == 1, "Deceased was Registered",
                    "Deceased was Unregistered")
  ) %>%
  group_by(frame, status) %>%
  summarise(
    prop = mean(first, na.rm = TRUE),
    n = sum(!is.na(first)),
    se = sqrt(prop * (1-prop) / n),
    .groups = "drop"
  )

p8 <- ggplot(fig8, aes(x = status, y = prop, fill = frame)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = prop - 1.96*se, ymax = prop + 1.96*se),
                position = position_dodge(0.7), width = 0.2) +
  geom_text(aes(label = sprintf("n=%d", n)),
            position = position_dodge(0.7), vjust = -0.5, size = 3) +
  labs(title = "Figure 8: NOK Should Donate (First Question)",
       y = "Proportion Saying 'Should Donate'",
       x = "", fill = "") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1))

print(p8)
ggsave("fig8_nok.png", p8, width=8, height=5)
