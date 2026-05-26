setwd('D:/2e master biologie/Thesis')
D=read.csv('D1.csv')
getwd()
str(D)
df <- D |> dplyr::mutate(across(where(is.character), as.factor))




library(glmmTMB)
library(dplyr)
library(ggplot2)

df <- D %>% mutate(obs_id = factor(row_number()))

# Fit model with treatment included as a factor (or numeric if needed)
model_bb <- glmmTMB(
  cbind(alive, dead) ~ treatment * pond_code +
    (1 | clone_code) + (1 | date) + (1 | observer) + (1 | obs_id),
  family = binomial(link = "logit"),
  data = df
)
# Extract BLUPs for clone_code
blups <- ranef(model_bb)$cond$clone_code
blup_values <- blups[,1]

blup_df <- data.frame(
  clone_code = rownames(blups),
  blup = blup_values
)

# Output in PLINK-style format
blup_out1 <- data.frame(
  FID = rownames(blups),
  IID = rownames(blups),
  Phenotype = blup_values
)

write.table(
  blup_out1,
  file = "new.txt",
  quote = FALSE,
  row.names = FALSE,
  col.names = TRUE,
  sep = " "
)

# Check average survival by clone across all treatments
avg_surv <- df %>%
  group_by(clone_code) %>%
  summarise(mean_survival = mean(alive / 10, na.rm = TRUE))

plot_df <- merge(avg_surv, blup_df, by = "clone_code")

# Plot BLUPs vs average survival
ggplot(plot_df, aes(x = mean_survival, y = blup)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average survival",
    y = "BLUP (clone effect)"
  ) +
  theme_minimal()

# If you want BLUPs on probability scale
plot_df$blup_prob <- plogis(plot_df$blup)

ggplot(plot_df, aes(x = mean_survival, y = blup_prob)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average survival",
    y = "BLUP (probability scale)"
  ) +
  theme_minimal()

