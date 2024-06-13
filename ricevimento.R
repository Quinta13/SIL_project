# Ricevimento da chiedere

# 1. Warning dei constraint
# 2. Okay non usare Shootings mese prima come predittore
# 3. Conferma effetto collinearità coefficienti negativi
# 4. Se va bene togliere larceny per il Simpson Paradox
# 5. LASSO no CI e SE
# 6. QuasiPois no AIC
# 7. PCA interpertazione per predittori

# Link
# quasilikelihood # https://stats.stackexchange.com/questions/418832/why-the-absence-of-probability-distribution-for-using-quasi-likelihood
# gvif threshold  # https://stats.stackexchange.com/questions/559924/which-threshold-should-i-use-for-gvif1-2⋅df-variance-inflation-factor
# lasso ci        # https://stats.stackexchange.com/questions/418832/why-the-absence-of-probability-distribution-for-using-quasi-likelihood
# 

{
source("src/init.R")
# Load data
crimes <- read.csv(PATHS$NycMonthCrimes)

crimes <- prepare_crimes(crimes_df=crimes)

crime_names <- colnames(crimes)[4:17]


# Split Train and Test
split <- train_test_split_by_year(df_ = crimes, year_test = YEAR_TEST)
crimes      <- split$train
crimes_test <- split$test
rm(split)

# Take apart Queens from the borough and recompute the total

split_out <- split_statenisland(crimes=crimes, crimes_test=crimes_test)

crimes         <- split_out$crimes
crimes_test    <- split_out$crimes_test
si_crimes      <- split_out$si_crimes
si_crimes_test <- split_out$si_crimes_test
}

# contrasts(crimes$MonthName) <- contr.sum(12)

fit <- lm(Shootings ~ MonthName, data = crimes)

summary(crimes)

table(crimes$MonthName)

summary(fit)

preds <- predict(fit, data.frame(MonthName = "Jan"))
preds
