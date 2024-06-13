crimes <- read.csv("data/nyc_month_crimes.csv")

head(crimes)

crimes$Borough <- as.factor(crimes$Borough)


# Fit the null model (a model with no predictors)
null_model <- lm(Shootings ~ 1, data = crimes)

# Fit the full model (a model with all predictors)
full_model <- lm(Shootings ~ ., data = crimes)

# Perform stepwise regression using forward selection
final_model <- step(
    null_model,
    scope = list(lower = null_model, upper = full_model),
    direction = "forward",
    k = log(nrow(crimes))
)
