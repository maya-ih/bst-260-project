# extrapolate COVID cases and death
extrapolate <- function(data, future_weeks) {
  lm_cases <- lm(new_cases ~ as.numeric(end_date), data = data)
  lm_deaths <- lm(new_deaths ~ as.numeric(end_date), data = data)
  
  predictions <- tibble(
    end_date = future_weeks,
    new_cases = predict(lm_cases, newdata = tibble(end_date = as.numeric(future_weeks))),
    new_deaths = predict(lm_deaths, newdata = tibble(end_date = as.numeric(future_weeks)))
  )
  
  predictions <- predictions %>%
    mutate(
      new_cases = pmax(round(new_cases), 0),
      new_deaths = pmax(round(new_deaths), 0)
    )
  
  return(predictions)
}