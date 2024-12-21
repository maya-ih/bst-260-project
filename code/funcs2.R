# extrapolate all cause death
extrapolate <- function(data, future_dates) {
  lm_outcome <- lm(Outcome ~ as.numeric(date), data = data)
  
  predictions <- tibble(
    Date = future_dates,
    Outcome = predict(lm_outcome, newdata = tibble(date = as.numeric(future_dates)))
  )
  
  predictions <- predictions %>%
    mutate(
      Outcome = pmax(round(Outcome), 0) # Ensure no negative values
    )
  
  return(predictions)
}
