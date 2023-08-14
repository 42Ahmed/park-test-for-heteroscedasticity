
park_test <- function(x, y){
  
  # Combine the independent variables into a matrix
  x_matrix <- cbind(1, x)
  
  # Perform a linear regression of y on x and store the residuals
  model <- lm(y ~ x_matrix)
  residuals <- model$residuals
  
  # Square the residuals and take their logarithm
  squared_log_resid <- log(residuals^2)
  
  # Take the logarithm of the independent variables
  log_x <- log(x_matrix)
  
  # Perform a linear regression of squared_log_resid on log_x
  park_model <- lm(squared_log_resid ~ log_x)
  
  # Output the results of the Park test
  summary(park_model)
}

# Define two vectors of data
x1 <- c(234662,53510,75168,34645,127639,96162,155801,
        143472,34004,81317,73258,54742,72090,52443)

y <- c(2716,816,2277,2294,34839,1760,1375,
       8531,4955,18724,15204,2424,15005,4374)



# Call the park_test function
park_test(x1, y)