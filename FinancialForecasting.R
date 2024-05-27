library(readxl)
library(neuralnet)
library(Metrics)

ExchangeUSD <- read_excel("ExchangeUSD.xlsx")
head(ExchangeUSD)

# Taking only rates column
exchange_rates <- ExchangeUSD$'USD/EUR'
head(exchange_rates)
length(exchange_rates)

max_rate <- max(exchange_rates)
min_rate <- min(exchange_rates)

# Function used to normalize
normalize <- function(x, min_val, max_val) {
  return((x - min_val) / (max_val - min_val))
}

# Function used to de-normalize
denormalize <- function(x, min_val, max_val) { 
  return( x*(max_val - min_val) + min_val )
}

calculate_r_squared <- function(actual_values, predicted_values) {
  mean_actual <- mean(actual_values)
  
  TSS <- sum((actual_values - mean_actual)^2)
  
  # Calculate residual sum of squares (RSS)
  RSS <- sum((actual_values - predicted_values)^2)
  
  R_squared <- 1 - (RSS / TSS)
  
  return(R_squared)
}

# Function to draw scatterplots
draw_scatterplot <- function(predicted, actual,model_name) {
  plot(predicted, actual, 
       xlab = "Predicted Values", ylab = "Actual Values",
       main = model_name)
  abline(0, 1, col = "red") 
}

# Function to create IO matrix
create_io <- function(data, max_time_window) {
  io_matrix_list <- list()
  
  for (i in 1:max_time_window) {
    inputs <- matrix(NA, nrow = length(data)- i, ncol = i)
    for (j in 1:(length(data)- i)) {
      inputs[j,] <- data[j:(j + i - 1)]
    }
    
    outputs <- data[(i + 1) : (length(data))]
    
    input_df <- as.data.frame(inputs)
    colnames(input_df) <- paste0("input_", 1:i)
    
    output_df <- as.data.frame(outputs)
    
    io_matrix_df <- cbind(input_df, output_df)
    
    io_matrix_list[[i]] <- io_matrix_df 
  }
  return(io_matrix_list)
}

io_list <- create_io(exchange_rates, 4)
head(io_list[[4]])

# Function to train NN models
create_mlp_model <- function(hidden_layers, linear_output, activation, learning_rate, data) {
  model <- neuralnet(outputs ~ ., 
                     data = data, 
                     hidden = hidden_layers,
                     linear.output = linear_output,
                     act.fct = activation,
                     learningrate = learning_rate)
  return(model)
}


mlp_models <- list(
  mlp1 = list(data = io_list[[1]], hidden = c(5), linear.output = TRUE, activation = "logistic", learningrate = 0.01),
  mlp2 = list(data = io_list[[1]], hidden = c(10), linear.output = FALSE, activation = "tanh", learningrate = 0.0001),
  mlp3 = list(data = io_list[[1]], hidden = c(15), linear.output = TRUE, activation = "logistic", learningrate = 0.01),
  
  mlp4 = list(data = io_list[[2]], hidden = c(5), linear.output = TRUE, activation = "tanh", learningrate = 0.0001),
  mlp5 = list(data = io_list[[2]], hidden = c(10), linear.output = FALSE, activation = "logistic", learningrate = 0.001),
  mlp6 = list(data = io_list[[2]], hidden = c(20, 10), linear.output = TRUE, activation = "tanh", learningrate = 0.001),
  mlp7 = list(data = io_list[[2]], hidden = c(10, 5), linear.output = FALSE, activation = "logistic", learningrate = 0.001),
  
  mlp8 = list(data = io_list[[3]], hidden = c(5), linear.output = FALSE, activation = "logistic", learningrate = 0.001),
  mlp9 = list(data = io_list[[3]], hidden = c(5, 8), linear.output = TRUE, activation = "tanh", learningrate = 0.0001),
  mlp10 = list(data = io_list[[3]], hidden = c(10, 5), linear.output = FALSE, activation = "logistic", learningrate = 0.0001),
  mlp11 = list(data = io_list[[3]], hidden = c(20, 10), linear.output = TRUE, activation = "tanh", learningrate = 0.01),
  
  mlp12 = list(data = io_list[[4]], hidden = c(10), linear.output = FALSE, activation = "logistic", learningrate = 0.0001),
  mlp13 = list(data = io_list[[4]], hidden = c(5, 5), linear.output = TRUE, activation = "tanh", learningrate = 0.001),
  mlp14 = list(data = io_list[[4]], hidden = c(20, 10), linear.output = FALSE, activation = "logistic", learningrate = 0.00001),
  mlp15 = list(data = io_list[[4]], hidden = c(10, 15), linear.output = TRUE, activation = "tanh", learningrate = 0.01)
)

model_results <- list()


for (i in names(mlp_models)) {
  config <- mlp_models[[i]]
  df <- config$data
  train_df <- df[1:400,]
  test_df <- df[401:nrow(df),]
  
  normalized_train_df <- as.data.frame(lapply(train_df, normalize, min_val = min_rate, max_val = max_rate))
  
  model <- create_mlp_model(config$hidden, config$linear.output, config$activation, config$learningrate, normalized_train_df)
  plot(model)
  
  test_inputs <- subset(test_df, select = -outputs)
  normalized_test_inputs <- as.data.frame(lapply(test_inputs, normalize, min_val = min_rate, max_val = max_rate))
  
  predicted_values <- compute(model, normalized_test_inputs)
  denormalized_predicted_values <- denormalize(predicted_values$net.result, min_rate, max_rate)
  
  actual_outputs <- test_df$outputs
  
  R_Squared <- calculate_r_squared(actual_outputs, denormalized_predicted_values)
  RMSE <- sqrt(mean((denormalized_predicted_values - actual_outputs)^2))
  MAE <- mean(abs(denormalized_predicted_values - actual_outputs))
  MAPE <- mean(abs((denormalized_predicted_values - actual_outputs) / actual_outputs)) * 100
  sMAPE <- 2 * mean(abs(denormalized_predicted_values - actual_outputs) / (abs(denormalized_predicted_values) + abs(actual_outputs))) * 100
  
  model_results[[i]] <- c(rsquared = R_Squared, rmse = RMSE, mae = MAE, mape = MAPE, smape = sMAPE)
  
  #if (i == "mlp4" || i == "mlp11") {
  #  draw_scatterplot(denormalized_predicted_values, actual_outputs, model_name = i)
  #} 
}


model_results










