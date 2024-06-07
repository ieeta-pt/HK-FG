# Load necessary library
library(dplyr)
library(ggplot2)

# Initialize the result_matrix to store total counts
result_matrix <- data.frame(p = numeric(), q = numeric(), Prob = integer(), stringsAsFactors = FALSE)

# Iterate through different files
for (i in 1:10) {
  ################################################## Change to the pretended file name
  filename <- sprintf("Orders/Improved_try%d_N_10000_ar1_0.6.txt", i)
  # Load data from the file
  data <- scan(filename, quiet = TRUE)
  
  # Create matrix from data
  matrix <- matrix(data, ncol = 2, byrow = TRUE)
  
  # Convert matrix to data frame
  df <- as.data.frame(matrix)
  colnames(df) <- c("p", "q")
  
  # Count unique pairs
  counts <- df %>%
    group_by(p, q) %>%
    summarise(Prob = n(), .groups = 'drop')
  
  # Merge the counts into result_matrix
  result_matrix <- result_matrix %>%
    full_join(counts, by = c("q", "p")) %>%
    mutate(Prob = coalesce(Prob.x, 0) + coalesce(Prob.y, 0)) %>%
    select(p, q, Prob)
}

# Display the result_matrix
print("Total Counts of Unique Orders:")
print(result_matrix)


####################################################################

data <- result_matrix
data$Prob <- data$Prob / 10000
data$Prob <- round(data$Prob, digits = 4)

# Plot
ggplot(data, aes(x = p, y = q, fill = Prob, label = Prob)) +
  geom_tile(color = "white", width = 1, height = 1) +  # Set width and height to 1
  geom_text(color = "black", size = 3) +
  scale_fill_gradient(low = "gray", high = "red", limits = c(0, 1)) +  # Set limits for the color scale
  labs(x = "p", y = "q", fill = "Probability") +
  ggtitle("ARMA(p,q)") +  # Change to the pretended title
  theme_minimal() +
  coord_fixed() +  # Set aspect ratio to be equal
  expand_limits(x = c(0, 6), y = c(0, 6)) +  # Set limits for x and y axes
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +  # Set breaks for x-axis
  scale_y_continuous(breaks = seq(0, 6, by = 1))    # Set breaks for y-axis