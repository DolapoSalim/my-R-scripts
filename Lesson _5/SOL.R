euclidean_distance <- function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}


# Example
print(euclidean_distance(c(0, 0), c(3, 4)))



interactive_distance <- function() {
  # Generate a matrix with 25 rows and 2 columns
  data <- matrix(runif(50), nrow = 25, ncol = 2)
  
  # Create a plot
  plot(data, main = "Click on 2 points to measure distance", 
       xlab = "X", ylab = "Y", cex = 2, col = "blue")
  
  # Use locator to select 2 points
  cat("Click on 2 points on the plot\n")
  points_clicked <- locator(n = 2)
  
  # Extract coordinates
  p1 <- c(points_clicked$x[1], points_clicked$y[1])
  p2 <- c(points_clicked$x[2], points_clicked$y[2])
  
  # Mark clicked points on plot
  points(p1[1], p1[2], col = "red", pch = 16, cex = 3)
  points(p2[1], p2[2], col = "red", pch = 16, cex = 3)
  
  # Calculate Euclidean distance
  distance <- sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
  
  # Print the result
  cat("Point 1:", p1, "\n")
  cat("Point 2:", p2, "\n")
  cat("Distance:", distance, "\n")
  
  return(distance)
}

# Run the function
result <- interactive_distance()