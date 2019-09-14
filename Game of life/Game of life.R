# Exercise 1

library(caTools)
# Request to the user the size of the lateral of the matrix, the probability of find a living cell at the beginning, the iterations
# desired and the delay for the creation of the gif

sides <- 0
while (sides <= 1) {
  sides <- as.integer(readline(prompt = "Insert the size of the grid. It must be greater than 1: "))
}
p <- -1
while (p < 0 | p > 1) {
  p <- as.double(readline(prompt = "Insert the probability of finding a living cell. It must be between 0 and 1: "))
}

iterations <- 0
while (iterations <= 0) {
  iterations  <- as.integer(readline(prompt = "Insert the number of iterations that you desire to run. It has to be at least 1: "))
}

delay <- -1
while (delay <= 0) {
 delay  <- as.integer(readline(prompt = "Insert the desired delay for the gif. It is strongly recommended to be between 5 and 20: "))
}

# Function created to integrate the results and provide the different results

function_GoL <- function(sides, p, iterations) {
  # The matrix with the data of the sides del grid is created
  matrix_data <- matrix(nrow = sides, ncol = sides)
  # We introduce the n first living cells randomly
  matrix_data[,] <- rbinom(sides**2, 1, p)
  # We store the matrix for the posterior calculi
  matrices_list <- array(0, dim=c(sides, sides, iterations+1))
  matrices_list[,,1] <- matrix_data
  counter <- 0
  copy_matrix <- matrix_data
  living_cells <- 0
  # While the counter is less or equal than the number of desired iterations:
  while (counter < iterations){
    for (i in c(1:sides)){
      for (j in c(1:sides)){
        living_cells <- living_cells + copy_matrix[safe_index(i-1,sides),safe_index(j-1,sides)]
        living_cells <- living_cells + copy_matrix[safe_index(i-1,sides),j]
        living_cells <- living_cells + copy_matrix[safe_index(i-1,sides),safe_index(j+1,sides)]
        living_cells <- living_cells + copy_matrix[i,safe_index(j-1,sides)]
        living_cells <- living_cells + copy_matrix[i,safe_index(j+1,sides)]
        living_cells <- living_cells + copy_matrix[safe_index(i+1,sides),safe_index(j-1,sides)]
        living_cells <- living_cells + copy_matrix[safe_index(i+1,sides),j]
        living_cells <- living_cells + copy_matrix[safe_index(i+1,sides),safe_index(j+1,sides)]
        matrix_data[i,j] <- life_dead(copy_matrix[i,j], living_cells)
        living_cells <- 0
        }
    }
    counter <- counter + 1
    matrices_list[,,counter+1] <- matrix_data
    copy_matrix <- matrix_data
  }
  return(matrices_list)
}
# Function to calculate safe_index and not to have lecture problems. A toroid world is created to solution the problem with
# the borders

safe_index <- function(j, sides){
  if (j < 1){
    return(sides)
  }
  else if (j > sides){
    return(1)
  }
  else {
    return(j)
  }
}

# Function to evaluate if a cell lives or dies
life_dead <- function(cell, living_cells){
  value <- cell
  if (cell == 1){
    if(living_cells < 6){
      value <- 0
    }
    else if(living_cells > 9){
      value <- 0
    }
  }
  else {
    if(living_cells == 9){
      value <- 1
    }
  }
  return(value)
}
# Function to create gifs
create_gif <- function(matrices_list, filename, delay){
  return(write.gif(matrices_list, filename=filename, delay=delay, col = "rainbow", scale = "smart"))
}

#Exercise 2
living_cells <- c()
for (i in seq(0.1, 0.4, by=0.1)){
  filename <- sprintf("Juego de la vida%1.1f.gif", i)
  create_gif(function_GoL(30,i,1000), filename, 50)
  living_cells <-c()
  matrices <- replicate(100, function_GoL(30,i,1000))
  for (k in c(1:1000)){
    living_cells<- c(living_cells, sum(matrices[,,k,])/100)
  }
  print(living_cells)
  plot(living_cells, type="l", main = sprintf("Laving cells as a function of the iterations for a given probability of %1.2f", i))
  living_cells <-c()
  a <- c()
}

# Exercise 3
# To do it, a modification of the main function is done in order to obtain the iteration number 50 of living cells

living_cells <- c()
probabilities <- seq(0,1,by=0.01)
for(k in probabilities) {
  living_cells<- c(living_cells, mean(replicate(100,sum(function_GoL(100,k,50)))))
}
plot(living_cells, xlab='Probability', ylab="Mean of living cells", type="l", xaxt = "n")
axis(1, at=c(1:101), labels = probabilities)
dataset <- as.data.frame(probabilities)
dataset[2] <- living_cells
print(dataset)

# Ejercicio 4
for(sides in c(10, 25, 50)) {
  matrices <- replicate(100,function_GoL(sides,0.3,500))
  for(k in c(1:500)){
    living_cells<-c(living_cells, sum(matrices[,,k,]/500)) 
  }
  plot(living_cells, ylab="Mean of living cells", type="l")
  living_cells <- c()
}

for(p in seq(.1, .4, by = .1)){
  matrices <- replicate(100,function_GoL(30,p,500))
  for(k in c(1:500)){
    living_cells<-c(living_cells, sum(matrices[,,k,]/500)) 
  }
  plot(living_cells, ylab="Mean of living cells", type="l")
  living_cells <- c()
}