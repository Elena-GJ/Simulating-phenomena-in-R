# Interactive version of the program

# We ask for the necessary data to the user
available_money <- NA
while(is.na(available_money) || available_money <= 0) {
  available_money <- readline(prompt = "Indicate the available money: ")
  available_money <- as.integer(available_money)
}

initial_money <- available_money

initial_bid <- NA
while(is.na(initial_bid) || initial_bid > available_money) {
  initial_bid <- readline(prompt = "Insert and initial amount for bidding: ")
  initial_bid <- as.integer(initial_bid)
}

max_plays <- 0
while(is.na(max_plays) || max_plays <= 0) {
  max_plays <- readline(prompt = "Indicate the maximum number of plays desired: ")
  max_plays <- as.integer(max_plays)
}

prob_win <- sample(0:10, size=1)  # For the interactive version of the code, the probability of winning is random
  
# The martingale function is defined. It receives as argument the initial bid, the initial amout of total money available, 
# and the maximum number of plays

martingale <- function(available_money, initial_bid, max_plays, prob_win, output) {
  num_plays <- 1
  bid <- initial_bid
  won_money <- 0

  # While the total amount of available money is more than 0, and the number of plays id less or equal to the maximum  
  # number of plays
  while (available_money > 0 & num_plays <= max_plays) {
    #	You win or lose randomly
    win_lose <- sample(c("win", "lose"), prob=c(prob_win,10-prob_win), size = 1)
    #	If you win
    if (win_lose=="win") {
      # The total amount of money adds the initial quantity
      available_money <- bid + available_money
      #	The bid is still the initial bid
      bid <- initial_bid
      #	The won money adds the bid
      won_money <- won_money + bid
    }
    # If you lose
    else {
      # You lose the bid from the available money you've got
      available_money <- available_money - bid
      # The bid is substracted from the won money
      won_money <- won_money - bid
      # Bid is doubled
      bid <- bid * 2
    }
    ##  num_plays increases by 1
    num_plays <- num_plays + 1
  }
  if (output == "Won money"){
    return(won_money)
  } else {
    return(num_plays)
  }
}

# The function is called and the result is provided
cat(sprintf("The won money is: %5.3f", martingale(available_money, initial_bid, max_plays, prob_win, "Won money")))
    




# Study of the maximum number of plays that can be played as a function of the winning probability
num_plays <- 0
probabilities <- seq(0,1, by= 0.01)
for (i in probabilities){
  num_plays <- c(num_plays, mean(replicate(1000, martingale(10, 500, 1000, i*10, "Number of plays"))))
}
num_plays <- num_plays[-1]
dataframe_num_plays <- data.frame(probabilities)
dataframe_num_plays[,2] <- num_plays

print(dataframe_num_plays[2])
plot(num_plays, xaxt="n", type="l", main = "Number of maximum plays as a function of winning probability")
axis(1, at=c(1:101), labels=probabilities)




# Study of benefits as a function of the probability of winning the bid
benefits <- 0
# Sequence of probabilities
probabilities <- seq(0, 1, by=0.1)
for (i in probabilities){
  benefits <- c(benefits, mean(replicate(1000, martingale(500, 500, 100, i*10, "Won money"))))
}
benefits <- benefits[-1]
plot(benefits, type="l", xaxt = "n", main = "Expected benefits as a function of winning probability")
axis(1, at=c(1:11), labels = probabilities)

dataframe <- data.frame(probabilities)
dataframe[,2] <- benefits

print(dataframe[2])


