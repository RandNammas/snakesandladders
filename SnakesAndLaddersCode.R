#Exercise 1.1.1
#Examine the following code and comment on it

#Maxmimum number of turns
max_turns <- 50

#This is your starting position (You start at 0)
position <- 0

#This is your end position (The game ends at position 60)
max_position <- 60

#Number of sides on a dice 
n_sides_die <- 6

#For loop to start the game
#For turn from 1 to 60
for ( turn in 1:max_turns ){
  #We now use the function sample choose a number from a side of the dice and putting it in die_roll, 1 is the number of dice  
  die_roll <- sample.int(n_sides_die, 1)
  
  #Moving to our new position as per number you get from roll
  position <- position + die_roll
  
  #If your position is more than 60, break the loop
  if ( position >= max_position ){
    break
  }
}

#Exercise 1.1.2
#Simulate a snakes and ladders game

start_game <- function() {

#Snakes_and_ladders_position_r_script
max_turns <- 100 #number of possible turns
current_position <- 0 #the position you start on
end_position <- 100 #last possible position
dice_sides <- 6 #number of sides on dice
n_turns <- 0 #number of game turns
snakes_position <- 0 #number of snakes encountered
ladders_position <- 0 #number of ladders encountered

#Indicates where each ladder starts and climbs to
ladder.df <- data.frame(start=c(1,4,9,21,28,36,51,71,80), end=c(38,14,31,42,84,44,67,91,100))

#Indicates where each snake starts and snakes to
snakes.df <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), end=c(78,75,73,24,60,19,53,11,26,6))

#Generating a loop to simulate a number of turns
for (turn in 1:max_turns) {
  #roll the dice to get random numbers
  dice_rolls = sample.int(dice_sides, 1)
  
  #moving the players position
  current_position = current_position + dice_rolls
  
  #Add 1 for each turn
  n_turns <- n_turns + 1
  
  
  # Check if we landed on a ladder to move to the top of it
  if (any(ladder.df$s == current_position)) {
    current_position <- ladder.df$e[ladder.df$s %in% current_position]
    ladders_position <- ladders_position + 1
  }
  
  # Check if we landed on a snake to slide down to its end
  if (any(snakes.df$s == current_position)) {
    current_position <- snakes.df$e[snakes.df$s %in% current_position]
    snakes_position <- snakes_position + 1
  }
  
  if (current_position >= end_position){
    break
  }
  
}

#Return number of turns to finish game
return(n_turns)

}


#Exercise 1.1.3
#Run simulation 1000 times to record number of turns and get average
#Total Number of games
Total_n <- 10000

#Storing number of turns to win in a vector
game_turns <- rep(0, Total_n)

#Looping my game function and finding the expected # of turns (mean), var, sd and CV
for (i in 1:Total_n){
  game_turns[i] <- start_game() 
  
  #Finding mean of expected number of turns for all games played
  expected_turns_n = mean(game_turns)
  
  #Find variance between expected number of turns for each game
  v <- var(game_turns)

  #Finding standard deviation of game turns
  s <- sd(game_turns)
  
  #Coefficient of variation to check if my sd is high or low
  #If CV >= 1, the sd is considered high, if CV<1 the sd is considered low
  #I have a low sd, meaning the samples are more clustered around the mean (more reliable)
  CV <- s/expected_turns_n
  
}

#Creating a Histogram 

#Reducing margin size because I kept getting this error: "figure margins too large"
#Chose the numbers this way so my labels on x and y axes would show
par(mar = c(5.1,4.1,4.1,2))

#Creating histogram with of my expected number of turns against my maximum n_turn (100)
hist(game_turns, 
     breaks = 100,
     main = "Turns to Win Snakes & Ladders",
     xlab = "Expected Turns Per Game",
     ylab = "Number of Games",
     ylim = c(0,300),
     xlim = c(0,100),
     border = "pink",
     col = "aquamarine1"
     )
