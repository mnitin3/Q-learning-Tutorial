#### clean workspace #####
cat("\014")
rm(list = ls(all = TRUE)); #start with empty workspace
if(!is.null(dev.list()["RStudioGD"])){
  dev.off(dev.list()["RStudioGD"])
}
cat("\014")
#### clean workspace end #####

set.seed(3)
library('plot.matrix')
library(RColorBrewer)

### Plotting the matrix Function ###
plot_matrix <- function(mat_pot, digits_arg){
  digits_arg <- ifelse(missing(digits_arg),0,digits_arg)
  plot(mat_pot, cex = 1.2, fmt.cell=paste0('%.',digits_arg,'f'), 
       col=brewer.pal(3,"Blues"), breaks=c(-500, 0, 0, 500),
       xlab="state", ylab = "",main = "")
}

###### Define Environment #######
states <- seq(1, 5, by = 1);
#actions <- seq(1, 5, by = 1);
state_seq <- cbind(merge(states,states), state = seq(1,length(states)*length(states)))
state_mat <- matrix(state_seq$state, nrow = length(states), ncol= length(states))
plot_matrix(state_mat)  ## matrix, digits

rewards <- c(0,-100,-100,0,-100,
             10,10,10,10,-100,
             10,10,-100,10,-100,
             10,10,10,10,-100,
             -100,-100,10,10,100
)
rewards_mat <- matrix(rewards, nrow = length(states), ncol= length(states))
plot_matrix(rewards_mat) ## matrix, digits

goal <- which(rewards==max(rewards), arr.ind=TRUE)
goal
# > rewards_mat
#       [,1] [,2] [,3] [,4] [,5]
# [1,]    0   10   10   10 -100
# [2,] -100   10   10   10 -100
# [3,] -100   10 -100   10   10
# [4,]    0   10   10   10   10
# [5,] -100 -100 -100 -100  100

## Initialize Q-Matrix
Q <- matrix(0,  nrow = length(states), ncol= length(states))
plot_matrix(Q,1)

###### get Next states ####### 
diagonal_steps <- FALSE
getNextStates <- function(cs) {
  stalen <- length(states);
  NS <- stalen*stalen
  aa <- state_seq[state_seq$state == cs,]
  if (aa$x == max(states)) {
    ns <- c(cs - 1, 
            cs - stalen, 
            cs + stalen);
  } else if (aa$x == min(states)) {
    ns <- c(cs + 1, 
            cs - stalen, 
            cs + stalen);
  } else {
    ns <- c(cs + 1,
            cs - 1,  
            cs - stalen, 
            cs + stalen);
  }
  nss <- sort(ns[ns > 0 & ns <= NS]);
  return(nss);
}
getNextStates(25)

########### Episodes Execution ################
N <- 10        # No. of Episode
alpha <- 0.8    # Learning Rate
gamma <- 0.7    # Discount Factor


for (i in 1:N) {
  if(!is.null(dev.list()["RStudioGD"])){
    dev.off(dev.list()["RStudioGD"])
  }
  current_episode <- i;
  cat("\nStart Episode: ", current_episode)
  
  ## choose next state from possible actions at current state
  cs <- sample(state_seq$state, 1)
  cat("\n\tCurrent state: ", cs)
  step_num <- 1; 
  while (T) {
    cat("\n\n\tStep no.: ", step_num)
    cat("\n\t\tCurrent State: ", cs)
    reward <- rewards[cs]
    if(reward == 0 | is.na(reward) | length(reward) == 0 ){
      reward <- 0
    }
    cat("\n\t\tReward CS: ", reward)
    next.states <- getNextStates(cs);
    cat("\n\t\tPossible next states: ", next.states)
    
    # next.states
    # If we have any states present, else choose randomly.
    if (length(next.states) == 1) {
      ns <- next.states
    } else {
      ns <- sample(next.states, 1)
    }
    cat("\n\t\tNext state: ", ns)
    
    # Update Q values for next states.
    Q[cs] <- round(Q[cs] + alpha * (reward + gamma * max(Q[getNextStates(ns)])-Q[cs]),1);
    cat("\n\t\tNew Q-Value: ", Q[cs])
    plot_matrix(Q,1)
    Sys.sleep(0.2)
    if (cs == goal | step_num > 20) {
      break;
    }
    cs <- ns;
    step_num <- step_num + 1;
  }
  cat("\nEnd Episode: ", current_episode)
}
