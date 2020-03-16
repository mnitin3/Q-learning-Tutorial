#### clean workspace #####
cat("\014")
rm(list = ls(all = TRUE)); #start with empty workspace
if(!is.null(dev.list()["RStudioGD"])){
 dev.off(dev.list()["RStudioGD"])
}
cat("\014")
#### clean workspace end #####

# Plotting the matrix
plot_matrix <- function(mat_pot, digits_arg){
  digits_arg <- ifelse(missing(digits_arg),0,digits_arg)
  plot(mat_pot, cex = 1.2, fmt.cell=paste0('%.',digits_arg,'f'), 
       col=brewer.pal(3,"Blues"), breaks=c(-500, 0, 0, 500),
       xlab="Actions", ylab = "States",main = "")
}

set.seed(3)
library('plot.matrix')
library(RColorBrewer)

##########
N <- 20         ## No. of Episode
alpha <- 0.8
gamma <- 0.7

  
###### Define Environment #######
states <- seq(1, 5, by = 1);
actions <- seq(1, 5, by = 1);
rewards <- c(0,-100,-100,0,-100,
                    10,10,10,10,-100,
                    10,10,-100,10,-100,
                    10,10,10,10,-100,
                    -100,-100,10,10,100
                    )
goal <- which(rewards==max(rewards), arr.ind=TRUE)

state_seq <- cbind(merge(actions,states), state = seq(1,length(states)*length(actions)))
state_mat <- matrix(state_seq$state, nrow = length(states), ncol= length(actions))
plot_matrix(state_mat)  ## matrix, digits

rewards_mat <- matrix(rewards, nrow = length(states), ncol= length(actions))
plot_matrix(rewards_mat) ## matrix, digits
# > rewards_mat
#       [,1] [,2] [,3] [,4] [,5]
# [1,]    0   10   10   10 -100
# [2,] -100   10   10   10 -100
# [3,] -100   10 -100   10   10
# [4,]    0   10   10   10   10
# [5,] -100 -100 -100 -100  100

## Initialize Q-Matrix
Q <- matrix(0,  nrow = length(states), ncol= length(actions))
plot_matrix(Q,1)

###### get Next states ####### 
diagonal_steps <- FALSE
getNextStates <- function(cs) {
  stalen <- length(states);
  actlen <- length(actions);
  NS <- stalen*actlen
  aa <- state_seq[state_seq$state == cs,]
  if (aa$x == max(states)) {
    ns <- c(cs - 1, 
            cs - stalen, 
            cs + stalen);
            #,cs - stalen - 1
            #,cs + stalen - 1);
    
  } else if (aa$x == min(states)) {
    ns <- c(cs + 1, 
            cs - stalen, 
            cs + stalen);
            #, cs - stalen + 1);#, 
            #cs + stalen + 1);
  } else {
    ns <- c(cs + 1,
            cs - 1,  
            cs - stalen, 
            cs + stalen);
            #,cs + stalen + 1
            #,cs - stalen + 1
            #,cs + stalen - 1
            #,cs - stalen - 1);
  }
  nss <- sort(ns[ns > 0 & ns <= NS]);
  return(nss);
}
getNextStates(1)


## Get reward for current state
get_Prereward <- function(current_state){
  reward_curr <- rewards[current_state]
  if(reward_curr == 0 | is.na(reward_curr) | length(reward_curr) == 0 ){
    reward_curr<-0
  }
  return(reward_curr)
}

########### Episodes Execution ################
for (i in 1:N) {
  if(!is.null(dev.list()["RStudioGD"])){#} & i%%10 ==0){
    dev.off(dev.list()["RStudioGD"])
  }
  #par=mfrow=c(1,2)
  #plot_matrix(rewards_mat)
  # plot_matrix(Q,1)
  # Sys.sleep(3)
  current_episode <- i;
  cat("\nStart Episode: ", current_episode)
  
  ## choose next state from possible actions at current state
  ## need to create environment. Where not possible actions get -1 in Reward.
  cs <- sample(state_seq$state, 1)
  cat("\n\tCurrent state: ", cs)
  cs1 <- cs;
  step_num <- 1; 
  while (T) {
    plot_matrix(Q,1)
    Sys.sleep(0.2)
    reward <- get_Prereward(cs)
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
    cs <- ns;
    if (cs1 == goal | step_num > 20) {
      break;
    }
    step_num <- step_num + 1;
  }
  cat("\nEnd Episode: ", current_episode)
}
