
## Q Table Crawler ## 
crawler <- function(currstate, goal){
  path  <- currstate
  count <- 0
  epsilon <- 0.8
  steps <- 30
  nextstate <- 0
  while(count < steps & nextstate != goal){
   if(steps*epsilon){
      nextstate <- getNextStates(currstate)[which(Q[getNextStates(currstate)] == max(Q[getNextStates(currstate)]))]
   }else {
   nextstate <- getNextStates(currstate)[which(Q[getNextStates(currstate)] > 0)]
   }
    nextstate <- ifelse(length(nextstate)>1,sample(nextstate), nextstate)
    path <- append(path,nextstate)
    currstate <- nextstate
    count <- count+ 1
  }
  traced_path <- paste0(path,collapse = "|")
  print(traced_path)
}

## This will find policy to reach the Goal from various states
for(i in 1:nrow(state_seq)){
  crawler(i,25)
}
