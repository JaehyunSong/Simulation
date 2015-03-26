# A Simulation of the Monty Hall Problem
MH.sim <- function(trials = 1000){
  result.df <- data.frame(id = NA,
                          atari = NA,
                          selection = NA,
                          if.switch = NA,
                          if.not.switch = NA)
  
  for(i in 1:trials){
    atari <- sample(1:3, 1)
    selection <- sample(1:3, 1)
    
    if(atari == selection){
      if.switch <- 0
      if.not.switch <- 1
    }else{
      if.switch <- 1
      if.not.switch <- 0
    }
    
    result.df[i,] <- c(i,
                       atari,
                       selection,
                       if.switch,
                       if.not.switch)
  }

  return(result.df)  
}
