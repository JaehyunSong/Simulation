Cjoint.N.Sim <- function(n.Attr, n.Level, n.Choice, 
                         n.Sim, n.Min, n.Max, chk.dupli = TRUE,
                         min.occur = 500){
        
        # Dependency: doMC, foreach
        
        # n.Attr   : a number of attributes (scalar)
        # n.Level  : numbers of levels in each attributes (vector
        # n.Choide : a number of profiles (scalar)
        # n.Sim    : a number of trials (scalar)
        # n.Min    : Minimum sample size (scalar)
        # n.Max    : Maximum sample size (scalar)
        # chk.dupli: Deleting cases if same levels were shown (logical)
        # min.occur: Minimun number of occurences
        
        # Return: Plot

        library(doMC)
        library(foreach)
        registerDoMC(cores = detectCores())
                
        temp.list <- foreach(i = 1:(n.Max - n.Min + 1)) %dopar% {
                
                n <- i + n.Min - 1
                temp.vec3 <- c()
                
                for(j in 1:n.Sim){
                        temp.vec2 <- c()
                        
                        for(k in 1:n.Attr){
                                temp.vec1 <- sample(1:n.Level[k], n * n.Choice, 
                                                    replace = TRUE)
                                
                                if(chk.dupli == TRUE){
                                        
                                        for(l in seq(from = 1, to = (n * n.Choice), by = 2)){
                                                
                                                if(temp.vec1[l] == temp.vec1[l + 1]){
                                                        
                                                        temp.vec1[l:(l + 1)] <- NA
                                                        
                                                }
                                                
                                        }
                                        
                                }else{
                                        
                                }
                                
                                temp.tbl  <- table(temp.vec1)
                                temp.vec2[k] <- min(temp.tbl)
                        }
                        
                        temp.vec3[j] <- min(temp.vec2)
                        
                }
                
                temp.vec3
        }
        
        prop.calc <- function(x) { return(mean(x >= min.occur)) }
        
        result <- unlist(lapply(temp.list, prop.calc))
        
        plot(x = n.Min:n.Max, y = result, type = "l",
             ylim = c(0, 1), col = "black",
             xlab = "Respondents × Task", ylab = "Proportion", 
             main = "Conjoint Sample Size Simulator")
        
}

#Cjoint.N.Sim(n.Attr = 2, n.Level = c(2, 4), n.Choice = 2, 
#             n.Sim = 100, n.Min = 1300, n.Max = 1550, chk.dupli = TRUE,
#             min.occur = 500)