CLT_Sim <- function(dist = "Dice", par = NULL,
                    sample = 30, trials = 1000){
        
        sample.list <- list()
        
        if(dist == "Dice"){
                for(i in 1:trials){
                        sample.list[[i]] <- sample(1:6, sample, replace = TRUE)
                }
        }else if(dist == "Normal"){
                for(i in 1:trials){
                        sample.list[[i]] <- rnorm(sample, mean = par["mean"], sd = par["sd"])
                }
        }else if(dist == "Uniform"){
                for(i in 1:trials){
                        sample.list[[i]] <- sample(par, sample, reaplce = TRUE)
                }
        }else if(dist == "Chi2"){
                for(i in 1:trials){
                        sample.list[[i]] <- rchisq(sample, df = par["df"])
                }
        }else if(dist == "Binomial"){
                for(i in 1:trials){
                        sample.list[[i]] <- rbinom(sample, size = par["size"], prob = par["prob"])
                }
        }else if(dist == "Poisson"){
                for(i in 1:trials){
                        sample.list[[i]] <- rpois(sample, lambda = par["lambda"])
                }
        }else{
                warning("正しい分布名を入力して下さい")
                break
        }
        
        sample.mean <- lapply(sample.list, mean)
        
        hist.lim <- c(round(min(unlist(sample.mean)) - 1, 0), 
                      round(max(unlist(sample.mean)) + 1, 0))

        for(i in 1:length(sample.mean)){
                #file_name = paste("CLT_", i, ".png", sep = "")
                #quartz(type = "png", width = 10, height = 5, file = file_name)
                par(mfrow = c(1, 2))
                plot(x = 1:10, y = 1:10, type = "n", xlab = "", ylab = "",
                     xaxt = "n", yaxt = "n", 
                     main = paste("# of Trials =", i))
                for(j in 0:9){
                        if(i + j > trials){
                                printed.text = ""
                        }else{
                                printed.text = paste(i + j,"回目: ",
                                                     paste0(as.character(sample.list[[i + j]][1:5]), 
                                                            sep = ", ", collapse =  ""),
                                                     "... | 平均値 = ",
                                                     round(mean(sample.mean[[i + j]]), 2))
                        }
                        
                        text(x = 0.5, y = 10 - j, labels = printed.text, 
                             cex = 0.8, pos = 4)
                }
                hist(x = unlist(sample.mean)[1:i], main = "",
                     xlim = hist.lim, ylab = "割合", xlab = "標本平均", prob = TRUE)
                if(i >= 2){
                        lines(density(unlist(sample.mean)[1:i], adjust = 2), col="red", lwd=2)
                }
                par(mfrow = c(1, 1))
                #dev.off()
                Sys.sleep(.1)    
        }
        
}

CLT_Sim(dist = "Dice", sample = 50, trials = 2000)