nauru.income <- round(rnorm(10210, mean = 5000, sd = 2000))
require(ggplot2)
plot1 <- ggplot(data = data.frame(nauru.income)) + geom_histogram(aes(x = nauru.income, y = ..density..),
                                   fill = "white", color = "black", binwidth = 500) +
  stat_function(fun = dnorm, args = list(mean = 5000, sd = 2000),
                color = "red", size = 1) +
  labs(x = "Income", y = "Density")
print(plot1)

result.df <- data.frame(n = NA, mean = NA) #まずは空のデータフレイムを作っとく
for(i in 1:1000){
  result.df[i, ] <- c(i,
                      mean(sample(nauru.income, i, replace = TRUE)))
}
head(result.df)

plot2 <- ggplot(data = result.df[]) + 
  geom_hline(yintercept = mean(result.df$mean), color = "red") +
  geom_line(aes(x = n, y = mean)) +
  labs(x = "Sample Size", y = "Mean")
print(plot2)

result.df2 <- data.frame(n = NA, 
                         mean1 = NA,
                         mean2 = NA,
                         mean3 = NA,
                         mean4 = NA,
                         mean5 = NA,
                         mean6 = NA,
                         mean7 = NA,
                         mean8 = NA,
                         mean9 = NA,
                         mean10 = NA) #まずは空のデータフレイムを作っとく
for(i in 1:1000){
  result.df2[i, ] <- c(i,
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)),
                      mean(sample(nauru.income, i, replace = TRUE)))
}
head(result.df)

plot3 <- ggplot(data = result.df2[1:100,]) + 
  geom_hline(yintercept = mean(result.df$mean), color = "red") +
  geom_line(aes(x = n, y = mean1), alpha = 0.5) +
  geom_line(aes(x = n, y = mean2), alpha = 0.5) +
  geom_line(aes(x = n, y = mean3), alpha = 0.5) +
  geom_line(aes(x = n, y = mean4), alpha = 0.5) +
  geom_line(aes(x = n, y = mean5), alpha = 0.5) +
  geom_line(aes(x = n, y = mean6), alpha = 0.5) +
  geom_line(aes(x = n, y = mean7), alpha = 0.5) +
  geom_line(aes(x = n, y = mean8), alpha = 0.5) +
  geom_line(aes(x = n, y = mean9), alpha = 0.5) +
  geom_line(aes(x = n, y = mean10), alpha = 0.5) +
  labs(x = "Sample Size", y = "Mean")
print(plot3)