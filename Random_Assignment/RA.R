# 作図のためにggplot2パッケージの呼び出し
# ない場合、まずinstall.packages("ggplot2")でインストールしておく
library(ggplot2)

# 実験群の数(例：統制群, 実験群1, 実験群2)
n.group <- 3

# シミュレーション結果を保存するデータフレームの作成
result.df <- data.frame(n = rep(NA, 996),
                        sex.mean = rep(NA, 996),
                        age.mean = rep(NA, 996),
                        trust.mean = rep(NA, 996),
                        n1 = rep(NA, 996),
                        n2 = rep(NA, 996),
                        n3 = rep(NA, 996),
                        sex.mean1 = rep(NA, 996),
                        sex.mean2 = rep(NA, 996),
                        sex.mean3 = rep(NA, 996),
                        age.mean1 = rep(NA, 996),
                        age.mean2 = rep(NA, 996),
                        age.mean3 = rep(NA, 996),
                        trust.mean1 = rep(NA, 996),
                        trust.mean2 = rep(NA, 996),
                        trust.mean3 = rep(NA, 996))

# シミュレーションのコード

for (i in 5:1000){
  # 架空のデータセットの生成する
  # 各グループに大体x人が属し、３つのグループで分けるなら
  # xの3倍のサンプルサイズが必要
  sex <- sample(0:1, i * n.group, replace = TRUE)
  age <- sample(20:70, i * n.group, replace = TRUE)
  trust <- sample(1:5, i * n.group, replace = TRUE)

  # グループを分ける
  group.id <- sample(1:n.group, i * n.group, replace = TRUE)

  # 架空データセットをデータフレームとして保存
  temp.df <- data.frame(sex, age, trust, group.id)

  # サンプルサイズ
  sample.size <- i * n.group

  # サンプル全体のsex, age, trustの平均値
  sex.mean <- mean(temp.df$sex)
  age.mean <- mean(temp.df$age)
  trust.mean <- mean(temp.df$trust)

  # 各グループにいくつのケースが属されているかを計算
  n1 <- nrow(subset(temp.df, group.id == 1))
  n2 <- nrow(subset(temp.df, group.id == 2))
  n3 <- nrow(subset(temp.df, group.id == 3))

  # 各グループごとのsex, age, trustの平均値を計算する
  sex.mean1 <- mean(subset(temp.df, group.id == 1)$sex)
  sex.mean2 <- mean(subset(temp.df, group.id == 2)$sex)
  sex.mean3 <- mean(subset(temp.df, group.id == 3)$sex)
  age.mean1 <- mean(subset(temp.df, group.id == 1)$age)
  age.mean2 <- mean(subset(temp.df, group.id == 2)$age)
  age.mean3 <- mean(subset(temp.df, group.id == 3)$age)
  trust.mean1 <- mean(subset(temp.df, group.id == 1)$trust)
  trust.mean2 <- mean(subset(temp.df, group.id == 2)$trust)
  trust.mean3 <- mean(subset(temp.df, group.id == 3)$trust)

  # 結果を保存
  result.df[i-4, ] <- c(sample.size, sex.mean, age.mean, trust.mean,
                        n1, n2, n3,
                        sex.mean1, sex.mean2, sex.mean3,
                        age.mean1, age.mean2, age.mean3,
                        trust.mean1, trust.mean2, trust.mean3)
}

# 各グループにちゃんと1/3ずつ入ったかを確認。
# 赤の実線に近いほど理想的な割合で配分されたということ
ggplot(data = result.df) +
  geom_line(aes(x = n, y = n1/n, color = "red"),   alpha = 0.4) +
  geom_line(aes(x = n, y = n2/n, color = "blue"),  alpha = 0.4) +
  geom_line(aes(x = n, y = n3/n, color = "green"), alpha = 0.4) +
  geom_hline(yintercept = 1/3, color = "red") +
  scale_color_discrete(name = "Group",
                       labels = c("Group1", "Group3", "Group2")) +
  labs(x = "Sample size", y = "Sample size in each group") +
  theme(legend.position = "bottom")

# 各グループのsex, age, trustの平均値。
# 紫(?)は全体の平均値
# 赤い実線は理論値
ggplot(data = result.df) +
  geom_line(aes(x = n, y = sex.mean,  color = "black"), alpha = 0.4) +
  geom_line(aes(x = n, y = sex.mean1, color = "red"),   alpha = 0.4) +
  geom_line(aes(x = n, y = sex.mean2, color = "blue"),  alpha = 0.4) +
  geom_line(aes(x = n, y = sex.mean3, color = "green"), alpha = 0.4) +
  geom_hline(yintercept = 0.5, color = "red") +
  scale_color_discrete(name = "Group",
                       labels = c("Group1", "Group3", "Group2", "Entire")) +
  labs(x = "Sample size", y = "Mean of Sex") +
  theme(legend.position = "bottom")

ggplot(data = result.df) +
  geom_line(aes(x = n, y = age.mean,  color = "black"), alpha = 0.4) +
  geom_line(aes(x = n, y = age.mean1, color = "red"),   alpha = 0.4) +
  geom_line(aes(x = n, y = age.mean2, color = "blue"),  alpha = 0.4) +
  geom_line(aes(x = n, y = age.mean3, color = "green"), alpha = 0.4) +
  geom_hline(yintercept = 45, color = "red") +
  scale_color_discrete(name = "Group",
                       labels = c("Group1", "Group3", "Group2", "Entire")) +
  labs(x = "Sample size in each group", y = "Mean of Age") +
  theme(legend.position = "bottom")

ggplot(data = result.df) +
  geom_line(aes(x = n, y = trust.mean,  color = "black"), alpha = 0.4) +
  geom_line(aes(x = n, y = trust.mean1, color = "red"),   alpha = 0.4) +
  geom_line(aes(x = n, y = trust.mean2, color = "blue"),  alpha = 0.4) +
  geom_line(aes(x = n, y = trust.mean3, color = "green"), alpha = 0.4) +
  geom_hline(yintercept = 3, color = "red") +
  scale_color_discrete(name = "Group",
                       labels = c("Group1", "Group3", "Group2", "Entire")) +
  labs(x = "Sample size in each group", y = "Mean of Trust") +
  theme(legend.position = "bottom")
