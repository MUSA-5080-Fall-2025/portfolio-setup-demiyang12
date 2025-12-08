# ✏️ 改成你的数据集名
data_name <- hp_clean     # 比如 hp、housing_data、joined_data 等

# ✏️ 改成你的变量名
Y_var <- "sale_price"   # 因变量
X_var <- "market_value"   # 自变量

# 生成不同变换列（名字固定，方便替换）
data_name <- data_name %>%
  filter(!is.na(.data[[Y_var]]), !is.na(.data[[X_var]]),
         .data[[Y_var]] > 0, .data[[X_var]] > 0) %>%
  mutate(
    Y = .data[[Y_var]],
    X = .data[[X_var]],
    logY = log(Y),
    logX = log(X),
    sqrtX = sqrt(X),
    X2 = X^2
  )

# 六种关系图
p1 <- ggplot(data_name, aes(X, Y)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "Y ~ X")

p2 <- ggplot(data_name, aes(logX, Y)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "Y ~ log(X)")

p3 <- ggplot(data_name, aes(sqrtX, Y)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "Y ~ sqrt(X)")

p4 <- ggplot(data_name, aes(X2, Y)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "Y ~ X²")

p5 <- ggplot(data_name, aes(X, logY)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "log(Y) ~ X")

p6 <- ggplot(data_name, aes(logX, logY)) +
  geom_point(alpha = 0.4) + geom_smooth(method = "lm", color = "red") +
  labs(title = "log(Y) ~ log(X)")

# 拼成一页显示
(p1 | p2 | p3) / (p4 | p5 | p6)