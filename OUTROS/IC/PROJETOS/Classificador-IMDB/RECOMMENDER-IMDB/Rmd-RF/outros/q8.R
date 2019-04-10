set.seed(42)
df <- data.frame(Category = sample(LETTERS), Count = rpois(26, 6))
df = db_miss

require("ggplot2")

p1 <- ggplot(df, aes(x = variables, y = percent_missing)) +
  geom_bar(stat = "identity")

p2 <- ggplot(df, aes(x = reorder(variables, -percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity") + geom_text(data = subset(cars, speed %% 5 == 0), aes(label = dist))

require("gridExtra")
grid.arrange(arrangeGrob(p1, p2))
