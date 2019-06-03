scores <- structure(list(Team = structure(c(3L, 4L, 2L, 1L, 7L, 6L, 5L), .Label = c("Chelsea", 
                                                                                    "Man City", "Manchester", "Liverpool", "Stoke", "West Ham", "Arsenal"
), class = "factor", scores = structure(c(-11, -32, -25, -30, 
                                          -26, -23, -22), .Dim = 7L, .Dimnames = list(c("Arsenal", "Chelsea", 
                                                                                        "Liverpool", "Man City", "Manchester", "Stoke", "West Ham")))), 
Goals = c(26L, 25L, 30L, 32L, 11L, 22L, 23L)), .Names = c("Team", 
                                                          "Goals"), row.names = c(NA, -7L), class = "data.frame")

db_miss = missing_data
db_miss$percent_missing = 100*db_miss$percent_missing
db_miss = db_miss[order(db_miss$percent_missing, decreasing = TRUE), ]
View(db_miss)

g <- ggplot(db_miss, 
            # keep all aesthetics in one place
            aes(x = variables, y = percent_missing, color = variables, fill = variables, label = round(percent_missing),4)) +
  # replacement of geom_bar(stat = "identity")
  geom_col() +
  # avoid overlap of text and bar to make text visible as bar and text have the same colour 
  geom_text(nudge_y = 1) + 
  # alternatively, print text inside of bar in discriminable colour
  # geom_text(nudge_y = -1, color = "black") + 
  ggtitle("Porcentagem de missing por variável") + 
  xlab("Variáveis") + ylab("Porcentagem de missing") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))
g
