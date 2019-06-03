n = dim(missing_data)[1]
dat <- missing_data
#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))
#This adds a column of color values
# based on the y values
dat$Col <- rbPal(10)[as.numeric(cut(dat$percent_missing,breaks = 10))]
plot(dat$percent_missing,dat$percent_missing,pch = 20,col = dat$Col)

ddb_NULL <- data.frame(x = seq_len(n) - 1) %>% 
  mutate(
    db
    y = 10 + x + 10 * sin(x),
    y = round(y, 1),
    z = (x*y) - median(x*y),
    e = 10 * abs(rnorm(length(x))) + 2,
    e = round(e, 1),
    low = y - e,
    high = y + e,
    value = y,
    name = sample(missing_data$variables, size = n),
    color = rep(colors, length.out = n),
    segmentColor = rep(colors2, length.out = n)
  )

missing_data


fruit[str_length(fruit) <= 5]


