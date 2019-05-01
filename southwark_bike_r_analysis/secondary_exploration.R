library(reshape2)

#let's find some clusters/groups
hist(bike_count_oob_loc$DFT.PEDAL.CYCLE)

unique(bike_count_oob_loc$DIRECTION)

sum(bike_count_oob_loc[bike_count_oob_loc$DIRECTION == "N" | bike_count_oob_loc$DIRECTION = "W",])

#test out the different directions
hist(bike_count_oob_loc$DIRECTION)

ggplot(bike_count_oob_loc, aes(x = DIRECTION)) +
  geom_bar()

ggplot(bike_count_oob, aes(x = TOTAL.FLOW, y = DFT.PEDAL.CYCLE, colour = DIRECTION)) + 
  geom_point()

#how about plotting bars of each location and what directioe they are in
ggplot(bike_count_oob_loc, aes(x = DFT.PEDAL.CYCLE, colour = DIRECTION)) +
  geom_bar()

#get the data into a wide format
bike_count_sub <- bike_count_oob_loc[ ,c(1,2,3,4,5,15)]

bike_count_sub$geometry <- NULL

bike_count_oob_wide <- melt(bike_count_sub, id = 1:5)

bike_count_oob_wide <- dcast(bike_count_oob_wide, ID + ROAD + MONTH + YEAR ~ DIRECTION)

#now plot it

ggplot(bike_count_oob_wide, aes(x = DFT.PEDAL.CYCLE, colour = DIRECTION)) +
  geom_bar()

#for viz stuff best to keep it long

#now that it is wide

#NSEW analysis

bike_count_oob_wide$perc_E <- (100/(bike_count_oob_wide$E+bike_count_oob_wide$W))*bike_count_oob_wide$E
bike_count_oob_wide$perc_W <- (100/(bike_count_oob_wide$E+bike_count_oob_wide$W))*bike_count_oob_wide$W
bike_count_oob_wide$perc_N <- (100/(bike_count_oob_wide$N+bike_count_oob_wide$S))*bike_count_oob_wide$N
bike_count_oob_wide$perc_S <- (100/(bike_count_oob_wide$N+bike_count_oob_wide$S))*bike_count_oob_wide$S

#also total
bike_count_oob_wide$total_bike <- rowSums(bike_count_oob_long[, c("N","E", "S", "W")], na.rm = TRUE)

#look at the distib of this total data for road/date
hist(bike_count_oob_wide$total_bike)

#look at direction share
rand_sub <- bike_count_oob[bike_count_oob$YEAR==2016, ]


rand_sub <- rand_sub[1:10, ]

#get a palette
cbPalette <- c("#E69F00", "#CC79A7", "#56B4E9", "#009E73")

poke_palette <- c('#a48b73', '#c5b4c5', '#bd314a', '#624a73')

#make a geom_bar of it
ggplot(rand_sub, aes(x=ROAD, y=DFT.PEDAL.CYCLE, fill = DIRECTION)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=cbPalette) +
  xlab("Location") +
  ylab("No. Bicycles") +
  theme_bw()
  

