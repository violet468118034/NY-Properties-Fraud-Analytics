
library(dplyr)

## merge data
load("ny(lot).rda")
load("CleanBLD_zy.RData")

zy <- Cleandata_zy[,c(3:6, 34, 2, 10, 1, 11, 33, 14, 23, 24, 35,
                      36, 7, 9, 21, 15:17)]
roger <- ny[, c(9, 10, 35)]
ny_pro <- cbind(zy, roger)


##FULLVAL
market_price <- ny_pro %>%
  filter(FULLVAL != 0) %>%
  group_by(TAXBIG, ZIP) %>%
  summarise(avg_price = mean(FULLVAL/BLDVOL))

ny_new <- merge(ny_pro, market_price, by.x=c("TAXBIG","ZIP"),
                by.y = c("TAXBIG","ZIP"), all.x = T)

ny_new$Price = ifelse(!is.na(ny_new$avg_price), ny_new$avg_price, 
                      mean(ny_new$avg_price, na.rm = T))
ny_new$FULLVAL_AS = ny_new$Price*ny_new$BLDVO

ny_new$FULLVALfinal = ifelse(ny_new$FULLVAL != 0, ny_new$FULLVAL, ny_new$FULLVAL_AS)


##AVTOT

data1 = ny_new %>%
  filter(AVTOT != 0)
data1$asses_rate = data1$FULLVAL/data1$AVTOT
data1 <- data1[,c(3, 29)]

ny_new <- merge(ny_new, data1, by.x=c("RECORD"),
                by.y = c("RECORD"), all.x = T)

Asses_Rate <- ny_new %>%
  filter(asses_rate != 0) %>%
  group_by(TAXBIG, ZIP) %>%
  summarise(avg_asses_rate = mean(asses_rate))

ny_new <- merge(ny_new, Asses_Rate, by.x=c("TAXBIG","ZIP"),
                by.y = c("TAXBIG","ZIP"), all.x = T)

ny_new$assessed_rate = ifelse(!is.na(ny_new$avg_asses_rate), ny_new$avg_asses_rate, 
                      mean(ny_new$avg_asses_rate, na.rm = T))

ny_new$AVTOTfinal <- ifelse(ny_new$AVTOT != 0, ny_new$AVTOT, 
                            ny_new$FULLVALfinal/ny_new$assessed_rate)

##AVLAND

data2 = ny_new %>%
  filter(AVLAND != 0)
data2$ratio = data2$AVLAND/data2$AVTOT
data2 <- data2[,c(3, 33)]


ny_new <- merge(ny_new, data2, by.x=c("RECORD"),
                by.y = c("RECORD"), all.x = T)

Ratio <- ny_new %>%
  filter(ratio != 0) %>%
  group_by(TAXBIG, ZIP) %>%
  summarise(avg_ratio = mean(ratio))

ny_new <- merge(ny_new, Ratio, by.x=c("TAXBIG","ZIP"),
                by.y = c("TAXBIG","ZIP"), all.x = T)

ny_new$avg_L_T_ratio <- ifelse(is.na(ny_new$avg_ratio), 
                               mean(ny_new$avg_ratio, na.rm = T),
                               ny_new$avg_ratio)

ny_new$AVLANDfinal <- ifelse(ny_new$AVLAND != 0, ny_new$AVLAND, 
                            ny_new$avg_L_T_ratio*ny_new$AVTOTfinal)

ny_new <- ny_new %>%
  arrange(RECORD)

#####
nyc_pro <- ny_new[, -c(19:21, 25:27, 29:31, 33:35)]
nyc_pro$FULLVAL <- nyc_pro$FULLVALfinal
nyc_pro$AVTOT <- nyc_pro$AVTOTfinal
nyc_pro$AVLAND <- nyc_pro$AVLANDfinal
nyc_pro <- nyc_pro[, c(3:10, 1:2, 11:21, 25:27)]

save(nyc_pro, file = "nyc_pro.RData")

mean(nyc_pro$FULLVAL)
mean(nyc_pro$AVTOT)
mean(nyc_pro$AVLAND)