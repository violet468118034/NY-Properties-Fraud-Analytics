
zc <- Cleandata_zy[, c(3, 33, 11, 34, 15:17)]
#zc <- head(zc, 1000)

##numeric data

library(dplyr)

data1 <- zc %>%
  group_by(BOROBLOCK) %>%
  summarise(fullval_boroblock = mean(FULLVAL), 
            avland_boroblock = mean(AVLAND), 
            avtot_boroblock = mean(AVTOT))

data2 <- zc %>%
  group_by(TAXCLASS) %>%
  summarise(fullval_taxclass = mean(FULLVAL), 
            avland_taxclass = mean(AVLAND), 
            avtot_taxclass = mean(AVTOT))

data3 <- zc %>%
  group_by(TAXBIG) %>%
  summarise(fullval_taxbig = mean(FULLVAL), 
            avland_taxbig = mean(AVLAND), 
            avtot_taxbig = mean(AVTOT))

nyc <- merge(zc, data1, by.x = c("BOROBLOCK"), by.y = c("BOROBLOCK"), 
             all.x = T)

nyc <- merge(nyc, data2, by.x = c("TAXCLASS"), by.y = c("TAXCLASS"), 
             all.x = T)

nyc <- merge(nyc, data3, by.x = c("TAXBIG"), by.y = c("TAXBIG"), 
             all.x = T)

nyc <- nyc %>%
  arrange(RECORD)

nyc$FullvalBoroblock = nyc$FULLVAL/nyc$fullval_boroblock
nyc$FullvalTaxclass = nyc$FULLVAL/nyc$fullval_taxclass
nyc$FullvalTaxbig = nyc$FULLVAL/nyc$fullval_taxbig
nyc$AvlandBoroblock = nyc$AVLAND/nyc$avland_boroblock
nyc$AvlandTaxclass = nyc$AVLAND/nyc$avland_taxclass
nyc$AvlandTaxbig = nyc$AVLAND/nyc$avland_taxbig
nyc$AvtotBoroblock = nyc$AVTOT/nyc$avtot_boroblock
nyc$AvtotTaxclass = nyc$AVTOT/nyc$avtot_taxclass
nyc$AvtotTaxbig = nyc$AVTOT/nyc$avtot_taxbig

ny <- nyc[, c(4, 17:25)]

### transfer 0 to 1

for (i in 2:length(ny)){
  ny[, i] <- ifelse(ny[,i] == 0, 1, ny[,i])
}

for (i in 2:length(ny)){
  ny[, i] <- ifelse(is.na(ny[,i]), 1, ny[,i])
}

for (i in 2:length(ny)){
  b <- nrow(subset(ny, ny[, i] == 0))
  print(b)
}

for (i in 2:length(ny)){
  b <- nrow(subset(ny, is.na(ny[,i])))
  print(b)
}

###Z-scale
nyc_z <- data.frame(ny$RECORD)
var = colnames(ny[2:10])

for (i in 1:9){
  nyc_z[paste(var[i], "_z", sep="")] <- scale(ny[, var[i]])
}

nyc_z <- nyc_z[, c(2, 8, 5, 3, 9, 6, 4, 10, 7)]

save(nyc_z, file = "nyc_zc.RData")
#nyc_z <- cbind(cyz, nyc_z)
#names(nyc_z)[1] = c("RECORD")

#save(nyc_z, file = "nyc_z.RData")
