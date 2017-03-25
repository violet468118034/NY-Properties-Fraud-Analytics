# cy : zipcode + bldcls *2
library(dplyr)
save(ny_pro, file = "final.RData")

cy <- ny_pro[,c(1,6,7,8,19:21)]

# zip
cy2 <- cy%>%
  group_by(ZIP)%>%
  mutate(FullMZip = mean(FULLVAL),AvtotMZip = mean(AVTOT),AvlandMZip=mean(AVLAND))

# bldcl
cy3 <- cy%>%
  group_by(BLDGCL)%>%
  mutate(FullMBld = mean(FULLVAL),AvtotMBld = mean(AVTOT),AvlandMBld=mean(AVLAND))

# bldcl big
cy4 <- cy%>%
  group_by(BLDCLBIG)%>%
  mutate(FullMBldB = mean(FULLVAL),AvtotMBldB = mean(AVTOT),AvlandMBldB=mean(AVLAND))

cynew <- cbind(cy,cy2[,c(8:10)],cy3[,c(8:10)],cy4[,c(8:10)])

# create new field
create <- function(value, byclass){
  return(value/byclass)
}

## ZIP
cynew$FullvalZip <- create(cynew$FULLVAL,cynew$FullMZip)
cynew$AvtotZip <- create(cynew$AVTOT,cynew$AvtotMZip)
cynew$AvlandZip <- create(cynew$AVLAND,cynew$AvlandMZip)

## bldcl
cynew$FullvalBld <- create(cynew$FULLVAL,cynew$FullMBld)
cynew$AvtotBld <- create(cynew$AVTOT,cynew$AvtotMBld)
cynew$AvlandBld<- create(cynew$AVLAND,cynew$AvlandMBld)

## bldclbig
cynew$FullvalBldB <- create(cynew$FULLVAL,cynew$FullMBldB)
cynew$AvtotBldB <- create(cynew$AVTOT,cynew$AvtotMBldB)
cynew$AvlandBldB <- create(cynew$AVLAND,cynew$AvlandMBldB)

cynew$FullvalBld <- ifelse(cynew$FullvalBld=="NaN",cynew$FullvalBldB,cynew$FullvalBld)
cynew$AvtotBld <- ifelse(cynew$AvtotBld=="NaN",cynew$AvtotBldB,cynew$AvtotBld)
cynew$AvlandBld <- ifelse(cynew$AvlandBld=="NaN",cynew$AvlandBldB,cynew$AvlandBld)
cyfinal <- cynew[,c(1,17:25)]

save(cyfinal, file = "cyfinal.RData")

colnames(cyfinal)

# z-score 
cyz <- data.frame(cy$RECORD)

var=colnames(cyfinal[2:10])

for (i in 1:9){
  cyz[paste(var[i],"_z",sep="")] <- scale(cyfinal[,var[i]])
}
colnames(cyz)[1] <-"RECORD"
save(cyz, file = "cy_zscale.RData")
