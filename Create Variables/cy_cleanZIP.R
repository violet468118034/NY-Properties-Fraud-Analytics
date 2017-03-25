# CREATE BOROBLOCK/TAXBIGCLASS/BLDBIGCLASS
library(dplyr)
cy <- data.frame(ny$BORO)
BOROBLOCK <- substr(ny$BBLE,1,6)
TAXBIG <- substr(ny$TAXCLASS,1,1)
BLDCLBIG <- substr(ny$BLDGCL,1,1)
ny$BOROBLOCK <- substr(ny$BBLE,1,6)
cy <- cbind(cy,BOROBLOCK,TAXBIG,BLDCLBIG)
cy$RECORD <- ny$RECORD
colnames(cy)[1]<-"BORO"


# CREATE ZIP
cy$ZIPold<- ny$ZIP

## GROUP BOROBLOCK & ZIP
ZIPt <- ny%>%
  group_by(BOROBLOCK,ZIP)%>%
  summarise(n=n())%>%
  arrange(BOROBLOCK,-n)

### DELETE DUPLICATES WITH LOW FREQUENCY COMBINATION
ZIPtDup<- duplicated(ZIPt[,1])
ZIPt<-ZIPt[!ZIPtDup,]

### CALCULATE ZIP: ALIGN NA ZIP TO THEIR NEIGHBOUR ZIPS
for (i in 1:nrow(ZIPt)){
  if(is.na(ZIPt$ZIP[i])){
    ZIPt$ZIP[i]=ZIPt$ZIP[i-1]
  }
}

## MERGE
cy2<-merge(cy,ZIPt,by.x="BOROBLOCK",by.y="BOROBLOCK",all.x=T)
colnames(cy2)[7] <- "ZIPnew"

## ALIGN NA ZIP TO NEW ZIPS

cy2$ZIP <- cy2$ZIPold
for (i in 1:nrow(cy2)){
  if(is.na(cy2$ZIP[i])){
    cy2$ZIP[i]=cy2$ZIPnew[i]
  }
}
sum(is.na(cy2$ZIP))

## DELETE UNUSED COLUMNS 
cy3 <- cy2[,c(5,2,1,3:4,9)]
ZIP <- cy3

## ORDER
ZIP <- ZIP[order(ZIP$RECORD),]
write.csv(ZIP,"zip.csv")
