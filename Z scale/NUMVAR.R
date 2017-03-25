load("cleaned_zhy.Rdata")
load("cleaned_lot.RDA")

data1 = cleaned_zhy
data2 = ny
library(dplyr)

record,BLDCL,ZIP,STORIES,BLDFRONT,BLDDEPTH,FP_BLD,BLDVOL,LOTFRONT,LOTDEPTH,
FP_LOT

data3 =data1 %>%
  select(RECORD,BLDGCL,ZIP_NEW,STORIES,FULLVAL,AVTOT,AVLAND,FP_BLD,BLDFRONT,
         BLDDEPTH,BLDVOL)

data4 = data2 %>%
  select(LTFRONT,LTDEPTH,FP_LOT)

data5 = cbind(data3,data4)

data5$LTFRONT = round(data5$LTFRONT,0)
data5$LTDEPTH = round(data5$LTDEPTH,0)
data5$FP_LOT = round(data5$FP_LOT,0)

names(data5)[3]<-paste("ZIP") 
NUMVAR = data5
save(NUMVAR,file = "NUMVAR.RData")
