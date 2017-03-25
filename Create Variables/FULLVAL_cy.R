library(dplyr)
roger<- ny[,c(1:2,30:31,4,7,33,8,32,9:10,35,11:14,20:21,34)]
zy <- Cleandata_zy[,c(35:36)]
nynew <- cbind(roger,zy)
#nynew <- head(nynew,1000)

# FULLVAL 
FV <- nynew%>%
  filter(FULLVAL!=0)%>%
  mutate(price = FULLVAL/BLDVOL)

FV2 <- FV%>%
  group_by(TAXBIG,ZIP)%>%
  summarise(Price = mean(price))

nynew$FULLVALold <- nynew$FULLVAL

nym <- merge(nynew,FV2,by.x=c("TAXBIG","ZIP"),by.y = c("TAXBIG","ZIP"),all.x = T)
pricemean <- mean(nym$Price,na.rm=T)
nym$Pricenew <- ifelse(is.na(nym$Price),pricemean,nym$Price)
nym$FULLVALnew <- nym$Pricenew*nym$BLDVOL

nym$FULLVAL <- ifelse(nym$FULLVALold==0 | is.na(nym$FULLVALold), nym$FULLVALnew,nym$FULLVALold)


# AVTOT
AVTOT <- nym%>%
  filter(AVTOT!=0)%>%
  mutate(ratio = AVTOT/FULLVAL)

AVTOT2 <- AVTOT%>%
  group_by(TAXBIG,ZIP)%>%
  summarise(Ratio = mean(ratio))

nym$AVTOTold <- nym$AVTOT

nym<- merge(nym,AVTOT2 , by.x=c("TAXBIG","ZIP"),by.y = c("TAXBIG","ZIP"),all.x = T)
nym$AVTOTnew <- nym$Ratio*nym$FULLVAL
nym$AVTOT <- ifelse(nym$AVTOTold==0, nym$AVTOTnew,nym$AVTOTold)


# AVLAND
AVLAND <- nynew%>%
  filter(AVLAND!=0)%>%
  mutate(avratio = AVLAND/AVTOT)

AVLAND2 <- AVLAND%>%
  group_by(TAXBIG,ZIP)%>%
  summarise(Avratio = mean(avratio))

nym$AVLANDold <- nym$AVLAND

nym <- merge(nym,AVLAND2,by.x=c("TAXBIG","ZIP"),by.y = c("TAXBIG","ZIP"),all.x = T)
nym$AVLANDnew <- nym$Avratio*nym$AVTOT

nym$AVLAND <- ifelse(nym$AVLANDold==0, nym$AVLANDnew,nym$AVLANDold)
colnames(nym)


nyf <- nym[,c(3:10,1,2,11:21)]

nyf <- nyf%>%
  arrange(RECORD)
save(nyf, file = "FULLVAL.RData")
save.image()
