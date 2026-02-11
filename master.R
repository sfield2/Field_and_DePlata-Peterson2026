packages <-c('terra','sf','ggplot2','tidyterra')

for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

setwd("C:/Users/sfield3/OneDrive - University of Wyoming/FIELD LAB HOME/PROJECT/CLIMATE_VARIABILITY_REDUX")

theme_set(theme_bw())

## read in raster data
ppt <- terra::rast("./DATA/CLIMATE/PPT.tif")

clipr <- ext(c(xmin=-107.6,xmax=-107.45,ymin=35.25,ymax=35.4))

r <- crop(ppt,clipr)



## calculate changes year to year
r_var <- (subset(r, 1:(nlyr(r)-1)) - subset(r, 2:nlyr(r))) * -1



## example plots
r_df <- as.data.frame(r,xy=T)
r_var_df <- as.data.frame(r_var,xy=T)

ggplot(r_df,aes(x=x,y=y))+
  geom_raster(aes(fill=PPT_1))+
  geom_text(aes(label=round(PPT_1)),col="white")+
  scale_fill_viridis_c(direction= -1)+
  labs(title="Year 1")+
  theme(legend.position = "none")


ggplot(r_df,aes(x=x,y=y))+
  geom_raster(aes(fill=PPT_2))+
  geom_text(aes(label=round(PPT_2)),col="white")+
  scale_fill_viridis_c(direction= -1)+
  labs(title="Year 2")+
  theme(legend.position = "none")


ggplot(r_var_df,aes(x=x,y=y))+
  geom_raster(aes(fill=PPT_1))+
  geom_text(aes(label=round(PPT_1)),col="white")+
  scale_fill_viridis_c(direction= -1)+ 
  labs(title="Difference from Year 1 to Year 2")+
  theme(legend.position = "none")



## read in shapefile data
sd(r_var_df$PPT_1151)

ggplot()+
  geom_histogram(data=r_var_df,aes(PPT_1))

ggplot()+
  geom_histogram(data=r_var_df,aes(PPT_1151))
min(r_var_df)

#this is a test

