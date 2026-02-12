packages <-c('terra','sf','ggplot2','tidyterra','dplyr','tidyr','gifski','gganimate')

for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

setwd("C:/Users/sfield3/OneDrive - University of Wyoming/FIELD LAB HOME/PROJECT/CLIMATE_VARIABILITY_REDUX")

theme_set(theme_bw())


#### READ IN STUDY EXTENT AND RASTERS
study_extent <- st_read("./DATA/study_extent.shp")%>%
  st_transform(.,crs=4269)

## read in raster data
#r <- terra::rast("./DATA/CLIMATE/PPT.tif")
r_var <- terra::rast("./DATA/CLIMATE/PPT_annualvariability.tif")


## calculate changes year to year
# r_var <- (subset(r, 1:(nlyr(r)-1)) - subset(r, 2:nlyr(r))) * -1
# writeRaster(r_var,"./DATA/CLIMATE/PPT_annualvariability.tif")

#clipr <- ext(c(xmin=-107.6,xmax=-107.45,ymin=35.25,ymax=35.4))
clipr <- ext(c(xmin=-108,xmax=-107,ymin=35,ymax=36))
r_var <- crop(r_var,clipr)

r_var1 <- r_var[[6:13]]

df <- as.data.frame(r_var1,xy=T)%>%
  pivot_longer(cols = -c(x, y), names_to = "layer",values_to = "value")%>%
  mutate(layer_num = as.numeric(gsub("PPT_", "", layer)))

p <- ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#bb3e03","#ca6702","#ee9b00","#FEFAE040","#94d2bd","#0a9396","#005f73"),
                       rescaler = ~ scales::rescale_mid(.x, mid = 0),
                       name = "Precipitation 
Variability (mm)")+
  #coord_equal() +
  labs(title = "Year (CE): {frame_time}",
       x=NULL,
       y=NULL) +
  transition_time(layer_num) +
  ease_aes("linear")+
  theme_minimal()

animate(p, fps = 1,nframes = 7, width = 750, height = 600)

anim_save("./FIGURES/raster_precipvariability_600to1300.gif")



## example plots
 
# ggplot(r_df,aes(x=x,y=y))+
#   geom_raster(aes(fill=PPT_1))+
#   geom_text(aes(label=round(PPT_1)),col="white")+
#   scale_fill_viridis_c(direction= -1)+
#   labs(title="Year 1")+
#   theme(legend.position = "none")
# 
# 
# ggplot(r_df,aes(x=x,y=y))+
#   geom_raster(aes(fill=PPT_2))+
#   geom_text(aes(label=round(PPT_2)),col="white")+
#   scale_fill_viridis_c(direction= -1)+
#   labs(title="Year 2")+
#   theme(legend.position = "none")
# 
# 
# ggplot(r_var_df,aes(x=x,y=y))+
#   geom_raster(aes(fill=PPT_1))+
#   geom_text(aes(label=round(PPT_1)),col="white")+
#   scale_fill_viridis_c(direction= -1)+ 
#   labs(title="Difference from Year 1 to Year 2")+
#   theme(legend.position = "none")





################################################################################
## Read in sites
### Read in Chaco Great House Database and merge with Chaco great house outlier metadata
chaco_greathouses <- st_read("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/VanDykeEtAl2015_WGS84.shp")%>%
  st_transform(.,crs=4269)%>%
  st_intersection(.,study_extent)

outlier_database <-read.csv("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/OutlierAttributes_Public_CRA.csv")%>%
  select("Site.name","Site.number.","Est..great.house.start.date.","Est..great.house.end.date.")%>%
  setNames(c("SITE_NA","SITE_NUMBE","BEGIN_DATE","END_DATE"))

gh <- merge(chaco_greathouses,outlier_database,by="SITE_NA")%>%
  filter(!is.na(BEGIN_DATE))%>%
  filter(!is.na(END_DATE))%>%
  subset(BEGIN_DATE != "No Data")%>%
  subset(END_DATE != "No Data")%>%
  select(-c("SITE_NUMBE.x","SITE_NUM_1","PROJECTED_","PROJECTE_1","PROJECTE_2"))%>%
  mutate(END_DATE = as.numeric(END_DATE),
         BEGIN_DATE = as.numeric(BEGIN_DATE),
         occupation_length = END_DATE - BEGIN_DATE)%>%
  subset(occupation_length >= 1)

rm(chaco_greathouses,outlier_database)


### 





### Read in VEP II North site locations
vepii_sites<-read.csv("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/db_vepii_n.csv")%>%
  st_as_sf(.,coords=c("UTMEast","UTMNorth"),crs=26912)%>%
  st_transform(.,crs=4269)%>%
  st_intersection(.,study_extent)%>%
  mutate(UNIQ_ID = 1:nrow(.))

vep_df <- read.csv("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/db_vepii_n.csv")%>%
  select(c("ph6","ph7","ph8","ph9","ph10","ph11","ph12",
           "ph13","ph14","ph15","ph16","ph17","ph18","ph19"))%>%
  mutate(UNIQ_ID = 1:nrow(.))%>%
  gather(phase,value,-UNIQ_ID)%>%
  mutate(value = ifelse(value<0,0,value))

vep_dates <- read.csv("./DATA/VEP_dates.csv")

vep <- left_join(vep_df,vep_dates,c("phase"="PHASE"))

rm(vep_dates)

vep<- arrange(vep,desc(UNIQ_ID),BEGIN_D)

expanded_vep <- vep %>%
  rowwise() %>%
  mutate(year = list(BEGIN_D:END_DAT)) %>%
  unnest(year) %>%
  select(UNIQ_ID, year, value) 

vep_years <- expanded_vep%>%
  group_by(UNIQ_ID)%>%
  mutate(year_until = find_years_until_disoccupation(value))%>%
  setNames(c("site_name","Year","occupation","years_until_disoccupation"))

length(unique(vep_years$site_name))


vep_detail <- merge(vep_years,vep_df,by=c("site_name","Year"))

vep_detail <- distinct(vep_detail)







### Read in Dendrochronological data 
# data shared by Tim on TDAR 3/26/2025
dendro_dates <- read.csv("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/BOCINSKY_ET_AL_2015_TREE_RINGS.csv")%>%
  st_as_sf(.,coords=c("LON","LAT"),crs=4269)%>%
  st_intersection(.,study_extent)%>%
  subset(.,Outer_Symbol == "V"|Outer_Symbol=="RLB"|Outer_Symbol=="R"|Outer_Symbol=="+B"|Outer_Symbol=="++G"|Outer_Symbol=="RG"
         |Outer_Symbol=="+V"|Outer_Symbol=="L"|Outer_Symbol=="+RLB"|Outer_Symbol=="CB"|Outer_Symbol=="+L"
         |Outer_Symbol=="+LB"|Outer_Symbol=="LGB"|Outer_Symbol=="C"|Outer_Symbol=="CL"|Outer_Symbol=="+R"
         |Outer_Symbol=="+CL"|Outer_Symbol=="CGB"|Outer_Symbol=="RLG"|Outer_Symbol=="GB"|Outer_Symbol=="VL"
         |Outer_Symbol=="GLB"|Outer_Symbol=="+GLB"|Outer_Symbol=="BB"|Outer_Symbol=="+LG"|Outer_Symbol=="+RGB"
         |Outer_Symbol=="CLB"|Outer_Symbol=="c"|Outer_Symbol=="r"|Outer_Symbol=="cB"|Outer_Symbol=="+r"
         |Outer_Symbol=="rB"|Outer_Symbol=="+VL"|Outer_Symbol=="CG"|Outer_Symbol=="+CG"|Outer_Symbol=="RC"
         |Outer_Symbol=="LB "|Outer_Symbol=="rGB"|Outer_Symbol== "RB COM"|Outer_Symbol== "R COMP"|Outer_Symbol== "R INC")%>%
  subset(.,!is.na(Site_Number))


### Read in CC locations (check with donna first Donna)


# ggplot()+
#   geom_sf(data=gh,col="#2a9d8f")+
#   geom_sf(data=dendro_dates,col="#e9c46a")+
#   geom_sf(data=vepii_sites,col="#e76f51",alpha=0.01)+
#   geom_sf(data=study_extent,fill=NA)

# st_write(gh, "./OUTPUT/chaco_greathouses.shp")
# st_write(dendro_dates, "./OUTPUT/dendro_data.shp")
# st_write(vepii_sites, "./OUTPUT/vepii_north_data.shp")


### get occupation timelines for sites
find_years_until_disoccupation <- function(x) {
  n <- length(x)
  out <- integer(n)
  counter <- 0
  for (i in seq(n, 1)) {
    if (x[i] == 0) {
      counter <- 0
    } else {
      counter <- counter + 1
    }
    out[i] <- counter
  }
  out
}












r_df <- as.data.frame(r,xy=T)
r_var_df <- as.data.frame(r_var,xy=T)

### pull summary stats together
mean_change <- r_var_df%>%
  select(-c("x","y"))%>%
  summarise_all(mean,na.rm=T)%>%
  t()%>%
  as.data.frame()%>%
  mutate(year=2:2000)%>%
  setNames(c("precip_mean_change","year"))

std_change <- r_var_df%>%
  select(-c("x","y"))%>%
  summarise_all(sd,na.rm=T)%>%
  t()%>%
  as.data.frame()%>%
  mutate(year=2:2000)%>%
  setNames(c("precip_std_change","year"))

absolute_change <- abs(r_var_df)%>%
  select(-c("x","y"))%>%
  summarise_all(mean,na.rm=T)%>%
  t()%>%
  as.data.frame()%>%
  mutate(year=2:2000)%>%
  setNames(c("precip_absolutemean_change","year"))

# NOTE: in most places all of the change is either negative or positive, so 
# usually absolute change is the additive inverse. However, in some places where 
# some cells change positively and some negatively, the absolute change is different.
# 
summary <- merge(mean_change,std_change,by="year")%>%
  merge(.,absolute_change,by="year")%>%
  gather(.,key="variable",value="value",-year)

#plot amount of variability across time
ggplot(data=summary,aes(year,value,color=variable))+
  geom_line(alpha=0.5)+
  geom_smooth(method="loess",span=0.05,se=F)+
  scale_color_manual(values=c("#2a9d8f","#e9c46a","#e76f51"))+
  scale_x_continuous(limits=c(750,1300))+
  facet_wrap(~variable,ncol=1)












