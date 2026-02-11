packages <-c('terra','sf','ggplot2','tidyterra','dplyr','tidyr','gifski','gganimate')

for(p in packages) if(p %in% rownames(installed.packages()) == F) { install.packages(p) }
for(p in packages) suppressPackageStartupMessages(library(p,quietly=T,character.only=T))

setwd("C:/Users/sfield3/OneDrive - University of Wyoming/FIELD LAB HOME/PROJECT/CLIMATE_VARIABILITY_REDUX")

theme_set(theme_bw())


#### READ IN STUDY EXTENT AND RASTERS
study_extent <- st_read("./DATA/study_extent.shp")%>%
  st_transform(.,crs=4269)

## read in raster data
r <- terra::rast("./DATA/CLIMATE/PPT.tif")
r_var <- terra::rast("./DATA/CLIMATE/PPT_annualvariability.tif")

#clipr <- ext(c(xmin=-107.6,xmax=-107.45,ymin=35.25,ymax=35.4))
#clipr <- ext(c(xmin=-10,xmax=-107,ymin=35,ymax=36))
#r <- crop(r,clipr)

## calculate changes year to year
# r_var <- (subset(r, 1:(nlyr(r)-1)) - subset(r, 2:nlyr(r))) * -1
# writeRaster(r,"./DATA/CLIMATE/PPT_annualvariability.tif")

# r_var1 <- r_var[[600:1300]]
# 
# df <- as.data.frame(r_var1,xy=T)%>%
#   pivot_longer(cols = -c(x, y), names_to = "layer",values_to = "value")%>%
#   mutate(layer_num = as.numeric(gsub("PPT_", "", layer)))
# 
# p <- ggplot(df, aes(x = x, y = y, fill = value)) +
#   geom_raster() +
#   scale_fill_gradientn(colors=c("#bb3e03","#ca6702","#ee9b00","#FEFAE040","#94d2bd","#0a9396","#005f73"),rescaler = ~ scales::rescale_mid(.x, mid = 0))+
#   coord_equal() +
#   theme_minimal() +
#   labs(title = "Time Step: {frame_time}") +
#   transition_time(layer_num) +
#   ease_aes("linear")
# 
# animate(p, fps = 5,nframes = 10, width = 600, height = 600)
# 
# anim_save("./FIGURES/raster_precipvariability_600to1300.gif")



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
  select(-c("SITE_NUMBE.x","SITE_NUM_1","PROJECTED_","PROJECTE_1","PROJECTE_2"))


### Read in VEP II North site locations
vepii_sites<-read.csv("C:/Users/sfield3/Documents/RESEARCH/DATA GENERAL/SENSITIVE SITE LOCATIONS/db_vepii_n.csv")%>%
  st_as_sf(.,coords=c("UTMEast","UTMNorth"),crs=26912)%>%
  st_transform(.,crs=4269)%>%
  st_intersection(.,study_extent)

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


############# GREAT HOUSE STUFF #####################
gh_output <- read.csv("./OUTPUT/summaries/chaco_greathouses/chaco_greathouse_1.csv")%>%
  #mutate(Mean_precip_variab = abs(Mean_precip_variab))%>%
  select(-c("X"))

gh <- st_read("./OUTPUT/spatial_outputs/chaco_greathouses.shp")%>%
  as.data.frame()%>%
  mutate(END_DAT = as.numeric(END_DAT))%>%
  mutate(BEGIN_D = as.numeric(BEGIN_D))%>%
  mutate(occupation_length = END_DAT - BEGIN_D)%>%
  subset(occupation_length >= 1)



########## look at variability in years before end of occupation
for (j in 1:nrow(gh)){
  start_year <- as.numeric(gh[j,c("BEGIN_D")])
  end_year <- as.numeric(gh[j,c("END_DAT")])
  site_na <- gh[j,c("UNIQU")]
  
  if(j==1){
    years <- as.data.frame(start_year:end_year)%>%
      mutate(site_name = site_na)%>%
      set_colnames(c("Year","site_name"))%>%
      mutate(years_before_abandonment = end_year-Year)
  }else {
    years_new <- as.data.frame(start_year:end_year)%>%
      mutate(site_name = site_na)%>%
      set_colnames(c("Year","site_name"))%>%
      mutate(years_before_abandonment = end_year-Year)
    years<-rbind(years,years_new)
  }
}
rm(years_new)

gh_summary <- merge(gh_output,years,by=c("Year","site_name"))

write.csv(gh_summary,"./OUTPUT/summaries/chaco_greathouses/gh_detail.csv")




################### VEP STUFF ###############################
#### bind all data together
for(i in 1:10){
  df<-read.csv(paste("./OUTPUT/summaries/vep/vep_",i,".csv",sep=""))
  name<-paste("vep_",i,sep="")
  assign(name,df)
}

vep_df <- rbind(vep_1,vep_2,vep_3,vep_4,vep_5,vep_6,vep_7,
                vep_8,vep_9,vep_10)%>%
  select(-c("X"))


#write.csv(vep_df,"./OUTPUT/summaries/vep/vep_total_recon.csv")

vep <- st_read("./OUTPUT/spatial_outputs/vepii_north_data.shp")%>%
  as.data.frame()%>%
  select(c("UNIQU","ph6","ph7","ph8","ph9","ph10","ph11","ph12",
           "ph13","ph14","ph15","ph16","ph17","ph18","ph19"))%>%
  gather(phase,value,-UNIQU)%>%
  mutate(value = ifelse(value<0,0,value))

vep_dates <- read.csv("./DATA/SETTLEMENT/VEP_dates.csv")

vep <- left_join(vep,vep_dates,c("phase"="PHASE"))

rm(vep_dates)

vep<- arrange(vep,desc(UNIQU),BEGIN_D)

expanded_vep <- vep %>%
  rowwise() %>%
  mutate(year = list(BEGIN_D:END_DAT)) %>%
  unnest(year) %>%
  select(UNIQU, year, value) 

vep_years <- expanded_vep%>%
  group_by(UNIQU)%>%
  mutate(year_until = find_years_until_disoccupation(value))%>%
  set_colnames(c("site_name","Year","occupation","years_until_disoccupation"))

length(unique(vep_years$site_name))


vep_detail <- merge(vep_years,vep_df,by=c("site_name","Year"))

vep_detail <- distinct(vep_detail)

write.csv(vep_detail,"./OUTPUT/summaries/vep/vep_detail.csv")











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












