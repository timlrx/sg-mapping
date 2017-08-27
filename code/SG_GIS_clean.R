library(rgdal) #use readOGR to read in .shp files 
library(rgeos)
library(raster)
library(maptools)
library(rmapshaper)
library(tmap)
library(tmaptools)
library(reshape2)
library(dplyr)
library(viridis)

## Map polygon (contains shape, islands) downloaded from: 
## https://geo.data.gov.sg/singaporemap-polygon/2017/06/22/shp/singaporemap-polygon.zip
## Line polygon (contains road data and contour) downloaded from: 
## https://geo.data.gov.sg/singaporemap-line/2017/06/20/shp/singaporemap-line.zip
## 2014 Master Plan, Planning Area Boundary: 
## https://geo.data.gov.sg/mp14-plng-area-web-pl/2014/12/05/shp/mp14-plng-area-web-pl.zip

sg_shape <- readOGR(dsn="data/GIS", layer="SingaporeMap_Polygon")
sg_road <- readOGR(dsn="data/GIS", layer="SingaporeMap_Line")
sg_plan <- readOGR(dsn="data/GIS", layer="MP14_PLNG_AREA_WEB_PL")
proj4string(sg_shape)
proj4string(sg_road)
proj4string(sg_plan)
names(sg_shape)
names(sg_road)
names(sg_plan)

unique(sg_shape$FOLDERPATH)
# [1] Layers/SMap_Anno1                  Layers/SMap_PedraBranca_Inset_Anno
# [3] Layers/SMap_Anno2                  Layers/Hydrographic_Anno          
# [5] Layers/PB_draw_frame               Layers/Hydrographic               
# [7] Layers/Airport_Runway              Layers/Central_Business_District  
# [9] Layers/Parks_NaturalReserve        Layers/Coastal_Outline     

unique(sg_road$FOLDERPATH)
# [1] Layers/Expressway        Layers/Major_Road        Layers/International_bdy
# [4] Layers/C_Line

exclude <- c("PEDRA BRANCA", "JOHOR")
sg_shape_main <- sg_shape[(sg_shape$FOLDERPATH %in% c("Layers/Coastal_Outline")) &
                          !grepl(paste(exclude,collapse="|"), sg_shape$NAME),]

plot(sg_shape_main)

roads <- sg_road[(sg_road$FOLDERPATH %in% c("Layers/Expressway",
                                            "Layers/Major_Road")),]
plot(roads)

temp2 <- sg_plan[(sg_plan$PLN_AREA_N %in% c("DOWNTOWN CORE","SINGAPORE RIVE",
                                            "MARINA SOUTH", "MARINA EAST","MARINE PARADE")),]

### Combine region mapping with actual Singapore map
sg <- intersect(buffer(sg_plan, dissolve=FALSE), as(sg_shape_main, 'SpatialPolygons')) # Remove data from main
sg <- unionSpatialPolygons(sg, IDs=as.factor(sg$PLN_AREA_N))
### Append the data back
sg <- SpatialPolygonsDataFrame(sg, sg_plan@data, match.ID="PLN_AREA_N")

## Difference in current map and URA plan
tm_shape(sg_shape_main) +
  tm_polygons(col="yellow") +
  tm_shape(sg_plan)+
  tm_polygons(col="blue", alpha=0.5)

### Read in data to plot
ghs2015_ethnic <- read.csv("data/GHS2015/resident_pop_ethnic_age/Level1.csv",
                           stringsAsFactors=FALSE)
ghs2015_ethnic$Level_3 <- toupper(trimws(sub("- Total","",ghs2015_ethnic$Level_3)))
ghs2015_ethnic$Level_2 <- trimws(ghs2015_ethnic$Level_2)
ghs2015_ethnic$Level_1 <- trimws(ghs2015_ethnic$Level_1)
ghs2015_ethnic$Value <- as.numeric(ghs2015_ethnic$Value)
ghs2015_ethnic_total <- ghs2015_ethnic %>%
  filter(Level_2=="Total" & Level_1=="Total") %>%
  select(Level_3, Value) %>%
  rename(Total = Value) 
  
ghs2015_ethnic <- ghs2015_ethnic %>%
  filter(Level_2=="Total") %>%
  filter(Level_1!="Total") %>%
  left_join(ghs2015_ethnic_total, by="Level_3") %>%
  mutate(prop = Value/Total * 100)

### Merge in survey data
ghs2015_ethnic_ch <- filter(ghs2015_ethnic, Level_1=="Chinese") %>% arrange(prop)
sg_ethnic_ch <- append_data(sg, ghs2015_ethnic_ch, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_ethnic_my <- filter(ghs2015_ethnic, Level_1=="Malays")
sg_ethnic_my <- append_data(sg, ghs2015_ethnic_my, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_ethnic_oth <- filter(ghs2015_ethnic, Level_1=="Others")
sg_ethnic_oth <- append_data(sg, ghs2015_ethnic_oth, key.shp="PLN_AREA_N",key.data="Level_3")


tm_shape(sg_ethnic_oth) +
  tm_polygons("prop", palette=viridis(5), colorNA="light grey")

### Read in religion data
ghs2015_religion <- read.csv("data/GHS2015/resident_pop_religion/Level2.csv",
                           stringsAsFactors=FALSE)
ghs2015_religion$Level_3 <- toupper(trimws(ghs2015_religion$Level_3))
ghs2015_religion$Level_2 <- trimws(ghs2015_religion$Level_2)
ghs2015_religion$Level_1 <- trimws(ghs2015_religion$Level_1)
ghs2015_religion$Value <- as.numeric(ghs2015_religion$Value)
ghs2015_religion_total <- ghs2015_religion %>%
  filter(Level_1=="Total") %>%
  select(Level_3, Value) %>%
  rename(Total = Value) 

ghs2015_religion <- ghs2015_religion %>%
  filter(Level_1!="Total") %>%
  left_join(ghs2015_religion_total, by="Level_3") %>%
  mutate(prop = Value/Total * 100)

### Religion data
ghs2015_religion_is <- filter(ghs2015_religion, Level_1=="Islam") %>% arrange(prop)
sg_religion_is <- append_data(sg, ghs2015_religion_is, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_religion_cat <- filter(ghs2015_religion, Level_1=="Christianity- Catholic" ) %>% arrange(prop)
sg_religion_cat <- append_data(sg, ghs2015_religion_cat, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_religion_chr <- filter(ghs2015_religion, Level_1=="Christianity- Other Christians") %>% arrange(prop)
sg_religion_chr <- append_data(sg, ghs2015_religion_chr, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_religion_bud <- filter(ghs2015_religion, Level_1=="Buddhism") %>% arrange(prop)
sg_religion_bud <- append_data(sg, ghs2015_religion_bud, key.shp="PLN_AREA_N",key.data="Level_3")

ghs2015_religion_tao <- filter(ghs2015_religion, Level_1=="Taoism") %>% arrange(prop)
sg_religion_tao <- append_data(sg, ghs2015_religion_tao, key.shp="PLN_AREA_N",key.data="Level_3")


tm_shape(sg_religion_tao) +
  tm_polygons("prop", palette=viridis(6), colorNA="light grey")


#  tm_text("PLN_AREA_N", size="AREA", scale=0.7, root=8, size.lowerbound = .4,
#        just=c("center","center"),
#        auto.placement = 1, legend.size.show = FALSE) +
  

fb_plot_2010 <- tm_shape(fb_map_2010) +
  tm_polygons("fb_share_2010", title="Percent",
              breaks = c(0, 2, 4, 6, 10, 15, 25, Inf), palette=viridis(7),
              lwd = 0.8) +
  tm_shape(us_shape_state) +
  tm_borders(col = "black", lwd = 0.8) +
  tm_legend(position=c("right","center"), legend.outside=TRUE) +
  tm_layout(main.title = "Foreign-born share in 2010",
            main.title.size = 1.3, frame = FALSE) 
