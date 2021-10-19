# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)
library(grid)
library(png)
library(ggrepel)
# Cargamos data
Bol_dep    <- getData('GADM', country='Bolivia', level=1) %>%st_as_sf() 
Bolivia    <- getData('GADM', country='Bolivia', level=0) %>%st_as_sf() 
Boliv         <- getData('GADM', country='Bolivia', level=3) %>%st_as_sf() 
SurAmerica     <- st_read ("SHP/SurAmerica.shp")  
SurAmeric      <- st_transform(SurAmerica,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Maruripi       <- st_read ("SHP/Bolivia/Maruripi.shp") 
Area           <- shapefile("SHP/Bolivia/Maruripi.shp")
band4 <- raster("Raster/Bolivia/reserva/LC08_L1TP_002068_20210923_20211003_02_T1_B6.tif")
band3 <- raster("Raster/Bolivia/reserva/LC08_L1TP_002068_20210923_20211003_02_T1_B5.tif")
band2 <- raster("Raster/Bolivia/reserva/LC08_L1TP_002068_20210923_20211003_02_T1_B2.tif")

# Combinancion de bandas agricultura
Sentinel_Natu = stack(band4, band3, band2)
##### A nivel de estudio 
ambito <- mapedit::drawFeatures()       # Creamos el objeto
ambito <- ambito %>% st_as_sf()         # Convertimos el objeto sf_ee

Poligonox  <-spTransform(Area, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify
#cortar con la zona de estudio
paute17n  <- spTransform(Area , CRS=crs(band4))
bandas1   <- crop(Sentinel_Natu , extent(paute17n))
bandas    <- mask(bandas1,paute17n)
ventana   = extent(550000, 570000,-1360000, -1340000)
ventana1  = extent(550000, 570000,-1330000, -1360000)
G1=ggRGB(bandas, r=1,g=2,b=3, stretch="lin")+
  theme_void()+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")

G5=ggRGB(bandas, r=1,g=2,b=3, stretch="lin", ext = ventana)+
  theme_void()

G6=ggRGB(bandas, r=1,g=2,b=3, stretch="lin", ext = ventana1)+
  theme_void()

SU=ggplot()+
  geom_sf(data = SurAmeric , fill="white", color="black", size=0.2)+
  geom_sf(data = Bolivia, fill="black", size=0.2)+
  theme_void()
SU.grob <- ggplotGrob(SU)


G3=ggplot()+
  geom_sf(data = Bol_dep, fill="white", color="black", size=0.5)+
  geom_sf_text(data = st_as_sf(Bol_dep), aes(label =  NAME_1), size = 2,family="serif") +
  geom_sf(data = Maruripi, fill="black")+
  theme_void()+
  annotation_custom(SU.grob, xmin = -60, xmax = -57, ymin =-14, ymax=-10)

# Mapa final
library(cowplot)
im=ggdraw() +
  coord_equal(xlim = c(0, 18), ylim = c(0, 10), expand = FALSE) +
  draw_plot(G3, width = 5, height = 5,x = 13, y = 5)+
  draw_plot(G1, width = 11, height = 11,x = -0.2, y = -0.8)+
  draw_plot(G5, width = 5, height = 5,x = 13, y = 0.05)+
  draw_plot(G6, width = 5, height = 5,x = 8, y = 0.05)+
  theme(panel.background = element_rect(fill = "white"))+
  annotate(geom = "text", x = 5, y = 8, label = "Reserva Nacional de Vida Silvestre \nAmazonica Manuripi", 
           family="serif", color = "black", size = 3.5,fontface = "bold")+
  annotate(geom = "text", x = 7, y = 0.5, label = "Mapa de Ubicacion de parcelas de muestreo \nal norte del Dpar, de la Paz", 
           family="serif", color = "black", size = 3,fontface = "bold")


# Exportacion
ggsave(plot = im ,"MAPAS/Bol_area.png",
       units = "cm", width = 21,height = 10, dpi = 900)# guardar grafico  



