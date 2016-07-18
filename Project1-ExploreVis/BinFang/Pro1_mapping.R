install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
# some standard map packages.
install.packages(c("maps", "mapdata"))
devtools::install_github("dkahle/ggmap")

setwd("/Users/binfang/Documents/Data_Academy/project/Project_1/matlab_codes")
mapping = readMat("mapping.mat")
mapping = data.frame(mapping)

########################################################################################
mapping_data = data.frame(Longitude = mapping$lst.data.1, Latitude = mapping$lst.data.2, 
                          Temperature = mapping$lst.data.5)
us_states <- map_data("state")
g1 = ggplot(data=mapping_data, aes(x=Longitude,y=Latitude,fill=Temperature))
g1 + geom_tile()+ geom_polygon(data=us_states,aes(x=long, y=lat, group=group), 
colour="black", fill="white", alpha=0)+ scale_fill_gradientn(colours = topo.colors(10)) +
labs(title="Temperature Anomalies(1998)")  

ggsave("lst_98.pdf",width = 25, height = 12.5, units = "cm")


########################################################################################
mapping_data = data.frame(Longitude = mapping$pcp.data.1, Latitude = mapping$pcp.data.2, 
                          Rainfall = mapping$pcp.data.5)
us_states <- map_data("state")
g1 = ggplot(data=mapping_data, aes(x=Longitude,y=Latitude,fill=Rainfall))
g1 + geom_tile()+ geom_polygon(data=us_states,aes(x=long, y=lat, group=group), 
                               colour="black", fill="white", alpha=0)+ 
  scale_fill_gradientn(colours = rev(topo.colors(10))) +
  labs(title="Rainfall Anomalies(1998)")  

ggsave("rain_98.pdf",width = 25, height = 12.5, units = "cm")

########################################################################################
mapping_data = data.frame(Longitude = mapping$sc.data.1, Latitude = mapping$sc.data.2, 
                          Snow_Cover = mapping$sc.data.5)
us_states <- map_data("state")
g1 = ggplot(data=mapping_data, aes(x=Longitude,y=Latitude,fill=Snow_Cover))
g1 + geom_tile()+ geom_polygon(data=us_states,aes(x=long, y=lat, group=group), 
                               colour="black", fill="white", alpha=0)+ 
  scale_fill_gradientn(colours = cm.colors(10)) +
  labs(title="Snow Cover Anomalies(1998)")  

ggsave("snow_98.pdf",width = 25, height = 12.5, units = "cm")

########################################################################################
mapping_data = data.frame(Longitude = mapping$sm.data.1, Latitude = mapping$sm.data.2, 
                          Soil_Wetness = mapping$sm.data.5)
us_states <- map_data("state")
g1 = ggplot(data=mapping_data, aes(x=Longitude,y=Latitude,fill=Soil_Wetness))
g1 + geom_tile()+ geom_polygon(data=us_states,aes(x=long, y=lat, group=group), 
                               colour="black", fill="white", alpha=0)+ 
  scale_fill_gradientn(colours = rev(topo.colors(10))) +
  labs(title="Soil Wetness Anomalies(1998)")  

ggsave("sm_98.pdf",width = 25, height = 12.5, units = "cm")

########################################################################################
mapping_data = data.frame(Longitude = mapping$veg.data.1, Latitude = mapping$veg.data.2, 
                          Veg_Transpiration = mapping$veg.data.5)
us_states <- map_data("state")
g1 = ggplot(data=mapping_data, aes(x=Longitude,y=Latitude,fill=Veg_Transpiration))
g1 + geom_tile()+ geom_polygon(data=us_states,aes(x=long, y=lat, group=group), 
                               colour="black", fill="white", alpha=0)+ 
  scale_fill_gradientn(colours = rev(topo.colors(10))) +
  labs(title="Vegetation Transpiration Anomalies(1998)")  

ggsave("veg.pdf",width = 25, height = 12.5, units = "cm")