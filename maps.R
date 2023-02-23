
#key: AIzaSyA8a28wRDs8Owyf_uExDg6A1WGoBuFaIDs
register_google(key="AIzaSyA8a28wRDs8Owyf_uExDg6A1WGoBuFaIDs")

rm(list=ls())

#Import the libraries

library(ggmap)
library(tmap)

geocode("Madrid")

fuencarral <- geocode('Fuencarral, Madrid, España',
                      source = "google")


map <- get_map(location = c(lon = -3.7025600, lat = 40.4165000),
                      color = "bw",
                      maptype = "toner",
                      scale = 1,
                      zoom = 12,
               source = "stamen")

#Define the functions

haversine <- function(lat1, lat2, lon1, lon2) {
  
  phi_1 = lat1*pi/180
  phi_2 = lat2*pi/180
  R = 6371000  # radius of Earth in meters
  delta_phi = (lat2 - lat1)*pi/180
  delta_lambda = (lon2 - lon1)*pi/180
  
  a = sin(delta_phi / 2.0) ** 2 + cos(phi_1) * cos(phi_2) * sin(delta_lambda / 2.0) ** 2
  
  c = 2 * atan2(sqrt(a), sqrt(1 - a))
  
  meters = R * c  # output distance in meters
  meters
}


df <- data.frame(lon = -3.64527777778, lat = 40.4080555556, est = "moratalaz")
df <- rbind(df, data.frame(lon = -3.71333333333, lat = 40.3472222222, est = "villaverde"))
df <- rbind(df, data.frame(lon = -3.67972222222, lat = 40.3658333333, est = "china"))
df <- rbind(df, data.frame(lon = -3.74027777778, lat = 40.4422222222, est = "acustica"))
df <- rbind(df, data.frame(lon = -3.65805555556, lat = 40.4627777778, est = "hortaleza"))
df <- rbind(df, data.frame(lon = -3.678, lat = 40.411, est = "fuencarral"))
df$type <- "meteo"


df_b <- data.frame(lon = -3.369302, lat = 40.485766, est = "alcala")
df_b <- rbind(df_b, data.frame(lon = -3.652764, lat = 40.542656, est = "alcobendas"))
df_b <- rbind(df_b, data.frame(lon = -3.598581, lat = 40.031749, est = "aranjuez"))
df_b <- rbind(df_b, data.frame(lon = -3.677585, lat = 40.436619, est = "arganzuela"))
df_b <- rbind(df_b, data.frame(lon = -3.565486, lat = 40.426118, est = "coslada"))
df_b <- rbind(df_b, data.frame(lon = -3.689492, lat = 40.440041, est = "etsii"))
df_b <- rbind(df_b, data.frame(lon = -3.726730, lat = 40.445736, est = "farmacia"))
df_b <- rbind(df_b, data.frame(lon = -3.732119, lat = 40.310142, est = "getafe"))
df_b <- rbind(df_b, data.frame(lon = -3.894515, lat = 40.518030, est = "las_rozas"))
df_b <- rbind(df_b, data.frame(lon = -3.768408, lat = 40.326022, est = "leganés"))
df_b <- rbind(df_b, data.frame(lon = -4.000687, lat = 40.655118, est = "villalba"))
df_b$type <- "bacteria"


df_p <- data.frame(lon = -3.682831, lat = 40.542755, est = "alcobendas_polen")
df_p <- rbind(df_p, data.frame(lon = -3.726169, lat = 40.445361, est = "ciudad_universitaria_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.894464, lat = 40.518128, est = "las_rozas_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.598315, lat = 40.031687, est = "aranjuez_polen"))
df_p <- rbind(df_p, data.frame(lon = -4.000709, lat = 40.655019, est = "hospital_collado_villalba_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.369291, lat = 40.485823, est = "alcala_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.764854, lat = 40.327831, est = "leganes_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.732061, lat = 40.310069, est = "getafe_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.565482, lat = 40.426071, est = "coslada_polen"))
df_p <- rbind(df_p, data.frame(lon = -3.677635, lat = 40.436934, est = "salamancapolen"))
df_p$type <- "polen"


df_c <- data.frame(lon = -3.727, lat = 40.455, est = "ciemat")
df_c$type <- "ciemat"


df_noaa <- data.frame(lon = -4.017, lat = 40.800, est = "navacerrada")
df_noaa <- rbind(df_noaa, data.frame(lon = -3.733, lat = 40.650, est = "colmenar"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.717, lat = 40.450, est = "universitaria"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.567, lat = 40.494, est = "barajas"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.583, lat = 40.483, est = "madrid_w_c"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.683, lat = 40.417, est = "retiro"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.785, lat = 40.371, est = "4_vientos"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.724, lat = 40.294, est = "getafe"))
df_noaa <- rbind(df_noaa, data.frame(lon = -3.446, lat = 40.497, est = "torrejon"))
df_noaa$type <- "noaa"


estaciones <- read.csv2("C:/Users/Jose María/Documents/AIRTEC/OBJETIVO_1/Calidad_aire/stations_long.csv", header = TRUE, sep = ',',
                        stringsAsFactors = FALSE)
estaciones <- na.omit(estaciones)


df_a <- data.frame("lon" = as.numeric(), "lat" = as.numeric(), "est" = as.character(), stringsAsFactors = FALSE)

for (i in c(1:length(estaciones$AQISD))) {
  df_a[i,1] = as.numeric(estaciones[i,"LONGITUD_G"])
  df_a[i,2] = as.numeric(estaciones[i,"LATITUD_G"])
  df_a[i,3] = estaciones[i,"NOMBRE"]
}
df_a$type <- "aqs"

df <- rbind(df, df_b, df_p, df_a, df_c, df_noaa)

df[,"lon"] <- as.numeric(df[,"lon"])
df[,"lat"] <- as.numeric(df[,"lat"])

# tmap_mode("plot")


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
#colour = 'red',
#pch = 17,
size = 5) +
  scale_shape_manual(values=c(18, 16, 17, 15, 25, 18))+
  geom_text(data = df,
            aes(label = est),
            hjust = 0,
            vjust = 0) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))



ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(18, 15, 17, 16, 25, 18))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

#Only bacteria


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  color = "green"
),
shape = 16,
data = df[df$type=="bacteria",],
size = 4) +
  scale_colour_manual(values="green") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(20, 15, 20, 20))+
  scale_colour_manual(values=c("red", "green", "blue", "violet"))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

#Only polen

ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  color = "violet"
),
shape = 15,
data = df[df$type=="polen",],
size = 4) +
  scale_colour_manual(values="violet") +
  scale_shape_manual(values=15) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))



ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(20, 20, 20, 15))+
  scale_colour_manual(values=c("red", "green", "blue", "violet"))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

#Only AQ

ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  color = "red"
),
shape = 18,
data = df[df$type=="aqs",],
size = 4) +
  scale_colour_manual(values="red") +
  scale_shape_manual(values=18) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(15, 20, 20, 20))+
  scale_colour_manual(values=c("red", "green", "blue", "violet"))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))


#Only CIEMAT


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  color = "red"
),
shape = 17,
data = df[df$type=="ciemat",],
size = 4) +
  scale_colour_manual(values="blue") +
  scale_shape_manual(values=17) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(20, 20, 15, 20))+
  scale_colour_manual(values=c("red", "green", "blue", "violet"))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

#Only meteo

df <- rbind(df, df_noaa)


ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(18, 16))+
  scale_colour_manual(values=c("red", "green"))+
  geom_text(data = df,
            aes(label = est),
            hjust = 0,
            vjust = 0) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))

#All without meteo and text labels
df <- rbind(df_b, df_p, df_a, df_c)

ggmap(map) + geom_point(aes(
  x = lon,
  y = lat,
  colour = factor(df$type),
  shape = factor(df$type),
  size = 2
),
data = df,
size = 5) +
  scale_shape_manual(values=c(18, 16, 17, 17))+
  scale_colour_manual(values=c("red", "green", "blue", "violet"))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"))



# tmap_mode("view")


#Calculate distances between pollen and aqs stations POR HACER

distances <- data.frame("pollen_est" = as.character(), "aqs" = as.character(), "distance" = as.numeric(), stringsAsFactors = FALSE)

count = 0
for (i in c(1:length(df_p$est))) {
  polen = as.character(df_p$est[i])
  print(polen)
  for (j in c(1:length(df_a$est))) {
    distances[j+count,"pollen_est"] = polen
    distances[j+count,"aqs"] = df_a$est[j]
    distances[j+count,"distance"] = haversine(df_p$lat[i], df_a$lat[j], df_p$lon[i], df_a$lon[j])
  }
  count = length(distances$pollen_est)
}

#Look for stations closer (5000 m)

estations_closest <- distances[distances$distance < 5000,]
