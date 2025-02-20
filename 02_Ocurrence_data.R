# Species Distribution Modelling
# 02 _ Download occurrencies data, cleaning and sampling
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025


## Especies ----

# # Caqueta
# cacao <- c("theobroma", "cacao")
# canangucha <- c("mauritia", "flexuosa")
# copoazu <- c("theobroma", "grandiflorum")
# 
# # Choco
# coco <- c("cocos", "nucifera")
# vainilla <- c("vanilla", "planifolia")
# c("Vanilla", "rivasii")
# c("Vanilla", "cribbiana")
# c("Vanilla", "trigonocarpa")
# c("Vanilla", "dresslerii")


## Descarga de datos GBIF ----

datos_gbif_crop <- sp_occurrence(especie[1], species=especie[2], ext = ext_sel)

write_csv(datos_gbif_crop, file = "data/descarga_gbif.csv")

datos_gbif_crop <- vect(datos_gbif_crop, geom=c("lon", "lat"), 
                        crs=crs(all_predictors[["elevation"]]), keepgeom = TRUE) %>% st_as_sf()
  

tag_esp <- paste(especie, collapse = " ") %>% str_to_title()

par(mfrow = c(1,1))
plot(all_predictors[["elevation"]], 
     main = paste0("Ocurrencia Especies GBIF - ", 
                   tag_esp), col = map.pal("elevation"))
lines(limites_sel)
points(datos_gbif_crop, col='red', cex= 1)
legend("bottom", #x = -73.5, y = 3, # Coordinates
       legend = tag_esp,
       col = c('red'))

## Data cleaning ----

# searching for records with longitudes of zero.
lonzero <-  subset(datos_gbif_crop %>% st_as_sf(), lon==0)

if(nrow(lonzero) > 0) {message("Hay latitudes cero en los datos")}


# which records are duplicates (only for the first 10 columns)?
dups <- duplicated(lonzero)

if(sum(dups) > 0) {message("Existen datos duplicados")}

# differentiating by (sub) species
# dups2 <- duplicated(acgeo[, c('species', 'lon', 'lat')])
# ignoring (sub) species and other naming variation
dups2 <- duplicated(datos_gbif_crop[, c('species', 'lon', 'lat')])
# number of duplicates

if(sum(dups2) > 0) {message(paste0("Existen ", sum(dups2), " datos duplicados"))}

# keep the records that are _not_ duplicated
data_cleaned <- datos_gbif_crop[!dups, ] 
data_cleaned <- datos_gbif_crop[!dups2, ] 

print(paste(nrow(data_cleaned), "Datos de ocurrencias de GBIF", tag_esp, "guardados en", "data folder"))
write_csv(data_cleaned, "data/gbif_cleaned_data.csv")










