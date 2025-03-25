

# Combinar los raster para obtener el delta
delta_245 <- (ensemble_binary * 10) + ensemble_binary_ssp245
delta_585 <- (ensemble_binary * 10) + ensemble_binary_ssp585

categorias_delta <- c("Consistentemente inadecuado",  # 00
                      "Ganancia de idoneidad",         # 01
                      "Pérdida de idoneidad",          # 10
                      "Consistentemente adecuado")

# # Convertir el raster a factor para asignar etiquetas a cada categoría
# delta_245 <- as.factor(delta_245)
# levels(delta_245) <- data.frame(ID = c(0, 1, 10, 11),
#                                 category = categorias_delta)    # 11
# 
# 
# # Convertir el raster a factor para asignar etiquetas a cada categoría
# delta_585 <- as.factor(delta_585)
# levels(delta_585) <- data.frame(ID = c(0, 1, 10, 11),
#                                 category = categorias_delta)    # 11

# # Visualizar el resultado
# par(mfrow = c(1,2))
# plot(delta_245, main = "Cambio en la idoneidad climática \nPeriodo: 2040-2060, SSP245",
#      col = c("lightgray", "lightgreen", "red", "darkgreen"))
# plot(study_area ,add=TRUE, fill=NA)
# plot(delta_585, main = "Cambio en la idoneidad climática \nPeriodo: 2040-2060, SSP585",
#      col = c("lightgray", "lightgreen", "red", "darkgreen"))
# plot(study_area ,add=TRUE, fill=NA)



# png(paste0(output_specie, tag_spe, "_Delta_Maps.png"),
#     width = 15, height = 11, units="in", res=300)
# par(mfrow = c(1,2))
# 
# # Primer mapa
# plot(delta_245,
#      main = "Cambio en la idoneidad climática \nPeriodo: 2040-2060, SSP245",
#      col = c("lightgray", "lightgreen", "red", "darkgreen"),
#      legend = FALSE)  # Desactivamos la leyenda automática
# plot(study_area, add=TRUE, fill=NA)
# 
# # Agregamos la leyenda con base R
# legend("topright",
#        legend = categorias_delta,
#        fill = c("lightgray", "lightgreen", "red", "darkgreen"),
#        bty = "y",
#        inset = c(0.01, 0.04))  # Sin borde en la caja de leyenda
# 
# # Segundo mapa
# plot(delta_585,
#      main = "Cambio en la idoneidad climática \nPeriodo: 2040-2060, SSP585",
#      col = c("lightgray", "lightgreen", "red", "darkgreen"),
#      legend = FALSE)
# plot(study_area, add=TRUE, fill=NA)
# 
# # Agregamos la leyenda de la misma forma
# legend("topright",
#        legend = categorias_delta,
#        fill = c("lightgray", "lightgreen", "red", "darkgreen"),
#        bty = "y",
#        inset = c(0.01, 0.04))
# 
# dev.off()




# Plot the binary raster using white for 0 and darkgreen for 1
jpeg(paste0(output_specie, tag_spe, "_Ensemble_Maps_2040_2050.jpg"),
     width = 11.7, height = 8.3, units="in", res=300)
par(mfrow = c(1,3))
plot(ensemble_continuous, main = str_to_title(paste0(tag_spe, " - ", "Ensemble Base ")))
#points(points_cleaned, col = "red")
plot(ensemble_continuous_ssp245, main = "Ensemble SSP245 - 2040_2060")
plot(study_area ,add=TRUE, fill=NA)
plot(ensemble_continuous_ssp245, main = "Ensemble SSP585 - 2040_2060")
plot(study_area ,add=TRUE, fill=NA)
dev.off()



# Calcular área de cada valor en delta_245
area_245 <- expanse(delta_245, unit="ha", byValue=TRUE)
# expanse() retorna un data.frame con columnas "value" y "cover"
# Creamos un data.frame con etiquetas descriptivas
df_245 <- data.frame(
  category = categorias_delta,
  value = area_245$value,
  area_ha = area_245$area
)

# Calculamos el porcentaje relativo
df_245$perc <- (df_245$area_ha / sum(df_245$area_ha)) * 100
df_245$scenario <- "SSP245"

# Repetimos para delta_585
area_585 <- expanse(delta_585, unit="ha", byValue=TRUE)
df_585 <- data.frame(
  category = categorias_delta,
  value = area_585$value,
  area_ha = area_585$area
)
df_585$perc <- (df_585$area_ha / sum(df_585$area_ha)) * 100
df_585$scenario <- "SSP585"

# Combinamos en un solo data.frame
df_delta <- rbind(df_245, df_585)

# Nos aseguramos de que las categorías tengan un orden
df_delta$category <- factor(df_delta$category,
                            levels = categorias_delta)



# Calcular perdidas y manten
lost_245   <- area_245$area[area_245$value == 10]  # Pérdida
remain_245 <- area_245$area[area_245$value == 11]  # Mantenimiento

lost_585   <- area_585$area[area_585$value == 10]
remain_585 <- area_585$area[area_585$value == 11]

# Área total originalmente adecuada (Baseline=1) para cada escenario
sum_245 <- lost_245 + remain_245
sum_585 <- lost_585 + remain_585


df_stack <- data.frame(
  scenario = rep(c("SSP245", "SSP585"), each = 2),
  category = rep(c("Mantiene", "Perdida"), 2),
  area_ha  = c(remain_245, lost_245,
               remain_585, lost_585)
)

# Proporción respecto del total de baseline adecuado en cada escenario
df_stack$perc <- c(
  remain_245 / sum_245 * 100,
  lost_245   / sum_245 * 100,
  remain_585 / sum_585 * 100,
  lost_585   / sum_585 * 100
)

# df_stack





# Aseguramos un orden lógico en la categoría (Remain abajo, Lost arriba)
df_stack$category <- factor(df_stack$category, levels = c("Perdida", "Mantiene"))

bar_plot_changes <- ggplot(df_stack, aes(x = scenario, y = perc, fill = category)) +
  geom_bar(stat = "identity") +
  # Capa para mostrar porcentajes dentro de las barras
  geom_text(
    aes(label = paste0(round(perc, 1), "%")),
    position = position_stack(vjust = 0.5),  # Centra el texto en cada barra apilada
    color = "black",                         # Color de texto contrastante
    size = 5                                 # Ajusta el tamaño a tu preferencia
  ) +
  scale_fill_manual(values = c("red", "darkgreen")) +  
  labs(
    x = "",
    y = "Proporción de cambio (%)",
    fill = "Categoría",
    title = "Cambios en la idoneidad climática",
    subtitle = "*En área originalmente adecuada",
    caption = "(Rodriguez-Espinoza et al., 2025)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 12),
    axis.text.y      = element_text(size = 12),
    axis.title.y     = element_text(size = 13),
    legend.position  = "bottom",
    legend.title     = element_text(size = 12),
    legend.text      = element_text(size = 11),
    plot.title       = element_text(size = 12, face = "bold")
  )





####################


jpeg(paste0(output_specie, tag_spe, "_Mapa_final_CC.jpg"),
     width = 13, height = 10, units="in", res=300)

par(mfrow=c(2,3))

# 1) Ensemble base
plot(ensemble_continuous, main = "Ensemble Linea Base: 1970-2000", colNA="white")

# 2) Ensemble futuro SSP245
plot(ensemble_continuous_ssp245, main = "Ensemble SSP245: 2040-2060", colNA="white")

# 3) Ensemble futuro SSP585
plot(ensemble_continuous_ssp245, main = "Ensemble SSP585: 2040-2060", colNA="white")

# 4) Delta map SSP245
plot(delta_245, main = "Delta 2040-2060, SSP245", 
     col = c("lightgray","lightgreen","red","darkgreen"), 
     legend=FALSE)
plot(study_area, add=TRUE, fill=NA)
legend("topright",
       legend = categorias_delta,
       fill = c("lightgray","lightgreen","red","darkgreen"),
       bty="y", cex=0.8, inset = c(0.06, 0.04))

# 5) Delta map SSP585
plot(delta_585, main = "Delta 2040-2060, SSP585",
     col = c("lightgray","lightgreen","red","darkgreen"),
     legend=FALSE)
plot(study_area, add=TRUE, fill=NA)
legend("topright",
       legend = categorias_delta,
       fill = c("lightgray","lightgreen","red","darkgreen"),
       bty="y", cex=0.8, inset = c(0.06, 0.04))



# 6) Aquí dejaremos espacio para la gráfica de barras

plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
print(bar_plot_changes, vp = vp1) 

dev.off()
