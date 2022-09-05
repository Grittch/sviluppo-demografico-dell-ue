# -.-.-.-.- OPERAZIONI PRELIMINARI -.-.-.-.-

# Settaggio della working directory:
setwd("C://Users//Andrea//Desktop//Sviluppo Demografico dell'UE")

# Settaggio della lingua:
Sys.setenv(language = "en")

# Installazione librerie:
install.packages("")

# Librerie necessarie:
library(cartogram)
library(dplyr)
library(geosphere)
library(ggplot2)
library(giscoR)
library(maps)
library(scales)
library(sf)
library(tidyr)
library(tidyverse)

# Colori:
blu_base = "#305897"

# Notazione:
options(scipen = 10000)

# -.-.-.-.- NATALITA' E MORTALITA' -.-.-.-.-
immigrati_e_emigrati <- read.csv("CSV//immigrati_e_emigrati.csv", sep = ";", header = TRUE, check.names = FALSE)
head(immigrati_e_emigrati)

immigrati_e_emigrati_circle <- read.csv("CSV//immigrati_e_emigrati_circle.csv", sep = ";", header = TRUE, check.names = FALSE)
head(immigrati_e_emigrati_circle)

provenienza_emigrati <- read.csv("CSV//provenienza_emigrati.csv", sep = ";", header = TRUE, check.names = FALSE)
head(provenienza_emigrati)

# --- IMMIGRAZIONE E EMIGRAZIONE: GRAFICI --- #

# ..... CARTOGRAMMA: IMMIGRAZIONE 2020 ..... #

# Creazione mappa base con coordinate geospaziali:
immigrati_e_emigrati_map <- map_data("world")

immigrati_e_emigrati_map <-
  left_join(immigrati_e_emigrati_map, immigrati_e_emigrati[c("region","immigration", "emigration")], by = "region")

# Eliminazione delle righe aventi valori NA:
immigrati_e_emigrati_map <- immigrati_e_emigrati_map %>%
  filter(!is.na(immigrati_e_emigrati_map$"immigration"))

# Intervalli per i colori sulla mappa:
intervalli_immigrati_e_emigrati_map <- c(5000, 10000, 25000, 50000, 100000, 200000, 400000, 600000, 800000)

immigrati_e_emigrati_map$values_cut <- cut(immigrati_e_emigrati_map$"immigration",
                                              breaks = intervalli_immigrati_e_emigrati_map,
                                              dig.lab = 5
)

# Testo legenda:
labs_immigrati_e_emigrati_map <- c("10K", "25K", "50K", "100K", "200K", "400K", "600K", "800K", "1M")

# Palette mappa:
palette_immigrati_e_emigrati_map <- hcl.colors(length(immigrati_e_emigrati_map) - 1, "Blues3", rev = TRUE)

plot_20 <-
  ggplot(immigrati_e_emigrati_map, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = values_cut), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(
    name = "Immigrati",
    values = palette_immigrati_e_emigrati_map,
    labels = labs_immigrati_e_emigrati_map,
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = 0.5,
      keywidth = 2.5,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE,
      label.position = "bottom"
    )
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(
      size = 11,
      color = "grey20"
    ),
    legend.text = element_text(
      size = 10,
      color = "grey20"
    ),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    rect = element_blank()
  )

plot_20

ggsave(filename = "plot_20.png",
       plot = plot_20,
       bg = "transparent", 
       width = 1500, height = 1500, units = "px")

# ..... CARTOGRAMMA: EMIGRAZIONE 2020 ..... #

# Creazione mappa base con coordinate geospaziali:
emigrati_map <- map_data("world")

emigrati_map <-
  left_join(emigrati_map, immigrati_e_emigrati[c("region", "emigration")], by = "region")

# Eliminazione delle righe aventi valori NA:
emigrati_map <- emigrati_map %>%
  filter(!is.na(emigrati_map$"emigration"))

# Intervalli per i colori sulla mappa:
intervalli_emigrati_map <- c(2400, 5000, 10000, 20000, 50000, 75000, 100000, 150000, 200000, 400000, 600000)

emigrati_map$values_cut <- cut(emigrati_map$"emigration",
                                           breaks = intervalli_emigrati_map,
                                           dig.lab = 5
)

# Testo legenda:
labs_emigrati_map <- c("2.5K", "5K", "10K", "25K", "50K", "75K", "100K", "150K", "200K", "400K", "800K")

# Palette mappa:
palette_emigrati_map <- hcl.colors(10, "Blues3", rev = TRUE)

plot_21 <-
  ggplot(emigrati_map, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = values_cut), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(
    name = "Emigrati",
    values = palette_emigrati_map,
    labels = labs_emigrati_map,
    drop = FALSE,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = 0.5,
      keywidth = 2.5,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE,
      label.position = "bottom"
    )
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(
      size = 11,
      color = "grey20"
    ),
    legend.text = element_text(
      size = 10,
      color = "grey20"
    ),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    rect = element_blank()
  )

plot_21

ggsave(filename = "plot_21.png",
       plot = plot_21,
       bg = "transparent", 
       width = 1500, height = 1500, units = "px")

# ..... EMICIRCLE: CONFRONTO I/E ..... #

plot_22 <- ggplot(immigrati_e_emigrati_circle, aes(x=x, y=sqrt(count), fill=type)) +
  geom_col(width = 1, color = "#000000FF") +
  scale_x_continuous(expand = c(0, 0), limits = c(0.5, 2.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_polar(theta = "x", direction = -1) +
  facet_wrap(~region, ncol = 7) +
  scale_fill_manual(values = c("#DDDDDD", blu_base), labels = c('Immigrazione', 'Emigrazione'), ) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top",
  )

plot_22

ggsave(filename = "plot_22.png",
       plot = plot_22,
       bg = "transparent", 
       width = 2000, height = 1500, units = "px")

# ..... GRAFO PROVENIENZA ..... #

# Mappa del mondo:
param <- par(mar = c(0, 0, 0, 0))

mappa <- map("world",
    col = "#cccccc", fill = TRUE, bg = "transparent", lwd = 0.05,
    mar = rep(0, 4), border = 1, ylim = c(-80,80) 
)

europe <- provenienza_emigrati %>%
  filter(provenienza_emigrati$continent == "Europe")
africa <- provenienza_emigrati %>%
  filter(provenienza_emigrati$continent == "Africa")
asia <- provenienza_emigrati %>%
  filter(provenienza_emigrati$continent == "Asia")
america <- provenienza_emigrati %>%
  filter(provenienza_emigrati$continent == "America")
oceania <- provenienza_emigrati %>%
  filter(provenienza_emigrati$continent == "Oceania")



draw_map <- function(x) {
  param
  mappa
  return(
    for(i in 1:nrow(x)){
      inter <- gcIntermediate(c(x$long[i], x$lat[i]),  c(13, 41), n = 50, addStartEnd = TRUE, breakAtDateLine = F)
      
      # Connessioni:
      lines(inter, col = "#305897", lwd = x$inflows_normalized[i] * 5)
      points(x = x$long[i], y = x$lat[i], col = "#305897", cex = 1, pch = 20)
    }
  )
}

draw_map(europe)
draw_map(africa)
draw_map(asia)
draw_map(america)
draw_map(oceania)
