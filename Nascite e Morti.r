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
library(ggplot2)
library(giscoR)
library(scales)
library(sf)
library(tidyr)
library(tidyverse)
library(treemap)

# Colori:
blu_base = "#305897"

# Notazione:
options(scipen = 10000)

# -.-.-.-.- NATALITA' E MORTALITA' -.-.-.-.-
numero_dei_nati <- read.csv("CSV//numero_dei_nati.csv", sep = ";", header = TRUE, check.names = FALSE)
head(numero_dei_nati)

numero_dei_morti <- read.csv("CSV//numero_dei_morti.csv", sep = ";", header = TRUE, check.names = FALSE)
head(numero_dei_morti)

tasso_di_fertilita_2020 <- read.csv("CSV//tasso_di_fertilita_2020.csv", sep = ";", header = TRUE, check.names = FALSE)
head(tasso_di_fertilita_2020)

eta_media_primogenito_2020 <- read.csv("CSV//eta_media_primogenito_2020.csv", sep = ";", header = TRUE, check.names = FALSE)
head(eta_media_primogenito_2020)

# --- NATALITA' E MORTALITA': GRAFICI --- #

# ..... NASCITE E MORTI 2020 ..... #
nati_2020 = sum(numero_dei_nati$"2020")
morti_2020 = sum(numero_dei_morti$"2020")

# ..... TREEMAP: NASCITE E MORTI 2020 ..... #

treemap(numero_dei_nati,
        index = "region",
        vSize= paste0(2020),
        type="value",
        vColor = paste0(2020),
        palette = hcl.colors(length(numero_dei_nati$region), "Blues 3"),
        fontsize.title = 0,
        title.legend="",
        title = NA,
        fontsize.labels= 8,
        draw=T,
        
)

treemap(numero_dei_morti,
        index = "region",
        vSize= paste0(2020),
        type="value",
        vColor = paste0(2020),
        palette = hcl.colors(length(numero_dei_nati$region), "Blues 3"),
        fontsize.title = 0,
        title.legend="",
        title = NA,
        fontsize.labels= 8
)

# ..... LINE PLOT: SERIE STORICA ITALIA NASCITE ..... #

numero_nati_pivot_longer <- pivot_longer(numero_dei_nati, paste0(c(1960:2020)), names_to = "year", values_to = "born")

plot_15 <- numero_nati_pivot_longer %>%
  filter(region == "ITA") %>%
  ggplot(., mapping = aes(x = as.numeric(year), y = born)) +
  stat_smooth(aes(x = as.numeric(year), y = born), color = blu_base, method = "lm",
              formula = y ~ poly(x, 21), se = F, size = 1.5) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = 0.1 )) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
  )

plot_15

ggsave(filename = "plot_15.png",
       plot = plot_15,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")

# ..... LINE PLOT: SERIE STORICA ITALIA MORTI ..... #

numero_morti_pivot_longer <- pivot_longer(numero_dei_morti, paste0(c(1960:2020)), names_to = "year", values_to = "death")

plot_16 <- numero_morti_pivot_longer %>%
  filter(region == "ITA") %>%
  ggplot(., mapping = aes(x = as.numeric(year), y = death)) +
  stat_smooth(aes(x = as.numeric(year), y = death), color = blu_base, method = "lm",
              formula = y ~ poly(x, 21), se = F, size = 1.5) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3, accuracy = 1)) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
  )

plot_16

ggsave(filename = "plot_16.png",
       plot = plot_16,
       bg = "transparent", 
       width = 2000, height = 1500, units = "px"
)

# ..... ISTOGRAMMA: DIFFERENZA TRA NATI E MORTI ..... #

numero_morti_pivot_longer <- numero_morti_pivot_longer %>%
  filter(region == "ITA")

plot_17 <- numero_nati_pivot_longer %>%
  filter(region == "ITA") %>%
  left_join(numero_morti_pivot_longer) %>%
  mutate(diff = born - death) %>%
  ggplot(., mapping = aes(x = as.numeric(year), y = diff)) +
  geom_histogram(stat = "identity", color = "#000000FF", fill = blu_base) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  # facet_wrap(Reato~., ncol = 1, strip.position = "top") +
  scale_y_continuous(labels = unit_format(unit = "k", scale = 1e-3, accuracy = 1 )) +
  theme_minimal() +
  # scale_fill_manual(values = cinque_colori) +
  # coord_cartesian(ylim = c(0, 1.08)) +
  xlab(NULL) +
  ylab("%") +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "none",
  )

plot_17

ggsave(filename = "plot_17.png",
       plot = plot_17,
       bg = "transparent", 
       width = 2000, height = 1500, units = "px"
)

# ..... CARTOGRAMMA: TASSO DI FERTILITA' 2020 ..... #

# Creazione mappa base con coordinate geospaziali:
tasso_di_fertilita_2020_map <- map_data("world")

tasso_di_fertilita_2020_map <-
  left_join(tasso_di_fertilita_2020_map, tasso_di_fertilita_2020[c("region","rate")], by = "region")

# Eliminazione delle righe aventi valori NA:
tasso_di_fertilita_2020_map <- tasso_di_fertilita_2020_map %>%
  filter(!is.na(tasso_di_fertilita_2020_map$"rate"))

# Intervalli per i colori sulla mappa:
intervalli_tasso_di_fertilita_2020_map <- c(1.15, 1.25, 1.35, 1.45, 1.55, 1.65, 1.75, 1.85)

tasso_di_fertilita_2020_map$values_cut <- cut(tasso_di_fertilita_2020_map$"rate",
                                               breaks = intervalli_tasso_di_fertilita_2020_map,
                                               dig.lab = 5
)

# Testo legenda:
labs_tasso_di_fertilita_2020_map <- c("1.10", "1.20", "1.30", "1.40", "1.50", "1.60", "1.70+")

# Palette mappa:
palette_tasso_di_fertilita_2020_map <- hcl.colors(length(tasso_di_fertilita_2020_map) - 1, "Blues3", rev = TRUE)

plot_18 <-
  ggplot(tasso_di_fertilita_2020_map, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = values_cut), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(
    name = "Tasso di Fertilità",
    values = palette_tasso_di_fertilita_2020_map,
    labels = labs_tasso_di_fertilita_2020_map,
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

plot_18

ggsave(filename = "plot_18.png",
       plot = plot_18,
       bg = "transparent", 
       width = 1500, height = 1500, units = "px")

# ..... ISTOGRAMMA: ETA' MEDIA PRIMOGENITO 2020 ..... #

plot_19 <- eta_media_primogenito_2020 %>%
  ggplot(., mapping = aes(x = reorder(region, age), y = age, fill = age, label = age)) +
  geom_histogram(stat = "identity", color = "#000000FF", fill = blu_base) +
  theme_minimal() +
  xlab(NULL) +
  ylab(NULL) +
  theme(
    axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 2.5)


plot_19

ggsave(filename = "plot_19.png",
       plot = plot_19,
       bg = "transparent", 
       width = 2000, height = 1000, units = "px")
