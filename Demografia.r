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
library(OpenImageR)
library(scales)
library(sf)
library(tidyr)
library(tidyverse)
library(treemap)

# Colori:
blu_base = "#305897"

# -.-.-.-.- ANALISI DEMOGRAFICA -.-.-.-.- #
popolazione_al_1_gennaio <- read.csv("CSV//popolazione_al_1_gennaio.csv", sep = ";", header = TRUE, check.names = FALSE)
head(popolazione_al_1_gennaio)

area_degli_stati <- read.csv("CSV//area_degli_stati.csv", sep = ";", header = TRUE, check.names = FALSE)
head(area_degli_stati)

fascia_15_29 <- read.csv("CSV//fascia_15_29.csv", sep = ";", header = TRUE, check.names = FALSE)
head(fascia_15_29)

fasce_eta_italia <- read.csv("CSV//fasce_eta_italia.csv", sep = ";", header = TRUE, check.names = FALSE)
head(fasce_eta_italia)

piramide_delle_eta <- read.csv("CSV//piramide_delle_eta.csv", sep = ";", header = TRUE, check.names = FALSE)
head(piramide_delle_eta)

rateo_m_f <- read.csv("CSV//rateo_m_f.csv", sep = ";", header = TRUE, check.names = FALSE)
head(rateo_m_f)

proiezione_della_popolazione_nel_2100 <- read.csv("CSV//proiezione_della_popolazione_nel_2100.csv", sep = ";", header = TRUE, check.names = FALSE)
head(proiezione_della_popolazione_nel_2100)

# --- ANALISI DEMOGRAFICA: GRAFICI --- #

# ..... POPOLAZIONE UE ..... #
popolazione_UE_2020 = sum(popolazione_al_1_gennaio$"2020")
popolazione_UE_2021 = sum(popolazione_al_1_gennaio$"2021")
popolazione_UE_2022 = sum(popolazione_al_1_gennaio$"2022")

# ..... POPOLAZIONE EUROPA ..... #
popolazione_europa = 745173769

# ..... CARTOGRAMMA: POPOLAZIONE UE 2022 ..... #

# Creazione mappa base con coordinate geospaziali:
popolazione_al_1_gennaio_map <- map_data("world")

popolazione_al_1_gennaio_map <-
  left_join(popolazione_al_1_gennaio_map, popolazione_al_1_gennaio[c("region","2022")], by = "region")

# Eliminazione delle righe aventi valori NA:
popolazione_al_1_gennaio_map <- popolazione_al_1_gennaio_map %>%
  filter(!is.na(popolazione_al_1_gennaio_map$"2022"))

# Intervalli per i colori sulla mappa:
intervalli_popolazione_al_1_gennaio_map <- c(1000000, 5000000, 1000000, 20000000, 40000000, 60000000, 80000000, 100000000)

popolazione_al_1_gennaio_map$values_cut <- cut(popolazione_al_1_gennaio_map$"2022",
                           breaks = intervalli_popolazione_al_1_gennaio_map,
                           dig.lab = 5
)

# Testo legenda:
labs_popolazione_al_1_gennaio_map <- c("1M", "5M", "10M", "20M", "40M", "60M", "80M", "100M")

# Palette mappa:
palette_popolazione_al_1_gennaio_map <- hcl.colors(length(intervalli_popolazione_al_1_gennaio_map) - 1, "Blues3", rev = TRUE)

plot_1 <-
  ggplot(popolazione_al_1_gennaio_map, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = values_cut), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(
    name = "Popolazione (MLN)",
    values = palette_popolazione_al_1_gennaio_map,
    labels = labs_popolazione_al_1_gennaio_map,
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

plot_1

ggsave(filename = "plot_1.png",
       plot = plot_1,
       bg = "transparent",
       width = 1500, height = 1500, units = "px")

# ..... CARTOGRAMMA: DENSITA' ABITATIVA ..... #

area_degli_stati <- area_degli_stati %>%
  left_join(popolazione_al_1_gennaio[c("region", "2022")], by = c("region"), copy = TRUE)

# Calcolo denstità popolativa di uno stato:
area_degli_stati <- area_degli_stati %>%
  mutate(density = `2022` / km2)

area_degli_stati$region[area_degli_stati$region=="UK"] <- "United Kingdom"

# Caricamento della mappa del mondo e selezione degli stati appartenenti all'UE:
country <- gisco_countries

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Republic","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

unione_europea <- subset(country, country$NAME_ENGL %in% europeanUnion)

names(area_degli_stati)[names(area_degli_stati) == 'region'] <- 'NAME_ENGL'

cartogram_data <- unione_europea %>%
  left_join(area_degli_stati, by = c("NAME_ENGL"))

cartogram_data <- st_transform(cartogram_data, 3035)

cartogram_data <- cartogram_cont(cartogram_data,
                                 weight = "density")

# Intervalli per i colori sulla mappa:
intervalli_cartogram_data <- c(0, 75, 150, 300, 500, 1000, 1500, 2000)

cartogram_data$values_cut <- cut(cartogram_data$density,
                                 breaks = intervalli_cartogram_data,
                                 dig.lab = 5
)

# Testo legenda:
labs_cartogram_data <- c("25", "75", "150", "300", "500", "1000", "1500+", "2000")

# Palette mappa:
palette_cartogram_data <- hcl.colors(length(intervalli_cartogram_data) - 1, "Blues3", rev = TRUE)

plot_2 <- ggplot(cartogram_data) +
  geom_sf(aes(fill = values_cut), color = "gray30") +
  theme_void() +
  scale_fill_manual(
    name = "Densità (AB/KM2)",
    values = palette_cartogram_data,
    labels = labs_cartogram_data,
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
  xlim(c(2200000, 7150000)) +
  ylim(c(1380000, 5500000)) +
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

plot_2

ggsave(filename = "plot_2.png",
       plot = plot_2,
       bg = "transparent",
       width = 1500, height = 1500, units = "px")

# ..... LINE PLOT: SERIE STORICA POPOLAZIONE UNIONE EUROPEA ..... #

# Somma della popolazione di anno in anno:
serie_storica <- popolazione_al_1_gennaio %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

serie_storica_pivot_longer <- pivot_longer(serie_storica, paste0(c(seq(1960, 2022, 1))), names_to = "year", values_to = "population")

plot_3 <- serie_storica_pivot_longer %>%
  filter(region == "Total") %>%
  ggplot(., mapping = aes(x = as.numeric(year), y = population)) +
  stat_smooth(aes(x = as.numeric(year), y = population), color = blu_base, method = "lm",
              formula = y ~ poly(x, 21), se = F, size = 1.5) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = 1 )) +
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

plot_3

ggsave(filename = "plot_3.png",
       plot = plot_3,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")

# ..... GRAFICO A BARRE: INDICE DI CRESCITA DEMOGRAFICO ..... #

tasso_di_crescita <- popolazione_al_1_gennaio %>%
  mutate(growth = (((`2022`/`1960`)^(1/62)) - 1) * 100)

rng = diff(range(tasso_di_crescita$growth))

plot_4 <- ggplot(tasso_di_crescita, mapping = aes(x = reorder(region, growth), y = growth, fill=region, label = format(growth, digit = 1, nsmall = 1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = rep(blu_base, 28)) +
  ylab("%") +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = ),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), aes(y = ifelse(growth < 0, growth - 0.03*rng, growth + 0.03*rng)), size = 2.5)

plot_4

ggsave(filename = "plot_4.png",
       plot = plot_4,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")

# ..... LINE PLOT: SERIE STORICA POPOLAZIONE BULGARIA ..... #

plot_5 <- serie_storica_pivot_longer %>%
  filter(region == "Bulgaria") %>%
  ggplot(., mapping = aes(x = as.numeric(year), y = population)) +
  stat_smooth(aes(x = as.numeric(year), y = population), color = blu_base, method = "lm",
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

plot_5

ggsave(filename = "plot_5.png",
       plot = plot_5,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")

# ..... GRAFICO A BARRE: INDICE DI CRESCITA DEMOGRAFICO 2010-2020 ..... #

tasso_di_crescita_2010_2020 <- popolazione_al_1_gennaio %>%
  mutate(growth = (((`2022`/`2012`)^(1/10)) - 1) * 100)

rng = diff(range(tasso_di_crescita_2010_2020$growth))

plot_6 <- ggplot(tasso_di_crescita_2010_2020, mapping = aes(x = reorder(region, growth), y = growth, fill = region, label = format(growth, digit = 1, nsmall = 1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = rep(blu_base, 28)) +
  ylab("%") +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), aes(y = ifelse(growth < 0, growth - 0.03*rng, growth + 0.03*rng)), size = 2.5)

plot_6

ggsave(filename = "plot_6.png",
       plot = plot_6,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")



# ..... CARTOGRAMMA: FASCIA ETA' 15 - 29 ..... #

# Creazione mappa base con coordinate geospaziali:
fascia_15_29_map_data <- map_data("world")

# Popolazione dei paesi solo nel 2021:
popolazione_2021 <- popolazione_al_1_gennaio

popolazione_2021[, c(paste0(seq(1960, 2020, 1)), "2022")] <- NULL

# Creazione percentuale della fascia 15-29:
fascia_15_29 <- fascia_15_29 %>%
  left_join(popolazione_2021)

fascia_15_29 <- fascia_15_29 %>%
  mutate(perc = round((population/`2021`) * 100, 1))

# Dati base per la mappa:
fascia_15_29_map <-
  left_join(fascia_15_29_map_data, fascia_15_29, by = "region")

# Eliminazione delle righe aventi valori NA:
fascia_15_29_map <- fascia_15_29_map %>%
  filter(!is.na(fascia_15_29_map$perc))

# Intervalli per i colori sulla mappa:
intervalli_fascia_15_29_map <- c(14, 15, 16, 17, 18, 19, 20, 21)

fascia_15_29_map$values_cut <- cut(fascia_15_29_map$perc,
                                               breaks = intervalli_fascia_15_29_map,
                                               dig.lab = 5
)

# Testo legenda:
labs_fascia_15_29_map <- prettyNum(intervalli_fascia_15_29_map[-1], big.mark = ",") # c("0.5M", "1M", "5M", "10M", "25M", "50M", "75M", "100M")

# Palette mappa:
palette_fascia_15_29_map <- hcl.colors(length(intervalli_fascia_15_29_map) - 1, "Blues3", rev = TRUE)

plot_7 <-
  ggplot(fascia_15_29_map, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = values_cut), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(
    name = "Fascia 15-29 (%)",
    values = palette_fascia_15_29_map,
    labels = labs_fascia_15_29_map,
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

plot_7

ggsave(filename = "plot_7.png",
       plot = plot_7,
       bg = "transparent",
       width = 1500, height = 1500, units = "px")

# ..... DONUT PLOT: FASCIA ETA' ITALIA ..... #

totale = 59236213

# Percentuali fasce:
fasce_eta_italia <- fasce_eta_italia %>%
  mutate(perc = round(population / totale *100, 1))

# Percentuali cumulative (per posizionare le label):
fasce_eta_italia$ymax <- cumsum(fasce_eta_italia$perc)

fasce_eta_italia$ymin <- c(0, head(fasce_eta_italia$ymax, n=-1))

# Posizione delle label:
fasce_eta_italia$label_position <- ((fasce_eta_italia$ymax + fasce_eta_italia$ymin) / 2)

# Testo label:
fasce_eta_italia$label <- paste0(fasce_eta_italia$age, "\n", fasce_eta_italia$perc, "%")

# Palette:
palette_fasce_eta_italia <- hcl.colors(length(fasce_eta_italia$perc), "Blues3", rev = TRUE)

plot_8 <- ggplot(fasce_eta_italia, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = as.factor(perc))) +
  geom_rect(color = "#000000") +
  geom_text(x = 3.5, aes(y = label_position, label = label), size = 5, color = ifelse(fasce_eta_italia$perc < 18, "#000000", "#ffffff")) +
  scale_fill_manual(values = palette_fasce_eta_italia) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none",
    legend.title = element_text(size = 11),
    strip.text.x = element_text(size = 8),
    strip.text.y = element_text(size = 8)
  )

plot_8

ggsave(filename = "plot_8.png",
       plot = plot_8,
       bg = "transparent",
       width = 1500, height = 1500, units = "px")

# ..... PIRAMIDE DELLE ETA': ITALIA ..... #

piramide_delle_eta$age <- factor(piramide_delle_eta$age, levels = piramide_delle_eta$age)

piramide_delle_eta_pivot_longer <- pivot_longer(piramide_delle_eta, c(male, female), names_to = "gender", values_to = "population")

plot_9 <- ggplot(piramide_delle_eta_pivot_longer, aes(x = age, fill = gender,
                                            y = ifelse(gender == "male",
                                                       yes = -population, no = population))) + 
  geom_bar(stat = "identity", color = "#000000") +
  scale_fill_manual(values = c("#F2ADB7", "#72B7F2")) +
  ylab(NULL) +
  xlab(NULL) + 
  scale_y_continuous(
    labels = c("3 M", "2 M","1 M","0","1 M","2 M","3 M")
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = "none",
  ) +
  coord_flip()

plot_9

ggsave(filename = "plot_9.png",
       plot = plot_9,
       bg = "transparent",
       width = 1500, height = 2000, units = "px")

# ..... CARTOGRAMMA: RATEO M/F ..... #

rateo_m_f_map <- map_data("world")

rateo_m_f <- rateo_m_f %>%
  mutate(rateo = round((female / male)*100, 1))

rateo_m_f[order(rateo_m_f$rateo),]

# Dati base per la mappa:
rateo_m_f <-
  left_join(rateo_m_f_map, rateo_m_f, by = "region")
rateo_m_f <- rateo_m_f %>%
  filter(!is.na(rateo_m_f$rateo))

plot_10 <-
  ggplot(rateo_m_f, aes(x = long, y = lat, group = group)) +
  coord_map(projection = "mercator") +
  geom_polygon(aes(fill = rateo), color = "black") +
  theme_void() +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_gradient2(
    name = "Donne x 100 Uomini",
    low = "#5080A9", mid = "#FFFFFF", high = "#C28A92",
    midpoint = 100,
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
      label.position = "bottom",
      aesthetics = "fill"
    )
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.title = element_text(
      size = 11,
      color = "grey20"
    ),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    axis.ticks = element_blank(),
    rect = element_blank()
  )

plot_10

ggsave(filename = "plot_10.png",
       plot = plot_10,
       bg = "transparent",
       width = 1500, height = 1500, units = "px")

# ..... LINE PLOT: POPOLAZIONE NEL 2100 ..... #

popolazione_2100 <- proiezione_della_popolazione_nel_2100 %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

popolazione_2100_pivot_longer <- pivot_longer(popolazione_2100, paste0(c(seq(2020, 2100, 5))), names_to = "year", values_to = "population")

plot_11 <- popolazione_2100_pivot_longer %>%
  filter(region == "Total") %>%
  ggplot(., aes(x = as.numeric(year), y = population, color = region),) +
  stat_smooth(aes(x = as.numeric(year), y = population), color = blu_base, method = "lm",
              formula = y ~ poly(x, 16), se = F, size = 1.5) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6, accuracy = 1)) +
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


plot_11

ggsave(filename = "plot_11.png",
       plot = plot_11,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")

# ..... BAR PLOT: CRESCITA POPOLAZIONE ..... #

differenza_percentuale <- proiezione_della_popolazione_nel_2100 %>%
  mutate(perc = (`2100` - `2020`) / `2020` * 100)

rng_plot_12 = diff(range(differenza_percentuale$perc))

plot_12 <-
  ggplot(differenza_percentuale, mapping = aes(x = reorder(region, perc), y = perc, fill = region, label = round(perc, 1))) +
  geom_bar(stat = "identity", position = "dodge", color = "#000000FF") +
  theme_minimal() +
  scale_fill_manual(values = rep(blu_base, 28)) +
  ylab("%") +
  xlab(NULL) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "none",
  ) +
  geom_text(position = position_dodge(width = .9), aes(y = ifelse(perc < 0, perc - 0.02*rng_plot_12, perc + 0.02*rng_plot_12)), size = 2.5)

plot_12

ggsave(filename = "plot_12.png",
       plot = plot_12,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")
