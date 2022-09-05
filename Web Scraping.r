# -.-.-.-.- OPERAZIONI PRELIMINARI -.-.-.-.-

# Settaggio della working directory:
setwd("C://Users//Andrea//Desktop//Sviluppo Demografico dell'UE")

# Settaggio della lingua:
Sys.setenv(language = "en")

# Installazione librerie:
install.packages("")

# Librerie necessarie:
library(dplyr)
library(ggplot2)
library(ggraph)
library(htmlwidgets)
library(igraph)
library(quanteda)
library(quanteda.textstats)
library(rvest)
library(stopwords)
library(stringr)
library(tidygraph)
library(tidytext)
library(tm)
library(widyr)
library(wordcloud2)

webshot::install_phantomjs()

# Settaggio lingua italiana per lavorare su testi italiani:
quanteda_options(language_stemmer ="italian")
stopwords("it")

# -.-.-.-.- WEB SCRAPING -.-.-.-.- #
translated <- read.csv("CSV//articles.csv", sep = ";", header = TRUE, check.names = FALSE)
head(translated)

urls <- c()

# Loop per recuperare gli articoli degli ultimi 4.5 anni:
for(n in 1:28) {
  
  # URL della pagina in cui fare web scraping:
  cervelli_in_fuga = paste0("https://www.ilfattoquotidiano.it/cervelli-fuga/page/", n, "/")
  
  # Lettura del codice HTML:
  page = read_html(cervelli_in_fuga)
  
  # URL degli articoli:
  link <- page %>%
    html_nodes("h3.p-item") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  urls <- append(urls, link)
  
}

cervelli_in_fuga_df <- data.frame(
  urls = urls
)

articles <- c()

# Loop per recuperare i titoli, il corpo degli articoli e i commenti degli utenti:
for(url in cervelli_in_fuga_df$urls) {
  
  article <- read_html(iconv(url, to = "UTF-8"), encoding =  "utf8") %>%
    html_node("section.article-content") %>%
    html_text()
  
  articles <- append(articles, article)
  
}

cervelli_in_fuga_df$articles <- articles

# Preprocessing dei dati - caratteri speciali, punteggiatura e stop words:

preprcocessed_articles <- cervelli_in_fuga_df$articles %>%
  str_to_lower() %>%
  str_replace_all("[:punct:]", " ")


preprcocessed_articles_tokens <- preprcocessed_articles %>% 
  tokens()

preprcocessed_articles_tokens <- tokens_remove(preprcocessed_articles_tokens, 
                                               stopwords('it'))

preprcocessed_articles_tokens <- preprcocessed_articles_tokens %>%
  str_replace_all("[\\s]", " ")

word_counts <- with(as.data.frame(preprcocessed_articles_tokens), as.data.frame(preprcocessed_articles_tokens)[ preprcocessed_articles_tokens != "" & preprcocessed_articles_tokens != " ", ])

# -.-.-.-.- WORD CLOUD -.-.-.-.- #

word_cloud <- table(word_counts)
word_cloud_filtered <- word_cloud[as.numeric(word_cloud) >= 100]

wc = wordcloud2(data = word_cloud_filtered, size=1, color='#305897', backgroundColor="transparent")

# saveWidget(wc,"plot_23.html", selfcontained = F, background = "transparent")

# -.-.-.-.- CO-OCCURENCY -.-.-.-.- #

co_occurency <- as.data.frame(word_counts)

# Separazione delle parole in sezioni (30 linee):
co_occurency <- co_occurency %>%
  mutate(section = row_number() %/% 30) %>% #30
  filter(section > 0)

# Si considerano solo le parole presenti almeno 50 volte:
co_occurency <- co_occurency %>%
  group_by(word_counts) %>%
  filter(n() >= 50) %>%
  pairwise_cor(word_counts, section, sort = TRUE)

# Grafico:
co_occurency <- co_occurency %>%
  filter(correlation > .1) %>%
  as_tbl_graph() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE, color = "#305897") +
  geom_node_point(color = "lightblue", size = 1) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

co_occurency

ggsave(filename = "plot_24.png",
       plot = co_occurency,
       bg = "transparent",
       width = 2000, height = 2000, units = "px")

# -.-.-.-.- SENTIMENT ANALYSIS -.-.-.-.- #

# Preprocessing dei dati - caratteri speciali, punteggiatura e stop words:

preprcocessed_translated <- as.vector(translated) %>%
  str_to_lower() %>%
  str_replace_all("[:punct:]", " ")

preprcocessed_translated_tokens <- preprcocessed_translated %>% 
  tokens()

preprcocessed_translated_tokens <- tokens_remove(preprcocessed_translated_tokens, 
                                               stopwords('en'))

preprcocessed_translated_tokens <- preprcocessed_translated_tokens %>%
  str_replace_all("[\\s]", " ")

articles_sentiment <- as.data.frame(preprcocessed_translated_tokens)

names(articles_sentiment)[names(articles_sentiment) == 'preprcocessed_translated_tokens'] <- 'word'


palette_sentiment <- c("#F15757", "#EB8E3C", "#5BB177", "#4C3A53", "#F2D68C", "#F29991", "#057FB9", "#00407C", "#8D679D", "#5FBEA6")

group_by(country, species) %>% 
  summarise(numbers = sum(numbers))

# ..... BARPLOT: SENTIMENT PIU' FREQUENTI ..... #

articles_sentiment_first <- articles_sentiment%>%
  count(word) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment)

articles_sentiment_first$word <- NULL

plot_25 <- articles_sentiment_first %>%
  count(sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  ylab(NULL) +
  xlab(NULL) + 
  geom_col(color = "black") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = palette_sentiment) +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, hjust = 1),
    legend.position = "none",
  )

plot_25

ggsave(filename = "plot_25.png",
       plot = plot_25,
       bg = "transparent",
       width = 2500, height = 2000, units = "px")

# ..... BARPLOT: 6 PAROLE PIU' FREQUENTI ..... #

plot_26 <- articles_sentiment%>%
  count(word) %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(6, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment, label = n)) + #hcl.colors(10, "Blues3")
  geom_col(color = "#000000") +
  ylab(NULL) +
  xlab(NULL) + 
  coord_cartesian(ylim = c(0, 500)) +
  theme_minimal() +
  scale_fill_manual(values = palette_sentiment) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, size = 2.5) +
  facet_wrap(~ sentiment, scales = "free", ncol = 5) +
  theme(
    axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    legend.position = "none",
  )

plot_26

ggsave(filename = "plot_26.png",
       plot = plot_26,
       bg = "transparent",
       width = 2000, height = 1500, units = "px")
