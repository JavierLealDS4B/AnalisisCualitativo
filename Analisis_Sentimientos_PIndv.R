#Prueba Análisis de Sentimientos en Protocolos Individuales"

# Limpiar el workspace, consola y fijar a UTF-8

rm(list = ls())
cat("\014")
options(encoding = "utf-8")

# 1. Cargar Librerias
library(tidyverse)
library(tidytext)
library(pdftools)

# 2. cargar el diccionario y la función get_sentiments.R
sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
#Aplicar la "Función modificada" para obviar la que trae {tidytext} por defecto
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

sentimientos #Ver el Diccionario como Tabla 

# 3. Carga de los Protocolos Individuales

protocolo_1US <- pdftools::pdf_text("ProtocoloSimon01.pdf")

protocolo_1US[1]
length(protocolo_1US)
head(protocolo_1US)

protocolo_2US <- pdftools::pdf_text("ProtocoloSimon02.pdf")

protocolo_2US[1]
length(protocolo_2US)
#head(protocolo_2US)

protocolo_1UP <- pdftools::pdf_text("ProtocoloUPT01.pdf")

protocolo_1UP[1]
length(protocolo_1UP)
#head(protocolo_1UP)

protocolo_2UP <- pdftools::pdf_text("ProtocoloUPT02.pdf")

protocolo_2UP[1]
length(protocolo_2UP)
#head(protocolo_2UP)

# 4. Crear un vector de caracteres con cada protocolo
profesor_col <- c("UNESR01","UNESR02","UPTT01","UPTT02")

# 5. crear una lista, llamada "protocolos", con todos los protocolos anécdoticos indv.
protocolos <- list(protocolo_1US,protocolo_2US,protocolo_1UP,protocolo_2UP)

# 6.  Crear una gran tabla,"conjunto), con todos los protocolos
conjunto <- NULL #Inicializarla

for(i in seq_along(profesor_col)) {
  limpio <- tibble(parrafo = seq_along(protocolos[[i]]),
                   texto = protocolos[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(protocolos = profesor_col[i]) %>%
    select(protocolos, everything())
  conjunto <- bind_rows(conjunto, limpio)
}

# 7. Primeros Análisis
# Convertir la columna de los Profesores en un factor (<fct>).
conjunto$protocolos <- factor(conjunto$protocolos, levels = rev(profesor_col))

# Contabilozar palabras con carga positiva y negativa. Usando el Diccionario "nrc"
conjunto %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

# 8. Visualizar los resultados anteriores por cada Protocolo Individual
windows()
conjunto %>%
  group_by(protocolos) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(protocolos, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo, protocolos = factor(protocolos, levels = profesor_col)) %>%
  ggplot(aes(indice, sentimiento, fill = protocolos)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ protocolos, ncol = 2, scales = "free_x")+
  ggtitle(expression(paste("Sentimiento en ",
       italic("Protocolos Anécdoticos Individuales")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))+ylab("Sentimiento") +
  xlab("Tiempo narrativo")

# 9. Reconocer las Palabras que contribuyen al sentimiento
recuenta_palabras_bing <- conjunto %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE)

recuenta_palabras_bing

# 10. Visualizar la "Contribución al Sentimiento" de las palabras
windows()
recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(25) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribución al sentimiento (Top 25)", x = NULL) +
  coord_flip()

