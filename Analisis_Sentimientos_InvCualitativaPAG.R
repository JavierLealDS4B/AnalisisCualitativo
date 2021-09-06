
#Prueba Análisisi de Sentimientos en Protocolos Unificados usando "syuzhet"

# Limpiar el workspace, consola y fijar a UTF-8

rm(list = ls())

cat("\014")

options(encoding = "utf-8")

# 1. Instalar y Cargar Librerias

# install.packages("syuzhet") - de no tenerlo

library(tidyverse)
library(tidytext)
library(syuzhet)
library(pdftools)

# 3. cargar el diccionario y la función get_sentiments.R

sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

# 4. Carga del Protocolo

# Ubicar directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Lectura del archivo de Protocolo

protocolo <- pdftools::pdf_text("ProtocoloTotal.pdf")

protocolo[1]

length(protocolo)

head(protocolo)


# 5. Convertirlo en una tibble para poder dividirlo en palabras-token y
#hacer los primeros cálculos.

protocolo_analizar <- tibble(texto = protocolo)

protocolo_analizar <- protocolo_analizar %>%
  unnest_tokens(palabra, texto) %>% #Dividir en palabras Tokens "palabra"
  mutate(pagina = (1:n()) %/% 400 + 1) %>% #Crea un índice, equivalente a 400 palabras/página
  inner_join(get_sentiments("nrc")) %>% #Establecer palabras + y -, y marcar carga emotiva
  count(sentimiento, pagina = pagina) %>% #Recontarlas "sentimiento"
  spread(sentimiento, n, fill = 0) %>% #Generar tabla interna con valencia (NA=0)por cada página
  mutate(negativo = negativo*-1) #Transformar números de columna negativo en números negativos

protocolo_analizar #Ver el resultado


# 6.Sumar a los valores de la variable positivo a los de negativo

puntuacion <- protocolo_analizar %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)

# 7. Graficar los resultados

ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "pink1", fill = "aquamarine3") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Narrativa por Página") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Protocolos Anécdoticos Unificados UNESR-UPTT")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


#8. Visualizar con nitidez la curva de la línea por la que transita la narración
#aqui syuzhet, utiliza la función get_dct_transform()

protocolo_trans <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 10,
                                 #x_reverse_len = nrow(puntuacion),
                                 scale_range = TRUE)

#Nueva tabla que informa a ggplot() a qué página, 
#o segmento, corresponde qué valor

protocolo_trans <- tibble(pagina = seq_along(protocolo_trans),
                      ft = protocolo_trans)

protocolo_trans #Ver el resultado 


# 9. Graficar el resultado anterior "Forma de la historia"
windows ()
c4 = c("UNESR"=rep("aquamarine3", times = 47) , "UPTT"=rep("pink3", times = 53))
df = cbind(df, c4)
ggplot(protocolo_trans, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = c4, fill = c4) +
  theme_minimal() +
  labs(x = "Tiempo narrativo",
       y = "Transformación Valorada del Sentimiento") +
    ggtitle(expression(paste("Forma de la historia: ",
                           italic("Protocolos Anécdoticos Unificados UNESR-UPTT"))))


# Obtener una gráfica sencilla de líneas con los mismos datos
windows()
plot(protocolo_trans,
     type = "l",
     yaxt = 'n',
     ylab = "",
     xlab = "Tiempo narrativo",
     main = "La forma de la historia:\nProtocolos Anécdoticos Unificados")
abline(h = 0.0, col = "red")
