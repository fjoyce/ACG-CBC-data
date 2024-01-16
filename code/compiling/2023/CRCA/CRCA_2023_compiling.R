#Purpose of this script is to read in individual route species lists, append them in long format, and then pivot to wide.

#We could use a more straightforward approach if everyone used the same excel file, but they did not

library(tidyverse)
library(here)
library(janitor)



#sensoria
sensoria <- readxl::read_xlsx("data/compiling/2023/CRCA/sensoria/Cacao_Sensoria_total.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "sensoria") %>% 
  filter(nombre_en_ingles != "NA")


#estación cacao

estacion_cacao <- readxl::read_xlsx("data/compiling/2023/CRCA/estación_cacao/Conteo Cacao 2023 - Ruta Cacao.xlsx", 
                                sheet = "Totales Cacao", col_names = c("Especies", "Conteo (Sum)")) %>% 
  filter(! Especies %in% c("Totales Ruta Cacao", "Especies", "Grand Total")) %>% 
  mutate(ruta = "estacion_cacao") %>% 
  rename(nombre_en_ingles = "Especies") %>% 
  rename(total = "Conteo (Sum)") %>% 
  separate_wider_delim(col = nombre_en_ingles, delim = " (", names = c("nombre_en_ingles", "scientific_name")) %>% 
  select(-scientific_name) %>% 
  mutate(total = as.numeric(total))

crca_compiled_long <- bind_rows(sensoria, estacion_cacao)
