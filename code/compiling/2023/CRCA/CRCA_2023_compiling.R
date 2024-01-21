#Purpose of this script is to read in individual route species lists, append them in long format, and then pivot to wide.

#We could use a more straightforward approach if everyone used the same excel file, but they did not

library(tidyverse)
library(here)
library(janitor)


#leiva

#note that there is a row at bottom with the total number of species, but readxl seems to drop it

leiva <- readxl::read_xlsx("data/compiling/2023/CRCA/leiva/Cacao list-Katy & Frank-30 Dec 2023.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "leiva") %>% 
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


#nueva zelandia
nueva_zelandia <- read_csv("data/compiling/2023/CRCA/nueva_zelandia/S157901739_observations.csv") %>% 
  rename(nombre_en_ingles = "Species") %>% 
  rename(total = "Count") %>% 
  rename(ruta = "Location") %>% 
  rename(comentarios = "Details") %>% 
  select(c(nombre_en_ingles, ruta, total, comentarios))
  



#quica
quica <- readxl::read_xlsx("data/compiling/2023/CRCA/quica/Cacao_lista_aves_2023_Calixto_Quica.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "quica") %>% 
  filter(nombre_en_ingles != "NA")


#sensoria
sensoria <- readxl::read_xlsx("data/compiling/2023/CRCA/sensoria/Cacao_Sensoria_total.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "sensoria") %>% 
  filter(nombre_en_ingles != "NA")


#combine four routes into one long data frame.
#read in a list that used the full template 
#so that species are ordered taxonomically instead of alphabetically in the compiled list
#then delete duplicte rows (for:
#black phoebe, northern schiffornis, collared trogon
crca_compiled_long <- bind_rows(leiva, estacion_cacao, nueva_zelandia, quica, sensoria) %>% 
  distinct()


crca_compiled_long %>%
  dplyr::group_by(nombre_en_ingles, ruta) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

crca_compiled_wide <- crca_compiled_long %>% 
  pivot_wider(id_cols = nombre_en_ingles, names_from = ruta, values_from = total) %>% 
  group_by(nombre_en_ingles) %>% 
  mutate(total = sum(across(leiva:sensoria), na.rm = TRUE)) %>% 
  ungroup()

crca_observed_totals <- crca_compiled_wide %>% 
  filter(total > 0) 

crca_observed_totals_entry <- crca_observed_totals %>% 
  select(c(nombre_en_ingles,total))
