#Purpose of this script is to read in individual route species lists, append them in long format, and then pivot to wide.

#We could use a more straightforward approach if everyone used the same excel file, but they did not

library(tidyverse)
library(here)
library(janitor)

# Cacao ----
ruta_cacao <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Estación Cacao/Cacao_lista_aves_actualizada_2023_BYoung_2025.xlsx") %>%
                             clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "cacao") %>% 
  filter(nombre_en_ingles != "NA")
                           
#leiva ----
ruta_leiva <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Leiva-Brasilia/Cacao_lista_aves_actualizada_2023-Leiva-Helen, Frank Joyce-30 Dec 2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "leiva") %>% 
  filter(nombre_en_ingles != "NA")

# Nueva Zelandia- Brasilia ----
ruta_nueva_zelandia_brasilia <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Nueva Zelandia-Brasilia/Cacao_lista_aves_actualizada_2023_Albán Jiménez_Céspedes_30_12_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "nz_brasilia") %>% 
  filter(nombre_en_ingles != "NA")

# Oski Lodge ----
ruta_oski <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Oski_Lodge/Cacao_lista_aves_actualizada_OskiLodge.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "oski") %>% 
  filter(nombre_en_ingles != "NA") 

# Pitilla ----

ruta_pitilla <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Pitilla/Formulario_conteo_aves_Pitilla_2025.xlsx", 
                                  sheet = "Lista de especies",
                                  skip = 6) %>%
  clean_names() %>% 
  select(c(number, nombre_en_ingles, comentarios)) %>% 
  rename(total = number) %>%
  mutate(total = as.numeric(total)) %>% 
  mutate(ruta = "pitilla") %>% 
  filter(nombre_en_ingles != "NA") 

ruta_pitilla_pasmompa <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Pitilla/Formulario_conteo_aves_PitillaPasmompa_2025.xlsx",
                                           sheet = "Lista de especies",
                                           skip = 6) %>% 
  clean_names() %>% 
  select(c(number, nombre_en_ingles, comentarios)) %>% 
  rename(total = number) %>%
  mutate(total = as.numeric(total)) %>% 
  mutate(ruta = "pitilla_pasmompa") %>% 
  filter(nombre_en_ingles != "NA") 

# San Cristobal ----
# Luis Fernando & Neyvin Hernandez Cortes

ruta_san_cristobal <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/San Cristobal/CRCA_2025_San_Cristobal_from_ebird_S291235630.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "san_cristobal") %>% 
  filter(nombre_en_ingles != "NA") %>% 
  filter(!is.na(total))

sum(ruta_san_cristobal$total)

# San Gerardo ----
ruta_san_gerardo <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/San_Gerardo/Cacao_lista_aves_actualizada_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "san_gerardo") %>% 
  filter(nombre_en_ingles != "NA") 

# Sensoria ----

ruta_sensoria <- readxl::read_xlsx(path = "data/compiling/2025/CRCA/Sensoria/CRCA_2025_Sensoria_trip_report_export_edited.xlsx",
                                    sheet = "Sheet1") %>%
  clean_names() %>% 
  filter(!is.na(scientific_name)) %>% 
  #select(c(species, sum)) %>%
  mutate(ruta = "sensoria",
         comentarios = NA,
         total = sum) %>%
    rename(nombre_en_ingles = species) %>% 
  select(nombre_en_ingles, total, comentarios, ruta)

                           

# combine routes ----
crca_compiled_long <- bind_rows(ruta_cacao, 
                                ruta_leiva, 
                                ruta_nueva_zelandia_brasilia, 
                                ruta_oski, 
                                ruta_pitilla, 
                                ruta_pitilla_pasmompa,
                                ruta_san_cristobal,
                                ruta_san_gerardo,
                                ruta_sensoria) %>% 
  #only need this if there are accidentally duplicated rows...
  #distinct() %>% 
    filter(total >= 1)

#then check for duplicate rows
crca_compiled_long %>%
  #group by name and route (to find duplicate rows)
  dplyr::group_by(nombre_en_ingles, ruta) %>%
  #summarize by sample size (i.e. number of records)
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # filter by groups with more than one row (1L indicates integer instead of double?)
  dplyr::filter(n > 1L)    

#good, no duplicates


#pivot wide
crca_compiled_wide <- crca_compiled_long %>% 
  pivot_wider(id_cols = nombre_en_ingles, names_from = ruta, values_from = total, values_fill = 0) %>% 
  group_by(nombre_en_ingles) %>% 
  mutate(total = sum(across(cacao:san_gerardo), na.rm = TRUE)) %>% 
  ungroup()
