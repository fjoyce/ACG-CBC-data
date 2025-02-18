#Purpose of this script is to read in individual route species lists, append them in long format, and then pivot to wide.

#We could use a more straightforward approach if everyone used the same excel file, but they did not

library(tidyverse)
library(here)
library(janitor)

#Los Angeles- San Gerardo (María Marta)
san_gerardo <- readxl::read_xlsx("data/compiling/2024/CRCA/CA-MMC-CBC_ACG_2024 Cacao Los Angeles a San Gerardo lista aves.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "los_angeles_san_gerardo") %>% 
  filter(nombre_en_ingles != "NA")


#estación cacao (Bruce)
# also see this eBird checklist by Freddy Quesada: https://ebird.org/checklist/S207190868

# Nelson Espinoza Mora (https://ebird.org/checklist/S207233562) and Erick Picado Zeledón (https://ebird.org/checklist/S207233776)
#submitted separate ebird checklists, supposedly from Sector Góngora
#Erick submitted only counts of 1 (i.e., presence/absence), and this is his only ebird checklist ever

#note this includes two additional species: golden-browed chlorophonia and turquoise-browed motmot
estacion_cacao <- readxl::read_xlsx("data/compiling/2024/CRCA/lista_de_aves_conteo_ACG_Cacao_2024_ Bruce Y_Estacion Biological Cacao.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "EB_Cacao") %>% 
  filter(nombre_en_ingles != "NA")
  

# Nuevo Mundo (Albán)
#not sure why number of observations is 415 instead of 417...
nuevo_mundo <- readxl::read_xlsx("data/compiling/2024/CRCA/CA-Alban Jimenez-lista_de_aves_conteo_ACG_Cacao_Albán_Jiménez 30-12-2024.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "Nuevo_Mundo") %>% 
  filter(nombre_en_ingles != "NA")

#Pitilla (Calixto)

#Calixto just shared an ebird checklist https://ebird.org/checklist/S207215635 

pitilla <- readxl::read_xlsx("data/compiling/2024/CRCA/Pitilla_2024-12-31_ebird_checklist_entered_by_fhj.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "Pitilla") %>% 
  filter(nombre_en_ingles != "NA")


#combine four routes into one long data frame.
#read in a list that used the full template 
#so that species are ordered taxonomically instead of alphabetically in the compiled list



crca_compiled_long <- bind_rows(estacion_cacao, nuevo_mundo, pitilla, san_gerardo) %>% 
  distinct() %>% 
  filter(total >= 1)

#then check for duplicate rows
crca_compiled_long %>%
  #group by name and route (to find duplicate rows)
  dplyr::group_by(nombre_en_ingles, ruta) %>%
  #summarize by sample size (i.e. number of records)
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # filter by groups with more than one row (1L indicates integer instead of double?)
  dplyr::filter(n > 1L) 

#bicolored antbird and collared trogon were duplicated on the blank data sheets, but no real duplicated species records within routes

#pivot wide
crca_compiled_wide <- crca_compiled_long %>% 
  pivot_wider(id_cols = nombre_en_ingles, names_from = ruta, values_from = total) %>% 
  group_by(nombre_en_ingles) %>% 
  mutate(total = sum(across(EB_Cacao:los_angeles_san_gerardo), na.rm = TRUE)) %>% 
  ungroup()

#already did this step so shouldn't change anything
crca_observed_totals <- crca_compiled_wide %>% 
  filter(total > 0) 

#select only two columns for for entry
crca_observed_totals_entry <- crca_observed_totals %>% 
  select(c(nombre_en_ingles,total))


#write wide data to csv
write_csv(crca_observed_totals, "data/compiling/2024/CRCA/submitted/CRCA_CBC_2024_route_lists.csv")
