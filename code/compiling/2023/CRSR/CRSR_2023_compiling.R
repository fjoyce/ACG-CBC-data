#Purpose of this script is to read in individual route species lists, append them in long format, and then pivot to wide.

#We could use a more straightforward approach if everyone used the same excel file, but they did not

library(tidyverse)
library(here)
library(janitor)


#read in Garrigues data
airstrip_Garrigues <- readxl::read_xlsx("data/compiling/2023/CRSR/airstrip_interamericana/Santa_Rosa_lista_aves_actualizada_2023 - Garrigues.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "campo_aterrizaje_interamericana") %>% 
  filter(nombre_en_ingles != "NA")

#read in Albán's Argelia data

argelia <- readxl::read_xlsx("data/compiling/2023/CRSR/argelia/Santa_Rosa_lista_aves_actualizada_2023 Argelia Albán Jiménez.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "argelia") %>% 
  filter(nombre_en_ingles != "NA")

#read in cafetal arriba
cafetal_arriba <- readxl::read_xlsx("data/compiling/2023/CRSR/cafetal_arriba/Santa_Rosa_bird_data_Cafetal_arriba_2023.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "cafetal_arriba") %>% 
  filter(nombre_en_ingles != "NA")

#read in cafetal lejos
cafetal_lejos <- readxl::read_xlsx("data/compiling/2023/CRSR/cafetal_lejos/Santa Rosa-far Cafetal - 28 Dec 2023-KCV-FJJ.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "cafetal_lejos") %>% 
  filter(nombre_en_ingles != "NA")

#read in Cangrejal_Aguacaliente

aguacaliente <- readxl::read_xlsx("data/compiling/2023/CRSR/cangrejal_aguacaliente/Manglar El Cangrejal - Aguacaliente  CBC aves 28dic23.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "aguacaliente") %>% 
  filter(nombre_en_ingles != "NA")


#Casona - Sendero Universal
casona <- readxl::read_xlsx("data/compiling/2023/CRSR/casona_sendero_universal/Santa_Rosa_lista_aves_actualizada_2023 (1).xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "casona") %>% 
  filter(nombre_en_ingles != "NA")


#Estero Real
estero_real <- readxl::read_xlsx("data/compiling/2023/CRSR/estero_real/Cacao_lista_aves_actualizada_2023_Ángel_Solís.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "estero_real") %>% 
  filter(nombre_en_ingles != "NA")

#Junquillal- Federico used separate workflow...

junquillal <- readxl::read_xlsx("data/compiling/2023/CRSR/junquillal/Conteo Santa Rosa 2023 Junquillal.xlsx", 
                                sheet = "Totales Junquillal", col_names = c("Especies", "Cantidad (Sum)")) %>% 
  filter(Especies != "Table 1 Pivot") %>%
  filter(Especies != "Especies") %>% 
  filter(Especies != "Grand Total") %>% 
  mutate(ruta = "junquillal") %>% 
  rename(nombre_en_ingles = "Especies") %>% 
  rename(total = "Cantidad (Sum)") %>% 
  separate_wider_delim(col = nombre_en_ingles, delim = " (", names = c("nombre_en_ingles", "scientific_name")) %>% 
  select(-scientific_name) %>% 
  mutate(total = as.numeric(total))

           
#Laguna escondida
laguna_escondida <- readxl::read_xlsx("data/compiling/2023/CRSR/laguna_escondida/Santa_Rosa_lista_aves_actualizada_2023.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "laguna_escondida") %>% 
  filter(nombre_en_ingles != "NA")


#Murcielago - Parcelas

murcielago_parcelas <- readxl::read_xlsx("data/compiling/2023/CRSR/murciélago_parcelas/Santa_Rosa_Murcielago_total.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "murcielago_parcelas") %>% 
  filter(nombre_en_ingles != "NA")


#nancite
nancite <- readxl::read_xlsx("data/compiling/2023/CRSR/nancite/Santa_Rosa_lista_aves_actualizada_2023 -  Nancite.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "nancite") %>% 
  filter(nombre_en_ingles != "NA")

#Santa Rosa- área administrativa

sr_area_admin <- readxl::read_xlsx("data/compiling/2023/CRSR/santa_rosa_area_administrativa/Administración SR-_lista_aves_actualizada_2023.xlsx") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "sr_area_admin") %>% 
  filter(nombre_en_ingles != "NA")


#Santa Rosa- siempre verde
sr_siempre_verde <- readxl::read_xls("data/compiling/2023/CRSR/siempre_verde/Santa_Rosa_lista_aves_actualizada_2023 (1).xls") %>% 
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "sr_siempre_verde") %>% 
  filter(nombre_en_ingles != "NA")




#append routes (long dataframe)

compiled_long <- bind_rows(aguacaliente,
          airstrip_Garrigues, 
          argelia, 
          cafetal_arriba, 
          cafetal_lejos, 
          casona, 
          estero_real,
          junquillal,
          laguna_escondida,
          murcielago_parcelas,
          nancite, 
          sr_area_admin,
          sr_siempre_verde) %>% 
  #delete duplicated house wren rows
  distinct()

compiled_wide <- compiled_long %>% 
  pivot_wider(id_cols = nombre_en_ingles, names_from = ruta, values_from = total) %>% 
  group_by(nombre_en_ingles) %>% 
  mutate(total = sum(across(aguacaliente:sr_siempre_verde), na.rm = TRUE)) %>% 
  ungroup()


#create list of observed-only species, for ease of entry into Audubon database

observed_totals <- compiled_wide %>% 
  filter(total > 0) 

observed_totals_entry <- observed_totals %>% 
  select(c(nombre_en_ingles,total))

# saving observed_totals as RDS

# saveRDS(observed_totals, file = here("data", "compiling", "2023", "CRSR", "observed_totals.RDS"))

write_csv(observed_totals, "SR_CBC_2023_route_lists.csv")
