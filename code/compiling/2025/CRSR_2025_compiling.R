library(tidyverse)
library(here)
library(janitor)

# administración (Bruce) ----

ruta_administracion <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Administración/Santa_Rosa_lista_aves_actualizada_BYoung_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "administracion",
         comentarios = as.character(comentarios)) %>% 
  filter(nombre_en_ingles != "NA")


#species list
sp_list_crsr <- ruta_administracion %>% 
  select(nombre_en_ingles)

ruta_agua_caliente <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Agua Caliente- Río Cuajiniquil/Santa_Rosa_lista_aves_ruta_Agua_Caliente_28_12_2025_Alban_Jimenez.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "agua_caliente",
         comentarios = as.character(comentarios)) %>%  
  filter(nombre_en_ingles != "NA")


# argelia ----

ruta_argelia <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Argelia/Santa_Rosa_lista_aves_ASolis_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "argelia",
         comentarios = as.character(comentarios)) %>% 
  filter(nombre_en_ingles != "NA")

# cafetal cerca ----
ruta_cafetal_cerca <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Cafetal Cerca/CRSR_CBC_2025_Cafetal_Cerca_data_entry.xlsx",
                                        sheet = "Sheet5") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total)) %>% 
  mutate(ruta = "cafetal_cerca", 
         comentarios = NA) %>% 
  filter(nombre_en_ingles != "NA") %>% 
  filter(nombre_en_ingles != "UNID raptor") %>% 
  filter(nombre_en_ingles != "(blank)") %>% 
  filter(nombre_en_ingles != "Grand Total")

#read in trip report (incomplete, but check for higher counts)

cafetal_ebird <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Cafetal Cerca/CRSR_2025_Cafetal_trip_report_clean.xlsx") %>% 
  clean_names() %>% 
  select(scientific_name, species, sum) %>% 
  rename(ebird_sum = sum)

#check for differences between lists

chk_cafetal <- ruta_cafetal_cerca %>% 
  full_join(cafetal_ebird, by = join_by(nombre_en_ingles == species)) %>% 
  mutate(adjustment = case_when(
    ebird_sum > total ~ "yes",
    .default = "no"
  )) %>% 
  mutate(total = case_when(
    nombre_en_ingles == "Black Vulture" ~ ebird_sum,
    nombre_en_ingles == "Gartered Violaceous Trogon" ~ ebird_sum,
    nombre_en_ingles == "Lesser Greenlet" ~ ebird_sum,
    nombre_en_ingles == "Orange-chinned Parakeet" ~ ebird_sum,
    nombre_en_ingles == "Ruddy Woodcreeper" ~ ebird_sum,
    nombre_en_ingles == "Streak-headed Woodcreeper" ~ ebird_sum,
    nombre_en_ingles == "Cabanis's Wren" ~ ebird_sum,
    nombre_en_ingles == "Northern Beardless-Tyrannulet" ~ ebird_sum,
    nombre_en_ingles == "Bright-rumped Attila" ~ ebird_sum,
    .default = total
  )) %>% 
  select(nombre_en_ingles:comentarios)


# Junquillal

ruta_junquillal_norte <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Junquillal/Santa_Rosa_lista_aves_actualizada_2026 Junquillal Norte.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "junquillal_norte",
         comentarios = as.character(comentarios)) %>% 
  filter(nombre_en_ingles != "NA")

ruta_junquillal_sur <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Junquillal/Santa_Rosa_lista_aves_actualizada_2026 Junquillal Sur.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "junquillal_sur",
         comentarios = as.character(comentarios)) %>%
  filter(nombre_en_ingles != "NA")

# Los Patos ----

#Note that some species were recorded with just X for presence (no abundance).
# Conservatively assigned abundance of 2


ruta_los_patos <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Los_Patos/Santa_Rosa_lista_aves_actualizada_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "los_patos",
         comentarios = as.character(comentarios)) %>%  
  filter(nombre_en_ingles != "NA") %>% 
  mutate(estimated = case_when(
    total == "X" ~ "yes",
    .default = "no"
  ),
  total = case_when(
    total == "X" ~ 2,
    .default = as.numeric(total)
  )) %>% 
  select(-estimated)

# manglar cuajiniquil ----
ruta_manglar_cuajil <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Manglar_Cuajiniquil/Santa_Rosa_lista_aves_actualizada_CuajilMangrove.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "manglar_cuajiniquil",
         comentarios = as.character(comentarios)) %>%
  filter(nombre_en_ingles != "NA")

# murcielago_cuajil ----
ruta_murcielago_cuajil <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Murciélago-Cuajiniquil/Santa_Rosa_Cuajiniquil-Murcielago-Garrigues_2025.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "murcielago_cuajiniquil",
         comentarios = as.character(comentarios)) %>% 
  filter(nombre_en_ingles != "NA")

# murcielago- parcelas ----
ruta_murcielago_parcelas <- readxl::read_xlsx(path = "data/compiling/2025/CRSR/Murcielago-Parcelas/lista/Santa_Rosa_lista_aves_CRCA_2025_parcelas.xlsx") %>%
  clean_names() %>% 
  select(c(nombre_en_ingles, total, comentarios)) %>% 
  mutate(ruta = "murcielago_parcelas",
         comentarios = as.character(comentarios)) %>% 
  filter(nombre_en_ingles != "NA")

# combine routes ----
crsr_compiled_long <- bind_rows(ruta_administracion,
                                ruta_agua_caliente,
                                ruta_argelia,
                                chk_cafetal,
                                ruta_junquillal_norte,
                                ruta_junquillal_sur,
                                ruta_los_patos,
                                ruta_manglar_cuajil,
                                ruta_murcielago_cuajil,
                                ruta_murcielago_parcelas
                                ) %>%  
#need this because of accidentally duplicated rows (e.g. for house wren)
distinct() %>% 
filter(total >= 1) %>% 
  #consolidate names
  mutate(nombre_en_ingles = case_when(
    nombre_en_ingles == "Common Squirrel-Cuckoo" ~ "Squirrel Cuckoo",
    .default = nombre_en_ingles
  ))

# (check for duplicate rows)
crsr_compiled_long %>%
  #group by name and route (to find duplicate rows)
  dplyr::group_by(nombre_en_ingles, ruta) %>%
  #summarize by sample size (i.e. number of records)
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # filter by groups with more than one row (1L indicates integer instead of double?)
  dplyr::filter(n > 1L) 

#pivot wide
crsr_2025_compiled_wide <- crsr_compiled_long %>% 
  pivot_wider(id_cols = nombre_en_ingles, names_from = ruta, values_from = total, values_fill = 0) %>% 
  group_by(nombre_en_ingles) %>% 
  mutate(total = sum(across(administracion:murcielago_parcelas), na.rm = TRUE)) %>% 
  ungroup()

crsr_2025_for_entry <- sp_list_crsr %>%
  full_join(crsr_2025_compiled_wide) %>% 
  filter(!is.na(total)) %>% 
  select(nombre_en_ingles, total)



