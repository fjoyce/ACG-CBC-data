
# script for reading in Elton Trait columns

taxon_elton <- read_csv(here::here("data", "BirdFuncDat.csv"),
                        col_types = list(col_number(), # SpecID
                                         col_character(), # PassNonPass
                                         col_character(), # IOCOrder
                                         col_character(), # BLFamilyLatin
                                         col_character(), # BLFamilyEnglish
                                         col_number(), # BLFamSequID
                                         col_number(), # Taxo
                                         col_character(), # Scientific
                                         col_character(), # English
                                         col_number(), # Diet-Inv
                                         col_number(), # Diet-Vend
                                         col_number(), # Diet-Vect
                                         col_number(), # Diet-Vfish
                                         col_number(), # Diet-Vunk
                                         col_number(), # Diet-Scav
                                         col_number(), # Diet-Fruit
                                         col_number(), # Diet-Nect
                                         col_number(), # Diet-Seed
                                         col_number(), # Diet-PlantO
                                         col_character(), # Diet-5Cat
                                         col_character(), # Diet-Source
                                         col_character(), # Diet-Cert
                                         col_character(), # Diet-Entered
                                         col_number(), # ForStrat-watbelowsurf
                                         col_number(), # ForStrat-wataroundsurf
                                         col_number(), # ForStrat-ground
                                         col_number(), # ForStrat-understory
                                         col_number(), # ForStrat-midhigh
                                         col_number(), # ForStrat-canopy
                                         col_number(), # ForStrat-aerial
                                         col_number(), # PelagicSpecialist
                                         col_character(), # ForStrat-Source
                                         col_number(), # ForStrat-SpecLevel
                                         col_character(), # ForStrat-EnteredBy
                                         col_number(), # Nocturnal
                                         col_number(), # BodyMass-Value
                                         col_character(), # BodyMass-Source
                                         col_number(), # BodyMass-SpecLevel
                                         col_character(), # BodyMass-Comment
                                         col_character() # Record-Comment
                        )) %>% 
  clean_names()