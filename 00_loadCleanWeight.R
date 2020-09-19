
# Loading packages

## Sourcing utils functions
source("utils.R")

## Creating list of packages to load
package_list <- c("oliviercecchi/butteR", "ElliottMess/hypegrammaR","ellieallien/cleaninginspectoR","hunzikp/rtree", "impact-initiatives/koboloops",
                  "tidyr","dplyr", "ggplot2", "readr", "stringr", "lubridate", "readxl", "rgdal", "sf", "purrr")

## Running the packaging loading function
loadInstall_package(package_list)


# Loading necessary data
## Loading raw_data

raw_data_csv <- "data/bfa2002_msna_2020_final_cleaning_20200913_PH_MF.csv"

raw_data_info_menage_csv <- "data/bfa2002_msna_2020_final_cleaning_20200913_PH_MF_info_menage.csv"

raw_data_excel <- "data/bfa2002_msna_2020_final_cleaning_20200913_PH_MF.xlsx"


#Loading data and cleaning data and questionnaire
raw_data <- read_csv(raw_data_csv)%>%
  select(-`personne m2`)%>%
  mutate(status = case_when(
      group_pop %in% c("pdi") ~ "pdi",
      group_pop %in% c("pop_local", "migrant_burkina", "rapatrie", "refugie", "migrant_int", "retourne") ~ "host",
      TRUE ~ NA_character_
      )
    )%>%
  rename("risque_fem/enlevements" ="risque_fem/enlèvements",
         "risque_hom/enlevements" = "risque_hom/enlèvements",
         "risque_fille/enlevements" = "risque_fille/enlèvements",
         "risque_garcon/enlevements" = "risque_garcon/enlèvements")%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\n")))%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\r")))%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed(";")))

names(raw_data) <- gsub("\\/", "\\.", names(raw_data))

survey <- read_excel("data/reach_bfa_msna_outil_V2.xlsx", sheet = "survey")%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\n")))%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\r")))%>%
  mutate(name = str_replace_all(name, pattern = fixed("è"), replacement = "e"))

write_csv(survey,"data/survey.csv")

choices <- read_excel("data/reach_bfa_msna_outil_V2.xlsx", sheet = "choices")%>%
  mutate(name = str_replace_all(name, pattern = "[è|é]", replacement = "e"))%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\n")))%>%
  mutate(across(where(is.character), str_remove_all, pattern = fixed("\r")))

write_csv(choices,"data/choices.csv")

### Loading repeat loops
raw_data_info_menage <- read_csv(raw_data_info_menage_csv)%>%
  rename(submission_uuid = `_submission__uuid`,
         parent_index = `_parent_index`)

raw_data_maladie_moins_5ans_rpt <- read_excel(raw_data_excel, sheet = "maladie_moins_5ans_rpt")%>%
  rename(submission_uuid = `_submission__uuid...2`,
         parent_index = `_parent_index...1`)

raw_data_naissances <- read_excel(raw_data_excel, sheet = "naissances")%>%
  rename(parent_index = `_parent_index...1`,
         submission_uuid = `_submission__id`)

raw_data_membre_marche_dificile <- read_excel(raw_data_excel, sheet = "membre_marche_dificile")
# %>%
#   rename(submission_uuid = `_submission__uuid...2`,
#          parent_index = `_parent_index...1`)

raw_data_membre_soins_difficile <- read_excel(raw_data_excel, sheet = "soins_difficile")

raw_data_membre_concentration_difficile <- read_excel(raw_data_excel, sheet = "concentration_difficile")

raw_data_membre_membre_vision_diffcile <- read_excel(raw_data_excel, sheet = "membre_vision_diffcile")
# %>%
#   rename(submission_uuid = `_submission__uuid...13`,
#          parent_index = `_parent_index...1`)

raw_data_membre_membre_entendre_difficile <- read_excel(raw_data_excel, sheet = "membre_entendre_difficile")
# %>%
#   rename(submission_uuid = `_submission__uuid...2`,
#          parent_index = `_parent_index...1`)

raw_data_membre_difficulte_communication <- read_excel(raw_data_excel, sheet = "communication_difficile")
# %>%
#   rename(submission_uuid = `_submission__uuid...2`,
#          parent_index = `_parent_index...1`)

raw_data_membre_repeat_nbre_pers_decedes <- read_excel(raw_data_excel, sheet = "repeat_nbre_pers_decedes")
# %>%
#   rename(submission_uuid = `_submission__uuid...2`,
#          parent_index = `_parent_index...1`)

loop_frames <- list(raw_data_info_menage, raw_data_maladie_moins_5ans_rpt, raw_data_naissances, raw_data_membre_marche_dificile,
                 raw_data_membre_soins_difficile, raw_data_membre_concentration_difficile, raw_data_membre_membre_vision_diffcile,
                 raw_data_membre_membre_entendre_difficile, raw_data_membre_difficulte_communication, raw_data_membre_repeat_nbre_pers_decedes)

loop_frames_names <- list("raw_data_info_menage", "raw_data_maladie_moins_5ans_rpt", "raw_data_naissances", "raw_data_membre_marche_dificile",
                    "raw_data_membre_soins_difficile", "raw_data_membre_concentration_difficile", "raw_data_membre_membre_vision_diffcile",
                    "raw_data_membre_membre_entendre_difficile", "raw_data_membre_difficulte_communication", "raw_data_membre_repeat_nbre_pers_decedes")


list_remove_data <- lapply(loop_frames, remove_delParents_fromLoop, raw_data, uuid.name.parent = "uuid", uuid.name.loop = "submission_uuid")

names(list_remove_data) <- loop_frames_names

### Loading OCHA's settlement layer

bfa_admin2 <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
  select(admin3Name, admin3Pcod)%>%
  distinct()%>%
  mutate(admin3Name = tolower(admin3Name),
         admin3Name = case_when(
           admin3Name == "karangasso - sambla" ~ "karangasso_sambla",
           admin3Name == "karangasso - vigue" ~ "karangasso_vigue",
           admin3Name == "fada n'gourma"  ~ "fada_n_gourma",
           admin3Name == "n'dorola" ~ "n_dorola",
           admin3Name == "tin-akoff" ~ "tin_akoff",
           admin3Name == "gorom-gorom" ~ "gorom_gorom",
           admin3Name == "la-todin" ~ "la_todin",
           admin3Name == "pobe-mangao" ~ "pobe_mangao",
           admin3Name == "bobo-dioulasso" ~ "bobo_dioulasso",
           TRUE ~ admin3Name
         ))


raw_data <- left_join(raw_data, bfa_admin2, by = c("admin3" = "admin3Name"))


# create_objects_from_df(list_remove_data)




### Loading sampling strategies 
samplingStrategies <- read_excel("data/Sampling_RU.xlsx", sheet = "Tableau Sampling Total", n_max = 663)%>%
  select(!starts_with("..."))%>%
  mutate_at(c("host_direct", "host_remote","idp_direct", "idp_telephone"), as.double)%>%
  pivot_longer(cols = c("host_direct", "host_remote","idp_direct", "idp_telephone"), names_to = "sampling_strat", values_to ="surveys_toBeDone")%>%
  filter(surveys_toBeDone >0, !is.na(surveys_toBeDone))%>%
  select(village_P_code, sampling_strat)%>%
  mutate(pcode_popGrp = paste(village_P_code, sampling_strat, sep = "_")
  )


### Cleaning pcodes for other localites

# bfa_pcodes <- read_excel("data/sampling-resume-enq.xlsx", sheet = "ITOS ADMIN3 PCODES")%>%
#   mutate(adm123 = cleaning_chr( gsub("-","_", tolower(paste0(ADM1_FR, ADM2_FR, ADM3_FR)))))
# 
# pcodes_probs <- raw_data%>%
#   select(admin1, admin2, admin3, pcode, localite, autre_localite, "_gpsmenage_latitude", "_gpsmenage_longitude", uuid)%>%
#   rename("gpsmenage_latitude" = "_gpsmenage_latitude", "gpsmenage_longitude"= "_gpsmenage_longitude")%>%
#   filter(localite == 'autre')%>%
#   mutate(
#     localite = case_when(localite == 'autre' ~ autre_localite,
#                          TRUE ~ localite),
#     gpsmenage_latitude = case_when(gpsmenage_latitude == "NA" ~ NA_character_ ,
#                                    TRUE ~ as.character(gpsmenage_latitude)),
#     gpsmenage_longitude = case_when(gpsmenage_longitude == "NA" ~ NA_character_ ,
#                                     TRUE ~ as.character(gpsmenage_longitude))
#   )%>%
#   group_by(admin1, admin2, admin3,pcode, localite)%>%
#   distinct()%>%
#   filter(!is.na(gpsmenage_latitude))%>%
#   ungroup()


### Loading clusters sampling frame and cleaning centers of clusters

clusterSample <- read_excel("data/Sampling_RU.xlsx", sheet = "sample_admin2_cluster_hexa500m_")%>%
  mutate(X = case_when(is.na(POINT_X) ~ as.double(NEAR_X),
                       TRUE ~ as.double(POINT_X)),
         Y = case_when(is.na(POINT_Y) ~ as.double(NEAR_Y),
                       TRUE ~ as.double(POINT_Y))
  )%>%
  select(-FID)%>%
  mutate(rowID = row_number())%>%
  group_by(id_sampl)%>%
  filter(rowID == min(rowID))%>%
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()


### Creating spatial objects for missingPcodes and clusters centers
# cluster_sf <- sf::st_as_sf(clusterSample, coords = c("X", "Y"), crs = 4326)%>%
#   select(POINT_X, POINT_Y, id_sampl, psu_id, strata_id, ADM3_PCODE, NEAR_pcode, NEAR_featureNam)%>%
#   st_set_crs("EPSG:4326")

# missingPoints_sf <- sf::st_as_sf(pcodes_probs, coords = c("gpsmenage_longitude", "gpsmenage_latitude"), crs = 4326)%>%
#   st_set_crs("EPSG:4326")


# bfa_settlments <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
#   sf::st_as_sf(coords = c("Coord_X", "Coord_Y"), crs = 4326)
  
### Replacing missing pcodes by closest pcode

# distance_check <- butteR::closest_distance_rtree(missingPoints_sf, bfa_settlments)%>%
#   select(uuid, pcode.1)
# 
# 
# raw_data <- raw_data%>%
#   left_join(distance_check, by = "uuid")%>%
#   mutate(pcode = case_when(is.na(pcode) ~ pcode.1,
#                            TRUE ~ pcode))



## Creating and writting cleaning_log to log changes

cleaning_log_change <- data.frame(Auteur = NA, Index = NA, uuid = NA, Date = NA, Base = NA, Enqueteur = NA, Question = NA,
         `Probl?me` = NA,
         "Anciennes valeurs" = NA, "Nouvelles valeurs" = NA, ddsagr = NA, "Retour Enquêteur" =NA, "Retour chargé" = NA, Action = NA,
         "Commentaire RBB" =0L,"Commentaire CC" = 0L,	"Commentaire AO" =0L,	"Commentaire GIS" =0L,	"dffgawesd" =0L)

##### Affecting data from repeat loops to main dataframe ####

raw_data_info_menage <- raw_data_info_menage%>%
  filter(parent_index %in% raw_data$index)%>%
  mutate(agegrp_0_5_femmes = if_else(age_hh < 5 &sexe_hh == "femme",1,0),
         agegrp_0_5_hommes = if_else(age_hh < 5 &sexe_hh == "homme",1,0),
         agegrp_5_18_femmes = if_else((age_hh >= 5 & age_hh <18 ) & sexe_hh == "femme",1,0),
         agegrp_5_18_hommes = if_else((age_hh >= 5 & age_hh < 18) & sexe_hh == "homme",1,0),
         agegrp_18_65_femmes = if_else((age_hh >= 18 & age_hh < 65 ) & sexe_hh == "femme",1,0),
         agegrp_18_65_hommes = if_else((age_hh >= 18 & age_hh < 65 ) & sexe_hh == "homme",1,0),
         agegrp_65plus_femmes = if_else(age_hh >= 65 &sexe_hh == "femme",1,0),
         agegrp_65plus_hommes = if_else(age_hh >= 65 &sexe_hh == "homme",1,0)
         )

info_menage_agg <- raw_data_info_menage %>%
  group_by(parent_index) %>%
  summarise(across(starts_with(c("agegrp_", "sum_age_0_5mois")), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data %>%
  left_join(info_menage_agg, by = c("index" = "parent_index"))

raw_data_info_menage <- koboloops::add_parent_to_loop(loop = as.data.frame(raw_data_info_menage), parent = as.data.frame(raw_data), variables.to.keep = c("barriere_edu", "status"), uuid.name.loop = "parent_index", uuid.name.parent = "index")%>%
  mutate(enfants_descolarise_insecurite = case_when(
    statut_educatif == "non" & barriere_edu %in% c("insecurite_ecole", "insecurite_trajet","groupes_armes_ecole", "non_fonctionnelle") ~ 1,
    statut_educatif == "oui" ~ 0,
    TRUE ~  0
    ),
  score_edu = case_when(statut_educatif == "non" & barriere_edu %in% c("insecurite_ecole", "insecurite_trajet","groupes_armes_ecole", "non_fonctionnelle") ~ "4",
                        statut_educatif == "non" ~ "3",
                        statut_educatif == "oui" & status == "pdi" ~ "3",
                        statut_educatif == "oui" & status != "pdi" ~ "1",
                        TRUE ~ NA_character_
    )
  )
         

# raw_data <- affect_loop_to_parent(loop = as.data.frame(raw_data_info_menage), parent = as.data.frame(raw_data), aggregate.function = sum,
#                                   variable.to.add = c(
#                                     sum_age_0_5mois = "age_0_5mois",
#                                     total_agegrp_0_5_femmes = "agegrp_0_5_femmes",
#                                     total_agegrp_0_5_hommes = "agegrp_0_5_hommes",
#                                     total_agegrp_5_18_femmes = "agegrp_5_18_femmes",
#                                     total_agegrp_5_18_hommes = "agegrp_5_18_hommes",
#                                     total_agegrp_18_65_femmes = "agegrp_18_65_femmes",
#                                     total_agegrp_18_65_hommes = "agegrp_18_65_hommes",
#                                     total_agegrp_65plus_femmes = "agegrp_65plus_femmes",
#                                     total_agegrp_65plus_hommes = "agegrp_65plus_hommes"
#                                   ),
#                                   uuid.name.loop = "parent_index", uuid.name.parent = "index")


raw_data_naissances <- levels_asBinaryColumns(raw_data_naissances, "lieu_accouchement")
raw_data_naissances <- levels_asBinaryColumns(raw_data_naissances, "raison_dominicile")

naissance_aggre <- raw_data_naissances %>%
  group_by(parent_index) %>%
  summarise(across(starts_with(c("lieu_accouchement.", "raison_dominicile.")), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data %>%
  left_join(naissance_aggre, by = c("index" = "parent_index"))



# issues_withNaissances <- raw_data%>%
#   select(total_naissance,
#          sum_lieu_accouchement.centre_sante,sum_lieu_accouchement.autre,sum_lieu_accouchement.maison,
#          uuid)%>%
#   mutate(oops_naissance = case_when(sum_lieu_accouchement.centre_sante > total_naissance ~"oops",
#                                     sum_lieu_accouchement.autre > total_naissance ~ "oops",
#                                     sum_lieu_accouchement.maison > total_naissance ~ "oops",
#                                     TRUE~"OK"))%>%
#   filter(oops_naissance=="oops")%>%
#   write_csv("problemes_repeat_Naissances.csv")


# raw_data_maladie_moins_5ans_rpt <- raw_data_maladie_moins_5ans_rpt%>%
#   mutate_at(vars(starts_with("maladie_moins_5ans/")), as.numeric)
# 
# raw_data <- affect_loop_to_parent(loop = as.data.frame(raw_data_maladie_moins_5ans_rpt), parent = as.data.frame(raw_data), aggregate.function = sum,
#                                       variable.to.add = c(
#                                         sum_enfants_5ans_maladie.palu = "maladie_moins_5ans/palu",
#                                         sum_enfants_5ans_maladie.infect_respiratoire = "maladie_moins_5ans/infect_respiratoire",
#                                         sum_enfants_5ans_maladie.diarrhee =  "maladie_moins_5ans/diarrhee",
#                                         sum_enfants_5ans_maladie.autre = "maladie_moins_5ans/autre",
#                                         sum_enfants_5ans_maladie.nsp = "maladie_moins_5ans/nsp"
#                                       ),
#                                       uuid.name.loop = "parent_index", uuid.name.parent = "index")
# 
# issues_enfants_malades <- raw_data%>%
#   select(total_moins_5ans,
#          sum_enfants_5ans_maladie.palu,sum_enfants_5ans_maladie.infect_respiratoire,sum_enfants_5ans_maladie.diarrhee,sum_enfants_5ans_maladie.autre,sum_enfants_5ans_maladie.nsp,
#          uuid)%>%
#   mutate(oops_malade = case_when(sum_enfants_5ans_maladie.palu > total_moins_5ans ~"oops",
#                                     sum_enfants_5ans_maladie.infect_respiratoire > total_moins_5ans ~ "oops",
#                                     sum_enfants_5ans_maladie.diarrhee > total_moins_5ans ~ "oops",
#                                     sum_enfants_5ans_maladie.autre > total_moins_5ans ~ "oops",
#                                     sum_enfants_5ans_maladie.nsp > total_moins_5ans ~ "oops",
#                                     TRUE~"OK"))%>%
#   filter(oops_malade=="oops")%>%
#   write_csv("problemes_repeat_malades.csv")

maladie_agg <- raw_data_maladie_moins_5ans_rpt %>%
  group_by(parent_index)%>%
  mutate(across(starts_with("maladie_moins_5ans/"), as.numeric))%>%
  summarise(across(starts_with("maladie_moins_5ans/"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))


raw_data <- raw_data %>%
  left_join(maladie_agg, by = c("index" = "parent_index"))

raw_data_membre_marche_dificile <- levels_asBinaryColumns(raw_data_membre_marche_dificile, "genre_marche")%>%
  mutate(agegrp = case_when(genre_marche == "homme" & (age_marche < 5) ~ "garcons_moins5",
                            genre_marche == "femme" & (age_marche < 5) ~ "filles_moins5",
                            genre_marche == "homme" & (age_marche >= 5 & age_marche <18) ~ "garcons_5_18",
                            genre_marche == "femme" & (age_marche >= 5 & age_marche <18) ~ "filles_5_18",
                            genre_marche == "homme" & (age_marche >= 18 & age_marche < 65) ~ "hommes_18_64",
                            genre_marche == "femme" & (age_marche >= 18 & age_marche < 65) ~ "femmes_18_64",
                            genre_marche == "homme" & (age_marche >= 65) ~ "hommes_65plus",
                            genre_marche == "femme" & (age_marche >= 65) ~ "femmes_65plus",
                            TRUE ~ NA_character_
  ),
  marche_dif_garcons_moins5 = if_else(agegrp == "garcons_moins5", 1, 0),
  marche_dif_filles_moins5 = if_else(agegrp == "filles_moins5", 1, 0),
  marche_dif_garcons_5_18 = if_else(agegrp == "garcons_5_18", 1, 0),
  marche_dif_filles_5_18 = if_else(agegrp == "filles_5_18", 1, 0),
  marche_dif_hommes_18_64 = if_else(agegrp == "hommes_18_64", 1, 0),
  marche_dif_femmes_18_64 = if_else(agegrp == "femmes_18_64", 1, 0),
  marche_dif_hommes_65plus = if_else(agegrp == "hommes_65plus", 1, 0),
  marche_dif_femmes_65plus = if_else(agegrp == "femmes_65plus", 1, 0)
  )


marche_difficile_agg <- raw_data_membre_marche_dificile%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("marche_dif_"), as.numeric))%>%
  summarise(across(starts_with("marche_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(marche_difficile_agg, by = c("index" = "parent_index"))


raw_data_membre_soins_difficile <- raw_data_membre_soins_difficile%>%
  mutate(
    soins_dif_garcons_moins5 = if_else(genre_soins == "homme" & (age_soins < 5), TRUE, FALSE),
    soins_dif_filles_moins5 = if_else(genre_soins == "homme" & (age_soins < 5), TRUE, FALSE),
    soins_dif_garcons_5_18 = if_else(genre_soins == "homme" & (age_soins >= 5 & age_soins <18), TRUE, FALSE),
    soins_dif_filles_5_18 = if_else(genre_soins == "femme" & (age_soins >= 5 & age_soins <18), TRUE, FALSE),
    soins_dif_hommes_18_64 = if_else(genre_soins == "homme" & (age_soins >= 18 & age_soins <65), TRUE, FALSE),
    soins_dif_femmes_18_64 = if_else(genre_soins == "femme" & (age_soins >= 18 & age_soins <65), TRUE, FALSE),
    soins_dif_hommes_65plus = if_else(genre_soins == "homme" & (age_soins >= 65), TRUE, FALSE),
    soins_dif_femmes_65plus = if_else(genre_soins == "femme" & (age_soins >= 65), TRUE, FALSE)
  )

soins_difficile_agg <- raw_data_membre_soins_difficile%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("soins_dif_"), as.numeric))%>%
  summarise(across(starts_with("soins_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))


raw_data <- raw_data%>%
    left_join(soins_difficile_agg, by = c("index" = "parent_index"))


raw_data_membre_concentration_difficile <- raw_data_membre_concentration_difficile%>%
  mutate(
    concentration_dif_garcons_moins5 = if_else(genre_concentration == "homme" & (age_concentration < 5), TRUE, FALSE),
    concentration_dif_filles_moins5 = if_else(genre_concentration == "homme" & (age_concentration < 5), TRUE, FALSE),
    concentration_dif_garcons_5_18 = if_else(genre_concentration == "homme" & (age_concentration >= 5 & age_concentration <18), TRUE, FALSE),
    concentration_dif_filles_5_18 = if_else(genre_concentration == "femme" & (age_concentration >= 5 & age_concentration <18), TRUE, FALSE),
    concentration_dif_hommes_18_64 = if_else(genre_concentration == "homme" & (age_concentration >= 18 & age_concentration <65), TRUE, FALSE),
    concentration_dif_femmes_18_64 = if_else(genre_concentration == "femme" & (age_concentration >= 18 & age_concentration <65), TRUE, FALSE),
    concentration_dif_hommes_65plus = if_else(genre_concentration == "homme" & (age_concentration >= 65), TRUE, FALSE),
    concentration_dif_femmes_65plus = if_else(genre_concentration == "femme" & (age_concentration >= 65), TRUE, FALSE)
  )

concentration_agg <- raw_data_membre_concentration_difficile%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("concentration_dif_"), as.numeric))%>%
  summarise(across(starts_with("concentration_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(concentration_agg, by = c("index" = "parent_index"))


raw_data_membre_membre_vision_diffcile <- raw_data_membre_membre_vision_diffcile%>%
  mutate(
    vision_dif_garcons_moins5 = if_else(genre_vision == "homme" & (age_vision < 5), TRUE, FALSE),
    vision_dif_filles_moins5 = if_else(genre_vision == "homme" & (age_vision < 5), TRUE, FALSE),
    vision_dif_garcons_5_18 = if_else(genre_vision == "homme" & (age_vision >= 5 & age_vision <18), TRUE, FALSE),
    vision_dif_filles_5_18 = if_else(genre_vision == "femme" & (age_vision >= 5 & age_vision <18), TRUE, FALSE),
    vision_dif_hommes_18_64 = if_else(genre_vision == "homme" & (age_vision >= 18 & age_vision <65), TRUE, FALSE),
    vision_dif_femmes_18_64 = if_else(genre_vision == "femme" & (age_vision >= 18 & age_vision <65), TRUE, FALSE),
    vision_dif_hommes_65plus = if_else(genre_vision == "homme" & (age_vision >= 65), TRUE, FALSE),
    vision_dif_femmes_65plus = if_else(genre_vision == "femme" & (age_vision >= 65), TRUE, FALSE)
  )

vision_agg <- raw_data_membre_membre_vision_diffcile%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("vision_dif_"), as.numeric))%>%
  summarise(across(starts_with("vision_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(vision_agg, by = c("index" = "parent_index"))


raw_data_membre_membre_entendre_difficile <- raw_data_membre_membre_entendre_difficile%>%
  mutate(
    entendre_dif_garcons_moins5 = if_else(genre_entendre == "homme" & (age_entendre < 5), TRUE, FALSE),
    entendre_dif_filles_moins5 = if_else(genre_entendre == "homme" & (age_entendre < 5), TRUE, FALSE),
    entendre_dif_garcons_5_18 = if_else(genre_entendre == "homme" & (age_entendre >= 5 & age_entendre <18), TRUE, FALSE),
    entendre_dif_filles_5_18 = if_else(genre_entendre == "femme" & (age_entendre >= 5 & age_entendre <18), TRUE, FALSE),
    entendre_dif_hommes_18_64 = if_else(genre_entendre == "homme" & (age_entendre >= 18 & age_entendre <65), TRUE, FALSE),
    entendre_dif_femmes_18_64 = if_else(genre_entendre == "femme" & (age_entendre >= 18 & age_entendre <65), TRUE, FALSE),
    entendre_dif_hommes_65plus = if_else(genre_entendre == "homme" & (age_entendre >= 65), TRUE, FALSE),
    entendre_dif_femmes_65plus = if_else(genre_entendre == "femme" & (age_entendre >= 65), TRUE, FALSE)
  )

entendre_agg <- raw_data_membre_membre_entendre_difficile%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("entendre_dif_"), as.numeric))%>%
  summarise(across(starts_with("entendre_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(entendre_agg, by = c("index" = "parent_index"))


raw_data_membre_repeat_nbre_pers_decedes <- raw_data_membre_repeat_nbre_pers_decedes%>%
  mutate(
    deces_dif_garcons_moins5 = if_else(sexe_deces == "homme" & (age_deces < 5), TRUE, FALSE),
    deces_dif_filles_moins5 = if_else(sexe_deces == "homme" & (age_deces < 5), TRUE, FALSE),
    deces_dif_garcons_5_18 = if_else(sexe_deces == "homme" & (age_deces >= 5 & age_deces <18), TRUE, FALSE),
    deces_dif_filles_5_18 = if_else(sexe_deces == "femme" & (age_deces >= 5 & age_deces <18), TRUE, FALSE),
    deces_dif_hommes_18_64 = if_else(sexe_deces == "homme" & (age_deces >= 18 & age_deces <65), TRUE, FALSE),
    deces_dif_femmes_18_64 = if_else(sexe_deces == "femme" & (age_deces >= 18 & age_deces <65), TRUE, FALSE),
    deces_dif_hommes_65plus = if_else(sexe_deces == "homme" & (age_deces >= 65), TRUE, FALSE),
    deces_dif_femmes_65plus = if_else(sexe_deces == "femme" & (age_deces >= 65), TRUE, FALSE)
  )%>%
  levels_asBinaryColumns("raison_deces")

decedes_agg <- raw_data_membre_repeat_nbre_pers_decedes%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("deces_dif_"), as.numeric))%>%
  mutate(across(starts_with("raison_deces."), as.numeric))%>%
  summarise(across(matches("deces_dif_|raison_deces."), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(decedes_agg, by = c("index" = "parent_index"))

raw_data_membre_difficulte_communication <- raw_data_membre_difficulte_communication%>%
  mutate(
    communication_dif_garcons_moins5 = if_else(genre_difficulte_communication == "homme" & (age_difficulte_communication < 5), TRUE, FALSE),
    communication_dif_filles_moins5 = if_else(genre_difficulte_communication == "homme" & (age_difficulte_communication < 5), TRUE, FALSE),
    communication_dif_garcons_5_18 = if_else(genre_difficulte_communication == "homme" & (age_difficulte_communication >= 5 & age_difficulte_communication <18), TRUE, FALSE),
    communication_dif_filles_5_18 = if_else(genre_difficulte_communication == "femme" & (age_difficulte_communication >= 5 & age_difficulte_communication <18), TRUE, FALSE),
    communication_dif_hommes_18_64 = if_else(genre_difficulte_communication == "homme" & (age_difficulte_communication >= 18 & age_difficulte_communication <65), TRUE, FALSE),
    communication_dif_femmes_18_64 = if_else(genre_difficulte_communication == "femme" & (age_difficulte_communication >= 18 & age_difficulte_communication <65), TRUE, FALSE),
    communication_dif_hommes_65plus = if_else(genre_difficulte_communication == "homme" & (age_difficulte_communication >= 65), TRUE, FALSE),
    communication_dif_femmes_65plus = if_else(genre_difficulte_communication == "femme" & (age_difficulte_communication >= 65), TRUE, FALSE)
  )

communication_agg <- raw_data_membre_difficulte_communication%>%
  group_by(parent_index)%>%
  mutate(across(starts_with("communication_dif_"), as.numeric))%>%
  summarise(across(matches("communication_dif_"), sum, na.rm = T)) %>%
  mutate(parent_index = as.character(parent_index))

raw_data <- raw_data%>%
  left_join(communication_agg, by = c("index" = "parent_index"))


### removing deleted from loops

list_remove_data <- lapply(loop_frames, remove_delParents_fromLoop, raw_data, uuid.name.parent = "uuid", uuid.name.loop = "submission_uuid")

names(list_remove_data) <- loop_frames_names


invisible(create_objects_from_df(list_remove_data))


# WEIGHTING #

# coords <- c("gpsmenage_longitude", "gpsmenage_latitude")
# 
# raw_data_ClusterSmpl <- raw_data%>%
#   rename("gpsmenage_latitude" = "_gpsmenage_latitude", "gpsmenage_longitude"= "_gpsmenage_longitude")%>%
#   mutate_at(vars(all_of(coords)), as.numeric)%>%
#   filter(!is.na(gpsmenage_latitude) & !is.na(gpsmenage_longitude), modalite == "direct" & status == "host")%>%
#   sf::st_as_sf(coords = coords, crs = 4326)%>%
#   st_set_crs("EPSG:4326")
# 
# ## Finding closest cluster to all direct host interviews
# 
# all_closest_cluster <- butteR::closest_distance_rtree(raw_data_ClusterSmpl, cluster_sf)
# 
# closest_cluster <- all_closest_cluster%>%
#   filter(dist_m <=1500)%>%
#   select(uuid, id_sampl)
# 
# raw_data <- raw_data%>%
#   left_join(closest_cluster, by = "uuid")%>%
#   mutate(pcode = case_when(id_sampl == "id_45382" ~ NA_character_,
#                            id_sampl == "id_45749" ~ NA_character_,
#                            TRUE ~ pcode
#   ))
#   
# 

raw_data <- raw_data%>%
  mutate(admin1Pcode = gsub(".{4}$", "", admin3Pcod),
         admin2Pcode = gsub(".{2}$", "", admin3Pcod),
         admin3Pcode = admin3Pcod,
         sampling_id = paste(admin3Pcode, status, sep = "_")
  )%>%
  select(-admin3Pcod)

### Adding useful columns for calculations
add_to_changeLog_ic_age <- raw_data%>%
  select(uuid, age_chef_menage, ic_age)%>%
  filter(is.na(age_chef_menage))

cleaning_log_change <- cleaning_log_change%>%
  add_row(Auteur = "Elliott Messeiller", Index = NA, uuid = add_to_changeLog_ic_age$uuid, Date = Sys.Date(), Base = NA, Enqueteur = NA, Question = "Age chef menage", "Probl.me" = "Manque âge du chef de ménage quand il est le répondant",
          Anciennes.valeurs = NA, Nouvelles.valeurs = add_to_changeLog_ic_age$ic_age)

add_to_changeLog_ic_genre <- raw_data%>%
  select(uuid, genre_chef_menage, ic_genre)%>%
  filter(is.na(genre_chef_menage))

cleaning_log_change <- cleaning_log_change%>%
  mutate(across(everything(), as.character))%>%
  add_row(Auteur = "Elliott Messeiller", Index = NA, uuid = add_to_changeLog_ic_genre$uuid, Date = as.character(Sys.Date()), Base = NA, Enqueteur = NA,
          Question = "Genre du chef menage", "Probl.me" = "Manque genre du chef de ménage quand il est le répondant",
          Anciennes.valeurs = NA, Nouvelles.valeurs = add_to_changeLog_ic_genre$ic_genre)

raw_data <- raw_data%>%
  rowwise()%>%
  mutate(
      age_chef_menage = case_when( chef_menage == "oui" && is.na(age_chef_menage) ~ ic_age,
                                  TRUE ~ age_chef_menage),
      genre_chef_menage = case_when(chef_menage == "oui" && is.na(genre_chef_menage) ~ ic_genre,
                                    TRUE ~ ic_genre),
      taille_abri = case_when(taille_abri == "NSP" ~ NA_integer_, TRUE ~ as.integer(taille_abri)),
      personne_m2 = taille_abri/taille_menage,
      fcs2 = sum(jr_consom_cereale*2 + jr_consom_noix*3 + jr_consom_lait*4 + jr_consom_viande*4 + jr_consom_legume*1 + jr_consom_fruit*1 + jr_consom_huile*0.5+
                   jr_consom_sucre*0.5 + jr_consom_epice*0, na.rm = T),
      fcs2_thresholds = case_when(fcs2>=0 & fcs2 <=21 ~ "pauvre",
                                  fcs2 > 21 & fcs2 <= 35 ~ "limite",
                                  fcs2 > 35 ~ "acceptable",
                                  TRUE ~ NA_character_),
      rcsi_score = sum(jr_moins_prefere*1, jr_emprunt_nourriture*2, jr_diminu_quantite*1, jr_rest_consommation*3, jr_nbr_repas*1, na.rm = T),
      rcsi_thresholds = case_when(
        rcsi_score == 0 ~ "no_coping",
        rcsi_score > 0 & rcsi_score <= 7 ~ "low",
        rcsi_score > 7 & rcsi_score <= 15 ~ "medium",
        rcsi_score > 15 ~ "high",
        TRUE ~ NA_character_
      ),
      nbr_aucun_aliment_new = hhs_recoding(aucun_aliment, nbr_aucun_aliment),
      nbr_dormir_affame_new = hhs_recoding(dormir_affame, nbr_dormir_affame),
      nbr_pas_assez_nourriture_new = hhs_recoding(pas_assez_nourriture, nbr_pas_assez_nourriture),
      hhs_score = sum(nbr_aucun_aliment_new, nbr_dormir_affame_new, nbr_pas_assez_nourriture_new, na.rm = T),
      hhs_thresholds = case_when(
        hhs_score <= 1 ~ "peu_pasFaim",
        hhs_score >1 & hhs_score <= 3 ~ "faim_moderee",
        hhs_score > 3 & hhs_score <= 6 ~ "faim_severe",
        TRUE ~ NA_character_
      ),
      vente_actif_recoded = lcs_recoding(vente_actif),
      vente_actif_prod_recoded = lcs_recoding(vente_actif_prod),
      reduction_depense_recoded = lcs_recoding(reduction_depense),
      epargne_recoded = lcs_recoding(epargne),
      emprunt_nourritur_recoded = lcs_recoding(emprunt_nourritur),
      enfant_ecole_recoded = lcs_recoding(enfant_ecole),
      vente_maison_recoded = lcs_recoding(vente_maison),
      activite_risque_recoded = lcs_recoding(activite_risque),
      mendie_recoded = lcs_recoding(mendie),
      vente_animal_recoded = lcs_recoding(vente_animal),
      conso_semence_recoded = lcs_recoding(conso_semence),
      lcs_urgence = if_else(sum(vente_maison_recoded, mendie_recoded, activite_risque_recoded, na.rm = T ) >= 1, 1,0),
      lcs_crise = if_else(sum(vente_actif_prod_recoded,reduction_depense_recoded,  enfant_ecole_recoded, conso_semence_recoded, na.rm = T) >= 1, 1, 0),
      lcs_stress = if_else(sum(vente_actif_recoded, epargne_recoded, emprunt_nourritur_recoded, vente_animal_recoded, na.rm = T) >= 1, 1,0),
      lcs_minimal = if_else((lcs_urgence + lcs_crise + lcs_stress) == 0, 1, 0),
      lcs_total = if_else(lcs_urgence ==1, "urgence", if_else(lcs_crise == 1, "crise", if_else( lcs_stress ==1, "stress", "minimal"))),
      auMoinsUneWG = if_else(sum(nombre_soins_difficile, nombre_difficulte_vision, nombre_difficulte_entendre,
                                 nombre_difficulte_marche, nombre_difficulte_concentration, nombre_difficulte_communication, na.rm = T)>0,1,0),
      auMoinsUnePersonneDortDehors = if_else(dorme_exterieur >0, 1,0),
      auMoinsUnEnfantMalade = if_else(malade_5ans>0,1,0),
      typologie_source_eau = case_when(source_eau %in% c("pmh","poste_auto", "puit_protege","source_amenage","borne_fontaine",
                                                         "eau _robi_conce","eau _bout","eau _camion") ~ "amelioree",
                                       source_eau %in% c("puit_tradi", "non_amenage") ~ "non_amelioree",
                                       source_eau %in% c("course_eau", "eau_pluie") ~ "surface",
                                       TRUE ~ NA_character_
      ),
      probleme_abri.inondation = case_when(str_detect(probleme_abri, "inondations") ~ TRUE,
                                            !str_detect(probleme_abri, "inondations") ~ FALSE),
      
      total_3_5 = sum(total_3_5_femmes, total_3_5_hommes, na.rm = T),
      total_6_12 = sum(total_6_12_femmes, total_6_12_hommes, na.rm = T),
      total_13_17 = sum(total_13_17_femmes, total_13_17_hommes, na.rm = T),
      
      educ.3_5an_total = sum(total_educ_3_5an_garcon,total_educ_3_5an_fille, na.rm = T),
      infor_educ.3_5an_total = sum(total_infor_educ_3_5an_garcon, total_infor_educ_3_5an_fille, na.rm = T),
      non_for_educ.3_5an_total = sum(total_non_for_educ_3_5an_garcon, total_non_for_educ_3_5an_fille, na.rm = T),
      aucune_educ.3_5an_total = sum(total_aucune_educ_3_5an_garcon, total_aucune_educ_3_5an_fille, na.rm = T),
      
      educ.6_12an_total = sum(total_educ_6_12an_garcon,total_educ_6_12an_fille, na.rm = T),
      infor_educ.6_12an_total = sum(total_infor_educ_6_12an_garcon, total_infor_educ_6_12an_fille, na.rm = T),
      non_for_educ.6_12an_total = sum(total_non_for_educ_6_12an_garcon, total_non_for_educ_6_12an_fille, na.rm = T),
      aucune_educ.6_12an_total = sum(total_aucune_educ_6_12an_garcon, total_aucune_educ_6_12an_fille, na.rm = T),
      
      educ.13_17an_total = sum(total_educ_13_17an_garcon,total_educ_13_17an_fille, na.rm = T),
      infor_educ.13_17an_total = sum(total_infor_educ_13_17an_garcon, total_infor_educ_13_17an_fille, na.rm = T),
      non_for_educ.13_17an_total = sum(total_non_for_educ_13_17an_garcon, total_non_for_educ_13_17an_fille, na.rm = T),
      aucune_educ.13_17an_total = sum(total_aucune_educ_13_17an_garcon, total_aucune_educ_13_17an_fille, na.rm = T),
      
      admin2 = case_when(admin3 == "solenzo" ~ "banwa", 
                         TRUE ~ admin2)
      
  )


raw_data <- raw_data%>%
  group_by(sampling_id)%>%
  filter(n() >2)%>%
  ungroup()

### Loading 2stage sampling frame

samplingFrame_raw <- read_excel("data/REACH_BFA_Pop_for_weighting_20201308.xlsx", sheet = "Population")%>%
  filter(Admin3Pcod %in% raw_data$admin3Pcode,
         Admin3 != "Nassoumbou")

  samplingFrameADM3 <- samplingFrame_raw%>%
  group_by(Admin1, Admin1Pcod, Admin2, Admin2Pcod,Admin3, Admin3Pcod)%>%
  summarise(host = sum(`Total local`, na.rm = TRUE),
            pdi = sum(`Total PDI`, na.rm = TRUE), .groups = "drop")%>%
  pivot_longer(cols = c("host", "pdi"), names_to = "pop_grp", values_to = "Population_adm3")%>%
  mutate(strata_adm3 = paste(Admin3Pcod, pop_grp, sep = "_"))%>%
  filter(!is.na(Population_adm3), Population_adm3 >0)

samplingFrameADM2 <- samplingFrame_raw%>%
  group_by(Admin1, Admin1Pcod, Admin2, Admin2Pcod)%>%
  summarise(host = sum(`Total local`, na.rm = TRUE),
            pdi = sum(`Total PDI`, na.rm = TRUE), .groups = "drop")%>%
  pivot_longer(cols = c("host", "pdi"), names_to = "pop_grp", values_to = "Population_adm2")%>%
  mutate(strata_adm2 = paste(Admin2Pcod, pop_grp, sep = "_"))%>%
  filter(!is.na(Population_adm2), Population_adm2 >0)

samplingFrame <- left_join(samplingFrameADM2,samplingFrameADM3, by = c("Admin2Pcod", "Admin1", "Admin1Pcod", "Admin2", "pop_grp"))

samplingFrame_adm2 <- samplingFrame%>%
  filter(Admin1 %in% c("Est", "Centre-Nord","Sahel","Boucle du Mouhoun","Nord"))%>%
  filter(!is.na(Population_adm3))


### Weighting cluster sample


# raw_data_clusters_tab <- raw_data%>%
#   group_by(id_sampl)%>%
#   summarise(effictive_interviews = n(), .groups = "drop")
# 
# clusterSample_check <- clusterSample%>%
#   left_join(raw_data_clusters_tab, by = "id_sampl")%>%
#   mutate(diff_sampl_reality = survey_buffer - effictive_interviews)%>%
#   select(id_sampl, strata_id, psu_id, population, ADM1_FR, ADM2_FR, ADM3_FR, ADM3_PCODE, survey_buffer, effictive_interviews, diff_sampl_reality)
#   ### Little difference between sample and effective 
# 
# 
# # 
# clusterSample_adm2 <- clusterSample%>%
#   filter(ADM1_PCODE %in% c("BF46", "BF49", "BF52", "BF54", "BF56"))
# 
# 
# 
# cluster_wght_representative <- map_to_weighting(sampling.frame = clusterSample, 
#                                               data = raw_data_representative,
#                                               sampling.frame.population.column = "population",
#                                               sampling.frame.stratum.column = "id_sampl",
#                                               data.stratum.column = "id_sampl"
# )

# raw_data$sampling_strat <- paste(raw_data$status, raw_data$modalite, sep = "_")
# 
# raw_data_representative <- raw_data%>%
#   filter(admin1Pcode %in% c("BF46", "BF49", "BF52", "BF54", "BF56"),
#          sampling_id %in% samplingFrame_adm2$strata,
#          sampling_strat == "host_direct",
#          sampling_id != "BF5002_host")
# 
# admin2_repesentative_wght <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
#                                               data = raw_data_representative,
#                                               sampling.frame.population.column = "Population",
#                                               sampling.frame.stratum.column = "strata",
#                                               data.stratum.column = "sampling_id")

# combined_weights_representative <- combine_weighting_functions(cluster_wght_representative, admin2_repesentative_wght)

# raw_data_representative$weights <- combined_weights_representative(raw_data_representative)

# raw_data_representative$weights_sampling <- admin2_repesentative_wght(raw_data_representative)



# ### Weighting for quota admin 2 only
# 
# raw_data_adm2_quota <- raw_data%>%
#   filter(admin1Pcode %in% c("BF46", "BF49", "BF52", "BF54", "BF56"),
#          sampling_strat != "host_direct",
#          sampling_id %in% samplingFrame_adm2$strata
#          )
# 
# 
# admin2_affected_wght <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
#                                                     data = raw_data_adm2_quota,
#                                                     sampling.frame.population.column = "Population",
#                                                     sampling.frame.stratum.column = "strata",
#                                                     data.stratum.column = "sampling_id"
#                                                     )
# 
# raw_data_adm2_quota$weights_sampling <- admin2_affected_wght(raw_data_adm2_quota)
# 
# 
# 
# ### Combining cluster weights with admin2 quota weights for 5 most affected regions
# 
# raw_data_adm2_affected_all <- rbind(raw_data_representative, raw_data_adm2_quota)

raw_data_adm2 <- raw_data%>%
  mutate(sampling_id_adm2 = paste(admin2Pcode, status, sep="_"))%>%
  filter(admin1Pcode %in% c("BF46", "BF49", "BF52", "BF54", "BF56"),
         sampling_id %in% samplingFrame_adm2$strata_adm3
  )

admin2_wght_adm2 <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
                                         data = raw_data_adm2,
                                         sampling.frame.population.column = "Population_adm2",
                                         sampling.frame.stratum.column = "strata_adm2",
                                         data.stratum.column = "sampling_id_adm2"
)

admin2_wght_adm3 <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
                                     data = raw_data_adm2,
                                     sampling.frame.population.column = "Population_adm3",
                                     sampling.frame.stratum.column = "strata_adm3",
                                     data.stratum.column = "sampling_id"
)

combined_weights_adm2 <- combine_weighting_functions(admin2_wght_adm2,admin2_wght_adm3)

raw_data_adm2$weights_sampling <- combined_weights_adm2(raw_data_adm2)


### Weighting for admin1 ###

#### Weighting admin1 cluster ####

# raw_data_ClusterSmpl_adm1 <- raw_data%>%
#   filter(sampling_strat == "host_direct", id_sampl %in% clusterSample$id_sampl,
#          !sampling_id %in%  c("NA_host", "NA_pdi"))
# 
# cluster_wght_admin1 <- map_to_weighting(sampling.frame = clusterSample,
#                                         data = raw_data_ClusterSmpl_adm1,
#                                         sampling.frame.population.column = "population",
#                                         sampling.frame.stratum.column = "id_sampl",
#                                         data.stratum.column = "id_sampl"
# )

# sf_cluster_wght_admin1 <- map_to_weighting(sampling.frame = samplingFrame, 
#                                            data = raw_data_ClusterSmpl_adm1,
#                                            sampling.frame.population.column = "Population",
#                                            sampling.frame.stratum.column = "strata",
#                                            data.stratum.column = "sampling_id"
# )

# combined_weights_adm1_cluster <- combine_weighting_functions(cluster_wght_admin1, sf_cluster_wght_admin1)

# raw_data_ClusterSmpl_adm1$weights <- combined_weights_adm1_cluster(raw_data_ClusterSmpl_adm1)


#### Weighting admin1 quotas ####


raw_data_adm1 <- raw_data%>%
  mutate(sampling_id_adm2 = paste(admin2Pcode, status, sep="_"))%>%
  filter(sampling_id %in% samplingFrame$strata_adm3)

admin1_wght_adm2 <- map_to_weighting(sampling.frame = samplingFrame, 
                                     data = raw_data_adm1,
                                     sampling.frame.population.column = "Population_adm2",
                                     sampling.frame.stratum.column = "strata_adm2",
                                     data.stratum.column = "sampling_id_adm2"
)

admin1_wght_adm3 <- map_to_weighting(sampling.frame = samplingFrame, 
                                     data = raw_data_adm1,
                                     sampling.frame.population.column = "Population_adm3",
                                     sampling.frame.stratum.column = "strata_adm3",
                                     data.stratum.column = "sampling_id"
)

combined_weights_adm1 <- combine_weighting_functions(admin1_wght_adm2,admin1_wght_adm3)

raw_data_adm1$weights_sampling <- combined_weights_adm1(raw_data_adm1)


### Removing from main data frames surveys with no sampling ID attriuable

not_in_raw_data_adm1 <- raw_data[!raw_data$uuid %in% raw_data_adm1$uuid, ]

raw_data <-raw_data%>%
  select(-contains("gps"), -DFERWF)

weights_adm2 <- raw_data_adm2%>%
  select(sampling_id, weights_sampling)%>%
  rename(weights_sampling_adm2 = weights_sampling)%>%
  mutate(weights_sampling_adm2 = as.vector(weights_sampling_adm2))

raw_data_adm1 <- raw_data_adm1%>%
  mutate(weights_sampling = as.vector(weights_sampling))%>%
  select(-contains("gps"), -DFERWF)%>%
  left_join(weights_adm2, by = "sampling_id")%>%
  distinct()

raw_data_adm2 <- raw_data_adm2%>%
  select(-contains("gps"), -DFERWF)



if(nrow(not_in_raw_data_adm1)>0){
cleaning_log_change <- cleaning_log_change%>%
  add_row(Auteur = "Elliott Messeiller", uuid = not_in_raw_data_adm1$uuid, Date = as.character(Sys.Date()), Enqueteur = NA, Question = "Localite",
          `Probl.me` = "Localité introuvable dans le sampling frame. Impossible d'attribuer au bon PSU.",
          `Anciennes.valeurs` = not_in_raw_data_adm1$uuid, Action = "Enquêtes supprimées")
}

### Writing files

write_csv(cleaning_log_change, "outputs/logs/cleaning_log_missingPcodes.csv")
write_csv(raw_data_adm1, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM1.csv")
# write_csv(raw_data_representative, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_representativeData.csv")
# write_csv(raw_data_adm2_quota, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_quota_ADM2affected.csv")
write_csv(raw_data_adm2, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM2_all.csv")

loopsFiles.names <- paste0("./outputs/datasets/loops/BFA_MSNA_2020_dataset_cleanedWeighted_",unlist(loop_frames_names), ".csv")

for(i in seq_along(list_remove_data)){
  write_csv(list_remove_data[[i]], loopsFiles.names[[i]])
}

