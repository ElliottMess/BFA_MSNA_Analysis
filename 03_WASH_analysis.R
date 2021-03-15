source("utils.R")

## Creating list of packages to load
package_list <- c("elliottmess/butteR", "hypegrammaR","ellieallien/cleaninginspectoR","hunzikp/rtree", "impact-initiatives/koboloops",
                  "tidyr","dplyr", "ggplot2", "readr", "stringr", "lubridate", "readxl", "rgdal", "sf", "purrr", "sdcMicro", "srvyr", "questionr")

## Running the packaging loading function
loadInstall_package(package_list)


source("01_PrepingAnalysis.R", encoding = "UTF-8")
source("utils.R")
source("analysisplan_factory.R", encoding = "UTF-8")
source("analysis_functions.R", encoding = "UTF-8")

# analysisplanTemplate_wash <- read_csv(template_analysisplan_file, locale = locale(encoding = "UTF-8"))%>%
#   filter(research.question == "eha" | dependent.variable %in% c("auMoinsUneWG", "taille_menage", "type_abri", "vbg",
#                                                                 "titre_propriete", "fcs2_thresholds", "hhs_thresholds", "lcsi", "rcsi_thresholds"))
# template_analysisplan_file_wash <- "data/analysis_plans/analysisplanTemplate_wash.csv"
# write.csv(analysisplanTemplate_wash, template_analysisplan_file_wash)
  
cleaned_data_adm1$admin0 <- "BFA"

cleaned_data_adm1 <- cleaned_data_adm1%>%
  mutate(
    pasEau_DAL = case_when(eau_suffi %in% c("insuffisant", "pas_suffisant") & infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_zonep_am", "dal_eau") ~ 1L,
                                  !eau_suffi %in% c("insuffisant", "pas_suffisant") & !infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_zonep_am", "dal_eau") ~ 0L,
                                  TRUE ~ NA_integer_),
    latrine_shared = case_when(
      infra_sanitaire  %in% c("lat_prive", "toilette") & is.na(type_latrine) ~ "ind",
      type_latrine == "lat_ind" ~ "ind",
      TRUE ~ type_latrine
    ),
    
    latrine_moins20 = case_when(
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared %in% c("ind", "lat_com_20") ~ 0L,
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com_50" ~ 1L,
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com" ~ 1L,
      infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ 1L,
      TRUE ~ NA_integer_
    ),

    latrine_pashygenique_plusmoins20 =
      case_when(latrine_moins20 == 0 & lat_hygiene == "oui" ~ 0L,
                latrine_moins20 == 0 & lat_hygiene == "non" ~ 1L,
                latrine_moins20 == 1 ~ 1L,
                TRUE ~ NA_integer_
      ),
    latrine_hygienique = case_when(
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") ~ paste("lat", lat_hygiene, sep = "_"),
      infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ "dal",
      TRUE ~ NA_character_
    ),
    lat_hyg_shared = case_when(
      latrine_hygienique == "lat_oui" & latrine_shared == "ind" ~ "lat_hyg_ind",
      latrine_hygienique == "lat_oui" & latrine_shared == "lat_com_20" ~ "lat_hyg_less20",
      latrine_hygienique == "lat_oui" & latrine_shared == "lat_com_50" ~ "lat_hyg_20to50",
      latrine_hygienique == "lat_oui" & latrine_shared == "lat_com" ~ "lat_hyg_20to50",
      TRUE ~ NA_character_
    ),
    open_defecation = case_when(
      infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ 1L,
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") ~ 0L,
      TRUE ~ NA_integer_
    )
  )

write.csv(cleaned_data_adm1, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM1_addCalc.csv")

cleaned_data_adm1_hrp <- cleaned_data_adm1%>%
  filter(admin1%in% c("centre_est", "boucle_du_mouhoun", "est", "sahel", "cascades", "nord", "centre_nord"))

# analysisplan_admin_0_wash <- analysisplanTemplate_wash%>%
#   mutate(repeat.for.variable = "admin0",
#          hypothesis.type = "direct_reporting",
#          )

# analysisplan_admin_0_wash <- analysisplan_admin_0_wash[!is.na(analysisplan_admin_0_wash$dependent.variable.type),]
# write_csv(analysisplan_admin_0_wash, "data/analysis_plans/analysisplan_admin_0_wash.csv")


# final_result_admin_0_wash <- from_analysisplan_map_to_output(data = cleaned_data_adm1_hrp,
#                                                             analysisplan = analysisplan_admin_0_wash,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# saveRDS(final_result_admin_0_wash, "outputs/final_result_admin_0_wash.RDS")


final_result_admin_0_wash <- readRDS("outputs/final_result_admin_0_wash.RDS")

# summary_stats_admin_0_final_wash <- format_results(cleaned_data_adm1_hrp, final_result_admin_0_wash, aggregate_level = "admin0", weights = "weights_sampling", csv = TRUE)%>%
#   # mutate(
#   #   `Groupe de population` = case_when(`Groupe de population` == "PDI" ~ "PDI",
#   #                                      `Groupe de population` == "CommunautÃ© hÃ´te" ~ "Communauté hôte",
#   #                                      TRUE ~ NA_character_)
#   # )%>%
#   rename(
#     # `Sous-ensemble de donnée` = `Sous-ensemble de donnÃ©e`,
#          `Régions incluses dans le HRP` = BFA)%>%
#   filter(`Sous-question de recherche` %in% analysisplanTemplate_wash$sub.research.question_label)


# write_csv(summary_stats_admin_0_final_wash, "outputs/tables/summary_stats_admin0_hrp_wash_total.csv")

# analysisplan_admin_0_grp_wash <- analysisplanTemplate_wash%>%
#   mutate(
#     repeat.for.variable = "admin0",
#     independent.variable = "status",
#     independent.variable.type = "categorical",
#     hypothesis.type = "group_difference"
#   )
# analysisplan_admin_0_grp_wash <- analysisplan_admin_0_grp_wash[!is.na(analysisplan_admin_0_grp_wash$dependent.variable.type),]
# write_csv(analysisplan_admin_0_grp_wash, "data/analysis_plans/analysisplan_admin_0_grp_wash.csv")
# analysisplan_admin_0_grp_wash <- read_csv("data/analysis_plans/analysisplan_admin_0_grp_wash.csv")
# # final_result_admin_0_hrp_admin1_grp_wash <- from_analysisplan_map_to_output(data = cleaned_data_adm1_hrp,
# #                                                                        analysisplan = analysisplan_admin_0_grp_wash,
# #                                                                        weighting = combined_weights_adm1,
# #                                                                        questionnaire = questionnaire)
# # saveRDS(final_result_admin_0_hrp_admin1_grp_wash, "outputs/final_result_admin_0_hrp_admin1_grp_wash.RDS")
# 
# final_result_admin_0_hrp_admin1_grp_wash <- readRDS("outputs/final_result_admin_0_hrp_admin1_grp_wash.RDS")
# 
# summary_stats_admin_0_hrp_admin1_grp_final_wash <- format_results(cleaned_data_adm1_hrp, final_result_admin_0_hrp_admin1_grp_wash,
#                                                              aggregate_level = "admin0", pop_grp = "status", weights = "weights_sampling", csv = TRUE)%>%
#   # mutate(
#   #   `Groupe de population` = case_when(`Groupe de population` == "PDI" ~ "PDI",
#   #                                      `Groupe de population` == "CommunautÃ© hÃ´te" ~ "Communauté hôte",
#   #                                      TRUE ~ NA_character_),
#   #   `Sous-ensemble de donnÃ©e` = case_when(`Sous-ensemble de donnÃ©e` == "Sous-ensemble de donnÃ©e" ~ "Sous-ensemble de donnée",
#   #                                          TRUE ~ `Sous-ensemble de donnÃ©e`)
#   # )%>%
#   rename(
#     # `Sous-ensemble de donnée` = `Sous-ensemble de donnÃ©e`,
#          `Régions incluses dans le HRP` = BFA)%>%
#   filter(`Sous-question de recherche` %in% analysisplanTemplate_wash$sub.research.question_label)
# 
# 
# summary_stats_admin_0_hrp_admin1_grp_final_wash_alltogether <- rbind(summary_stats_admin_0_final_wash, summary_stats_admin_0_hrp_admin1_grp_final_wash)
# 
# write_csv(summary_stats_admin_0_hrp_admin1_grp_final_wash, "outputs/tables/summary_stats_admin0_hrp_wash_grp.csv")
# 
# 
# write_csv(summary_stats_admin_0_hrp_admin1_grp_final_wash_alltogether, "outputs/tables/summary_stats_admin0_hrp_wash.csv")


#Manual calculations because hypegrammaR...
srv_cleaned_data_adm1_hrp <- srvyr::as_survey_design(cleaned_data_adm1_hrp, weights = "weights_sampling")

taille_moy_hh <- srv_cleaned_data_adm1_hrp%>%
  summarise(taille_moy_hh = survey_mean(taille_menage))

auMoinsUneWG_result <- srv_cleaned_data_adm1_hrp%>%
  summarise(auMoinsUneWG = survey_mean(auMoinsUneWG))

auMoinsUneWG_result_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(auMoinsUneWG = survey_mean(auMoinsUneWG))

lave_main_tot <- cleaned_data_adm1_hrp%>%
  select(starts_with("lave_main."), weights_sampling)%>%
  summarise(
    aucun = weighted.mean(lave_main.aucun, w = weights_sampling),
    apres_toilette = weighted.mean(lave_main.apres_toilette, w = weights_sampling),
    avant_manger = weighted.mean(lave_main.avant_manger, w = weights_sampling),
    avant_prepare = weighted.mean(lave_main.avant_prepare, w = weights_sampling),
    apres_netto = weighted.mean(lave_main.apres_netto, w = weights_sampling),
    avant_sein = weighted.mean(lave_main.avant_sein, w = weights_sampling),
    retour_champ = weighted.mean(lave_main.retour_champ, w = weights_sampling),
    autre = weighted.mean(lave_main.autre, w = weights_sampling),
    nsp = weighted.mean(lave_main.nsp, w = weights_sampling),
    ablution = weighted.mean(lave_main.ablution, w =weights_sampling)
    )

vbg_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(vbg = survey_mean(vbg))

vbg_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(vbg = survey_mean(vbg))

diarr_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(diarr = survey_mean(enfant_selle))

pasEau_DAL_pasSavon_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(pasEau_DAL_pasSavon_tot = survey_mean(pasEau_DAL_pasSavon, na.rm = T))

pasEau_DAL_pasSavon_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(pasEau_DAL_pasSavon_tot = survey_mean(pasEau_DAL_pasSavon, na.rm = T))

pasEau_DAL_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(pasEau_DAL_tot = survey_mean(pasEau_DAL, na.rm = T))

pasEau_DAL_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(pasEau_DAL_tot = survey_mean(pasEau_DAL, na.rm = T))


aumoins5moments_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(aumoins5moments = survey_mean(auMoins_5moments_mains, na.rm=T))

aumoins5moments_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(aumoins5moments = survey_mean(auMoins_5moments_mains, na.rm=T))

auMoins_aprLat_avantMan_tot <- srv_cleaned_data_adm1_hrp%>%
  summarise(auMoins_aprLat_avantMan = survey_mean(auMoins_aprLat_avantMan, na.rm = T))

auMoins_aprLat_avantMan_grp <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(auMoins_aprLat_avantMan = survey_mean(auMoins_aprLat_avantMan, na.rm = T))

all_collection_times <- cleaned_data_adm1_hrp%>%
  questionr::tabs(x = "temps_collecte", y = "temps_eau", weight = "weights_sampling")

diarr_infra_sanitaire <- cleaned_data_adm1_hrp%>%
  tabs(x = "enfant_selle", y = "infra_sanitaire", weight = "weights_sampling")

diarr_pasLatrine <- cleaned_data_adm1_hrp%>%
  mutate(presence_latrine = case_when(
    infra_sanitaire %in% c("lat_prive", "lat_privep", "lat_publiq", "toilette") ~ infra_sanitaire,
    infra_sanitaire %in% c("dal_eau", "dal_precis", "dal_zonep", "dal_zonep_am") ~ "DAL",
    TRUE ~ NA_character_
  ),
  presence_latrine_num = case_when(
    infra_sanitaire %in% c("lat_prive", "lat_privep", "lat_publiq", "toilette") ~ 1L,
    infra_sanitaire %in% c("dal_eau", "dal_precis", "dal_zonep", "dal_zonep_am") ~ 0L,
    TRUE ~ NA_integer_
  ),
  diarr = case_when(enfant_selle >0 ~ 1L,
                    enfant_selle == 0 ~ 0L,
                    TRUE ~ NA_integer_),
  prop_diar = enfant_selle/total_moins_5ans
  )
Jaccard = function (x, y) {
  M.11 = sum(x == 1 & y == 1)
  M.10 = sum(x == 1 & y == 0)
  M.01 = sum(x == 0 & y == 1)
  return (M.11 / (M.11 + M.10 + M.01))
}
diarr_pasLatrine_jaccard <- diarr_pasLatrine%>%select(diarr, presence_latrine_num)%>%filter(complete.cases(.))
Jaccard(diarr_pasLatrine_jaccard$diarr, diarr_pasLatrine_jaccard$presence_latrine_num)

diarr_pasLatrine_freq <- diarr_pasLatrine%>%
  tabs(x = "prop_diar", y = "presence_latrine", weight = "weights_sampling")

diarr_pasLatrine_plot <- ggplot(diarr_pasLatrine, aes(x = prop_diar, y = presence_latrine_num))+
  geom_jitter()+
  geom_smooth(method = lm)

diarr_pasLatrine_chisq <- questionr::chisq.residuals(diarr_pasLatrine_freq)

diarr_pasLatrine_subsetted <- diarr_pasLatrine%>%
  filter(prop_diar > 0)
  

print(cor.test(diarr_pasLatrine$prop_diar, diarr_pasLatrine$presence_latrine_num ))

insufEau_lavageMains <- cleaned_data_adm1_hrp%>%
  mutate(
    eau_suffi_num = case_when(
      eau_suffi == "plus_suffisant" ~ 5L,
      eau_suffi == "suffisant" ~ 4L,
      eau_suffi == "juste_suffisant" ~ 3L,
      eau_suffi == "insuffisant" ~ 2L,
      eau_suffi == "pas_suffisant" ~ 1L,
      TRUE ~ NA_integer_
    )
  )

insufEau_lavageMains_freq <- insufEau_lavageMains%>%
  tabs(x = "eau_suffi", y = "auMoins_aprLat_avantMan", weight = "weights_sampling")

cor.test(insufEau_lavageMains$eau_suffi_num, insufEau_lavageMains$auMoins_aprLat_avantMan)


diarr_lavageMains <- cleaned_data_adm1_hrp%>%
  mutate(
      diarr = case_when(enfant_selle >0 ~ 1L,
                        enfant_selle == 0 ~ 0L,
                        TRUE ~ NA_integer_),
      prop_diar = enfant_selle/total_moins_5ans
      )


diarr_lavageMains_freq <- diarr_lavageMains%>%
  tabs(x = "prop_diar", y = "auMoins_aprLat_avantMan", weight = "weights_sampling")

latrine_abris <- cleaned_data_adm1_hrp%>%
  tabs(x = "type_abri", y = "infra_sanitaire", weight = "weights_sampling")

write.csv(latrine_abris, "outputs/tables/EHA/latrine_abris.csv")

latrine_vbg <- cleaned_data_adm1_hrp%>%
  tabs(x = "infra_sanitaire", y = "vbg", weight = "weights_sampling")
write.csv(latrine_vbg, "outputs/tables/EHA/latrine_vbg.csv")

distance_eau_vbg <- cleaned_data_adm1_hrp%>%
  mutate(temps_total_eau_num = case_when(temps_total_eau == "eau_concession"~1L,
                                         temps_total_eau == "moins_5mn"~2L,
                                         temps_total_eau == "entre_5_15mn"~3L,
                                         temps_total_eau == "entre_16_30mn" ~ 4L,
                                         temps_total_eau == "entre_31_45mn" ~ 5L,
                                         temps_total_eau == "plus_46mn" ~ 5L,
                                         TRUE ~NA_integer_
                                         
  ))

distance_eau_vbg_freq <- cleaned_data_adm1_hrp%>%
  tabs(x = "temps_total_eau", y = "vbg", weight = "weights_sampling")
write.csv(distance_eau_vbg_freq, "outputs/tables/EHA/distance_eau_vbg.csv")


titreProp_latrine <- cleaned_data_adm1_hrp%>%
  tabs(x = "titre_propriete", y = "infra_sanitaire", weight = "weights_sampling")
write.csv(titreProp_latrine, "outputs/tables/EHA/titreProp_latrine.csv")

HHS_pointEau <- cleaned_data_adm1_hrp%>%
  tabs(x = "hhs_thresholds", y = "eau_suffi", weight = "weights_sampling")
write.csv(HHS_pointEau, "outputs/tables/EHA/HHS_pointEau.csv")

latrine_hygenique_moins20_grp  <- srv_cleaned_data_adm1_hrp%>%
  group_by(status)%>%
  summarise(latrine_hygenique_moins20 = survey_mean(latrine_hygenique_moins20, na.rm = T))
latrine_hygenique_moins20  <- srv_cleaned_data_adm1_hrp%>%
  summarise(latrine_hygenique_moins20 = survey_mean(latrine_hygenique_moins20, na.rm = T))




srv_cleaned_data_adm1 <- srvyr::as_survey_design(cleaned_data_adm1, weights = "weights_sampling")

jmp_ladder <- cleaned_data_adm1%>%
  mutate(jmp_ladder = as.factor(case_when(
                              typologie_source_eau == "amelioree" & temps_total_eau == "eau_concession" ~ "safely_managed",
                              typologie_source_eau == "amelioree" & temps_total_eau %in% c("moins_5mn", "entre_5_15mn", "entre_16_30mn") ~ "basic",
                              typologie_source_eau == "amelioree" & temps_total_eau %in% c("entre_31_45mn", "plus_46mn") ~ "limited",
                              typologie_source_eau == "non_amelioree" ~ "unimproved",
                              typologie_source_eau == "surface" ~ "surface",
                              TRUE ~ NA_character_
    
  )))%>%
  tabs(x = "jmp_ladder", y = "admin0", weight = "weights_sampling")

# cleaned_data_adm1 <- 
#   mutate(
# WASH PIN Calculations
# water_access = case_when(
#                           typologie_source_eau == "amelioree" & temps_total_eau == "eau_concession" ~ "1",
#                           typologie_source_eau == "amelioree" & temps_total_eau %in% c("moins_5mn", "entre_5_15mn", "entre_16_30mn") ~ "2",
#                           typologie_source_eau == "amelioree" & temps_total_eau %in% c("entre_31_45mn", "plus_46mn") ~ "3",
#                           typologie_source_eau == "non_amelioree" ~ "4",
#                           typologie_source_eau == "surface" ~ "4+",
#                           TRUE ~ NA_character_
#                           ),
#  latrine_shared = case_when(
#                           infra_sanitaire,  %in% c("lat_prive", "toilette") & is.na(type_latrine) ~ "ind",
#                           type_latrine == "lat_ind" ~ "ind",
#                           TRUE ~ type_latrine
#  ),
#  sanitation_access = case_when(
#                           infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "ind" ~ "1",
#                           infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com_20" ~ "2",
#                           infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com_50" ~ "3",
#                           infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com" ~ "4",
#                           infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ "4+",
#                           TRUE ~ NA_character_
#                           ),
#  diarr_prevalence = case_when(
#                           total_moins_5ans >= 1 & enfant_selle >= 1 ~ "3",
#                           total_moins_5ans >= 1 & enfant_selle == 0 ~ "1",
#                           total_moins_5ans == 0 ~ NA_integer_,
#                           TRUE ~ NA_integer_ 
#  )
# )


type_attente_total <- srv_cleaned_data_adm1 %>%
  filter(!is.na(source_eau), !is.na(temps_total_eau)) %>% 
  group_by(source_eau, temps_total_eau) %>% 
  summarise(pourcentage = survey_mean()) %>% 
  select(-pourcentage_se) %>% 
  group_by(source_eau) %>%
  mutate(pourcentage = case_when(is.na(pourcentage) ~ 0,
                                 TRUE ~ pourcentage)) %>% 
  pivot_wider(names_from = temps_total_eau, values_from = pourcentage) %>% 
  relocate(source_eau, c("eau_concession","moins_5mn", "entre_5_15mn", "entre_16_30mn",
                         "entre_31_45mn", "plus_46mn")) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))  


type_attente_total <- srv_cleaned_data_adm1 %>%
  filter(!is.na(source_eau), !is.na(temps_total_eau)) %>% 
  group_by(source_eau, temps_total_eau) %>% 
  summarise(pourcentage = survey_mean()) %>% 
  select(-pourcentage_se) %>% 
  group_by(source_eau) %>%
  mutate(pourcentage = case_when(is.na(pourcentage) ~ 0,
                                 TRUE ~ pourcentage)) %>% 
  pivot_wider(names_from = temps_total_eau, values_from = pourcentage) %>% 
  relocate(source_eau, c("eau_concession","moins_5mn", "entre_5_15mn", "entre_16_30mn",
                         "entre_31_45mn", "plus_46mn")) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))  

write_csv(type_attente_total, "source_eau_temps_total.csv")



type_attente_distance <- srv_cleaned_data_adm1 %>%
  filter(!is.na(source_eau), !is.na(temps_eau)) %>% 
  group_by(source_eau, temps_eau) %>% 
  summarise(pourcentage = survey_mean()) %>% 
  select(-pourcentage_se) %>% 
  group_by(source_eau) %>%
  mutate(pourcentage = case_when(is.na(pourcentage) ~ 0,
                                 TRUE ~ pourcentage)) %>% 
  pivot_wider(names_from = temps_eau, values_from = pourcentage) %>% 
  relocate(source_eau, c("eau_concession","moins_5mn", "entre_5_15mn", "entre_16_30mn",
                         "entre_31_45mn", "plus_46mn")) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))  

write_csv(type_attente_distance, "source_eau_temps_distance.csv")


type_attente_collect <- srv_cleaned_data_adm1 %>%
  filter(!is.na(source_eau), !is.na(temps_collecte)) %>% 
  group_by(source_eau, temps_collecte) %>% 
  summarise(pourcentage = survey_mean()) %>% 
  select(-pourcentage_se) %>% 
  group_by(source_eau) %>%
  mutate(pourcentage = case_when(is.na(pourcentage) ~ 0,
                                 TRUE ~ pourcentage)) %>% 
  pivot_wider(names_from = temps_collecte, values_from = pourcentage) %>% 
  relocate(source_eau, c("eau_concession","moins_5mn", "entre_5_15mn", "entre_16_30mn",
                         "entre_31_45mn", "plus_46mn")) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

write_csv(type_attente_collect, "source_eau_temps_collecte.csv")

