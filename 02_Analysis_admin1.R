### The commented lines below should be uncommented to run the script from scratch
library(srvyr)

source("01_PrepingAnalysis.R", encoding = "UTF-8")
# analysisplan_admin_1 <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                         questionnaire = questionnaire,
#                                                         repeat.for.variable = "admin1",
#                                                         hypothesis.type = "direct_reporting",
#                                                         template_file = template_analysisplan_file
# )
# 
# analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]
# write.csv(analysisplan_admin_1, "data/analysis_plans/analysisplan_admin_1.csv")
# final_result_admin_1 <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_1,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_1, "outputs/final_result_admin_1.RDS")

final_result_admin_1 <- readRDS("outputs/final_result_admin_1.RDS")

summary_stats_admin_1_final <- format_results(cleaned_data_adm1, final_result_admin_1, 
                                                  aggregate_level = "admin1", weights = "weights_sampling", csv = TRUE, questionnaire = questionnaire)

weights_cluster_id <- select(cleaned_data_adm1,sampling_id, weights_sampling, index ) %>% 
  mutate(index = as.numeric(index))

indiv_data <- read_csv("data/bfa2002_msna_2020_final_cleaning_20200913_PH_MF_info_menage.csv") %>% 
  dplyr::rename(submission_uuid = `_submission__uuid`, 
                parent_index = `_parent_index`) %>%
  filter(parent_index %in% cleaned_data_adm1$index) %>%
  left_join(weights_cluster_id, by = c("parent_index" = "index")) %>% 
  mutate(femmes_60plus = case_when(sexe_hh == "femme" & age_hh >= 60 ~ 1,
                                   TRUE ~ 0),
         femmes_18_59 = case_when(
           sexe_hh == "femme" & age_hh >= 18 & age_hh < 60 ~ 1,
           TRUE ~ 0),
         femmes_6_17 = case_when(
           sexe_hh == "femme" & age_hh >= 6 & age_hh < 18 ~ 1,
           TRUE ~ 0),
         femmes_0_5 = case_when(
           sexe_hh == "femme" & age_hh >= 0 & age_hh < 6 ~ 1,
           TRUE ~ 0),
         hommes_60plus = case_when(sexe_hh == "homme" & age_hh >= 60 ~ 1,
                                   TRUE ~ 0),
         hommes_18_59 = case_when(
           sexe_hh == "homme" & age_hh >= 18 & age_hh < 60 ~ 1,
           TRUE ~ 0),
         hommes_6_17 = case_when(
           sexe_hh == "homme" & age_hh >= 6 & age_hh < 18 ~ 1,
           TRUE ~ 0),
         hommes_0_5 = case_when(
           sexe_hh == "homme" & age_hh >= 0 & age_hh < 6 ~ 1,
           TRUE ~ 0)
         )

demo_data_indiv <- indiv_data %>% 
  as_survey(ids = sampling_id, weights = weights_sampling) %>% 
  summarise(across(starts_with("femmes_"), survey_mean),
            across(starts_with("hommes_"), survey_mean)
  ) %>%
  rename_with(~paste0("perc_", .x)) %>% 
  select(-ends_with("_se")) %>% 
  mutate(perc_femme = rowSums(select(., starts_with("femmes_"))),
         perc_homme = rowSums(select(., starts_with("hommes_"))))

demo_data_hh <- cleaned_data_adm1 %>%
  mutate( femme_cheffe_menage = case_when(
    genre_chef_menage == "femme" ~ 1L,
    genre_chef_menage == "homme" ~ 0L,
    TRUE ~ NA_integer_
  ),
  femme_cheffe_menage_sans_partenaire = case_when(
    genre_chef_menage == "femme" & situation_matrimoniale %in% c("marie_veuf", "celibataire", "divorce") ~ 1L,
    genre_chef_menage == "homme" | (genre_chef_menage == "femme"& !situation_matrimoniale %in% c("marie_veuf", "celibataire", "divorce"))~ 0L,
    TRUE ~ NA_integer_
  )) %>% 
  as_survey(ids = sampling_id, weights = weights_sampling) %>%
  summarise(taille_moyenne_menage = survey_mean(taille_menage),
            perc_menages_diriges_femme = survey_mean(femme_cheffe_menage, na.rm = T),
            perc_femme_cheffe_menage_sans_partenaire = survey_mean(femme_cheffe_menage_sans_partenaire, na.rm = T)) %>% 
  select(-ends_with("_se"))

demo_data_combined <- cbind(demo_data_indiv, demo_data_hh)

femme_cheffe <- cleaned_data_adm1 %>%
  mutate(
    femme_cheffe_menage = case_when(
      genre_chef_menage == "femme" ~ 1L,
      genre_chef_menage == "homme" ~ 0L,
      TRUE ~ NA_integer_)
    )%>% 
    as_survey(ids = sampling_id, weights = weights_sampling) %>%
  summarise(perc_femme_cheffe_menage = survey_mean(femme_cheffe_menage, na.rm = T))

write_csv(demo_data_combined, "outputs/tables/demo_info_admin0.csv")
