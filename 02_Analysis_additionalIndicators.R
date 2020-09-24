
source("analysisplan_factory.R")

source("utils.R")

### Loading necessary files

template_analysisplan_file <- "./data/analysis_plan_allVars_template.csv"
analysisplanTemplate <- read_csv(template_analysisplan_file, locale = locale(encoding = "UTF-8"))

cleaned_data_adm1 <- read_csv("outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM1.csv", locale = locale(encoding = "latin1"))
cleaned_data_adm2 <- read_csv("outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM2_all.csv", locale = locale(encoding = "latin1"))
choices <- read_csv("data/choices.csv", locale = locale(encoding = "UTF-8"))
survey <- read_csv("data/survey.csv", locale = locale(encoding = "UTF-8"))

questionnaire <- load_questionnaire(cleaned_data_adm1, choices = choices, questions = survey)

added_indicators <- analysisplanTemplate%>%
  filter(dependent.variable %in% c("fcs2", "fcs2_thresholds", "hhs_score", "hhs_thresholds",
                                                                                       "lcs_crise", "lcs_minimal", "lcs_stress", "lcs_total", "lcs_urgence", "lcsi",
                                                                                       "hhs_thresholds", "hhs_score","rcsi_thresholds", "rcsi_score"))%>%
  mutate(choices = choices_name,
         type = case_when(is.na(choices) & dependent.variable.type == "numerical" ~ "decimal",
                          !is.na(choices) & dependent.variable.type == "categorical"~ paste0("select_multiple liste_", dependent.variable),
                          TRUE ~ choices
         ))%>%
  separate(type, into=c("qtype", "list_name"), sep = " ", remove = FALSE)


added_indicators_survey <- added_indicators%>%
  mutate(
    hint = NA, required = NA, relevant = NA, choice_filter = NA, calculation = NA, constraint_message = NA,
    constraint = NA,`$given_name`  = NA, repeat_count = NA, default = NA, appearance = NA
  )%>%
  rename(name = dependent.variable, label = label_indicator)%>%
  select(type,name,label,hint,required,relevant,choice_filter,calculation,constraint_message,
         constraint,`$given_name`,repeat_count,default, appearance)%>%
  distinct()


survey_full <- rbind(survey, added_indicators_survey)

added_indicators_choices <- added_indicators%>%
  rename(name = choices_name, label = choices_label )%>%
  mutate(info_admin3 = NA, info_admin2 = NA, info_admin1 = NA, info_base = NA)%>%
  select(names(choices))

choices_full <- rbind(choices, added_indicators_choices)


dico <- form_dictionnary(cleaned_data_adm1, survey_full, choices_full, analysisplanTemplate)

running_timezz <- data.frame(matrix(ncol = 4, nrow = 6) )

names(running_timezz) <- c("Level", "Start", "End", "Running_time")


analysisplan_admin_1_grp <- make_analysis_plan_template(df= cleaned_data_adm1,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin1",
                                                        independent.variable = "status",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!is.na(analysisplan_admin_1_grp$dependent.variable.type),]

analysisplan_admin_1_grp_secal <- analysisplan_admin_1_grp %>%
  filter(dependent.variable %in% c("fcs2", "fcs2_thresholds", "hhs_score", "hhs_thresholds",
                                   "lcs_crise", "lcs_minimal", "lcs_stress", "lcs_total", "lcs_urgence", "lcsi",
                                   "hhs_thresholds", "hhs_score","rcsi_thresholds", "rcsi_score"))

final_result_admin_1_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
                                                            analysisplan = analysisplan_admin_1_grp_secal,
                                                            weighting = combined_weights_adm1,
                                                            questionnaire = questionnaire)



summary_stats_admin_1_grp <- final_result_admin_1_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  select(repeat.var.value, independent.var.value,dependent.var, dependent.var.value, numbers)%>%
  rename(admin1 = repeat.var.value, status = independent.var.value, variable = dependent.var, variable_value = dependent.var.value)

n_adm1 <- cleaned_data_adm1%>%
  group_by(admin1, status)%>%
  summarise(n = n(), .groups="drop")%>%
  pivot_longer(c(-admin1, -status), names_to = "variable", values_to = "n")%>%
  select(-variable)

which_subsets_adm1 <- cleaned_data_adm1%>%
  group_by(admin1, status)%>%
  summarise_all(funs(sum(is.na(.))))%>%
  pivot_longer(c(-admin1, -status), names_to = "variable", values_to = "NAs")%>%
  left_join(n_adm1, by = c("admin1", "status"))%>%
  mutate(perc_NAs = round(NAs/n*100, 0))%>%
  select(-n)

which_skipLogic <- map(names(cleaned_data_adm1%>%select(-eau_propre, -admin1, -admin2, -status)), function(x)questionnaire$question_is_skipped(question.name = x, data = cleaned_data_adm1%>%select(-eau_propre)))%>%
  as.data.frame()%>%
  mutate(admin1 = cleaned_data_adm1$admin1, admin2 = cleaned_data_adm1$admin2, status = cleaned_data_adm1$status)

names(which_skipLogic) <- c(names(cleaned_data_adm1%>%select(-eau_propre,  -admin1, -admin2, -status)), "admin1", "admin2", "status")

which_skipLogic_adm1 <- which_skipLogic%>%
  select(-admin2)%>%
  group_by(admin1, status)%>%
  summarise_all(funs(sum(.)))%>%
  pivot_longer(c(-admin1, -status), names_to = "variable", values_to = "Skipped")%>%
  left_join(n_adm1, by = c("admin1", "status"))%>%
  mutate(perc_Skipped = round(Skipped/n*100, 0))%>%
  select(-n)


bfa_admin1 <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
  select(admin1Name)%>%
  mutate(admin1name = tolower(admin1Name),
         admin1name = gsub("-","_", admin1name),
         admin1name = gsub(" ", "_", admin1name))%>%
  distinct()

target_research_question_order <- unique(dico$research.question_label)
target_sub_research_question_order <- unique(dico$sub.research.question_label)

which_skipLogic_adm1_var <- which_skipLogic_adm1 %>%
  group_by(variable) %>%
  summarise(Skipped = sum(Skipped, na.rm = T),
            perc_Skipped = sum(perc_Skipped, na.rm = T)) %>%
  mutate(subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))

summary_stats_admin_1_grp_final <- bind_rows(summary_stats_admin_1_grp)%>%
  left_join(which_subsets_adm1, by = c("admin1", "status", "variable"))%>%
  # left_join(which_skipLogic_adm1, by = c("admin1", "status", "variable"))%>%
  mutate(question_choice = case_when(is.na(variable_value) ~ variable,
                                     TRUE ~ paste0(variable, ".", variable_value))) %>%
  # ,
  # subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))%>%
  left_join(dico, by = "question_choice")%>%
  mutate(status = case_when(status == "pdi" ~ "PDI",
                            status == "host" ~ "Communauté hôte",
                            TRUE ~ NA_character_))%>%
  mutate(numbers = case_when(grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical"  ~ round(numbers*100,1),
                             !grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical" ~ round(numbers*100,0),
                             dependent.variable.type == "numerical"  ~ round(numbers,1)
  )
  )%>%
  select(research.question_label, sub.research.question_label, admin1, status, label_indicator, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin1, status, label_indicator, label_choice, numbers, subset)%>%
  left_join(bfa_admin1, by = c("admin1" = "admin1name"))%>%
  distinct()%>%
  mutate(label_choice = case_when(is.na(label_choice) ~ label_indicator, 
                                  TRUE ~ paste(label_indicator, label_choice, sep = ": ")))%>%
  select(research.question_label, sub.research.question_label, admin1Name, status, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin1Name, status, label_choice,subset, numbers)%>%
  distinct()%>%
  filter(!is.na(label_choice))%>%
  group_by(research.question_label, sub.research.question_label, admin1Name, status, label_choice)%>%
  pivot_wider(names_from = c(admin1Name), values_from = numbers)%>%
  # summarise(across(everything(), sum, na.rm=T))%>%
  ungroup()%>%
  mutate(sub.research.question_label = factor(sub.research.question_label, levels = target_sub_research_question_order ),
         research.question_label = factor(research.question_label, levels = target_research_question_order))%>%
  arrange(research.question_label,sub.research.question_label,status,label_choice)%>%
  filter(!is.na(label_choice)) %>%
  left_join(select(which_skipLogic_adm1_var, variable,subset) , by = "variable")%>%
  select(research.question_label, sub.research.question_label, status, label_choice,subset,everything())%>%
  select(-variable)




analysisplan_admin_2_grp <- make_analysis_plan_template(df= cleaned_data_adm2,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin2",
                                                        independent.variable = "status",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

analysisplan_admin_2_grp <- analysisplan_admin_2_grp[!is.na(analysisplan_admin_2_grp$dependent.variable.type),]

analysisplan_admin_2_grp_secal <- analysisplan_admin_2_grp %>%
  filter(dependent.variable %in% c("fcs2", "fcs2_thresholds", "hhs_score", "hhs_thresholds",
                                   "lcs_crise", "lcs_minimal", "lcs_stress", "lcs_total", "lcs_urgence", "lcsi",
                                   "hhs_thresholds", "hhs_score","rcsi_thresholds", "rcsi_score"))


final_result_admin_2_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
                                                            analysisplan = analysisplan_admin_2_grp_secal,
                                                            weighting = combined_weights_adm2,
                                                            questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_2_grp, "final_result_admin_2_grp.rds")

# final_result_admin_2_grp_weights_adm2 <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                             analysisplan = analysisplan_admin_2_grp,
#                                                             weighting = admin2_wght_adm2,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_2_grp_weights_adm2, "final_result_admin_2_grp_weights_adm2.rds")


summary_stats_admin_2_grp <- final_result_admin_2_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  select(repeat.var.value, independent.var.value,dependent.var, dependent.var.value, numbers)%>%
  rename(admin2 = repeat.var.value, status = independent.var.value, variable = dependent.var, variable_value = dependent.var.value)

bfa_admin2 <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
  select(admin1Name, admin2Name)%>%
  mutate(admin2name = tolower(admin2Name))%>%
  distinct()

n_adm2 <- cleaned_data_adm1%>%
  group_by(admin2, status)%>%
  summarise(n = n(), .groups="drop")%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "n")%>%
  select(-variable)

which_subsets_adm2 <- cleaned_data_adm1%>%
  group_by(admin2, status)%>%
  summarise_all(funs(sum(is.na(.))))%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "NAs")%>%
  left_join(n_adm2, by = c("admin2", "status"))%>%
  mutate(perc_NAs = round(NAs/n*100, 0))%>%
  select(-n)

which_skipLogic_admin2 <- map(names(cleaned_data_adm2%>%select(-eau_propre, -admin1, -admin2, -status)), function(x)questionnaire$question_is_skipped(question.name = x, data = cleaned_data_adm2%>%select(-eau_propre)))%>%
  as.data.frame()%>%
  mutate(admin1 = cleaned_data_adm2$admin1, admin2 = cleaned_data_adm2$admin2, status = cleaned_data_adm2$status)

names(which_skipLogic_admin2) <- c(names(cleaned_data_adm2%>%select(-eau_propre,  -admin1, -admin2, -status)), "admin1", "admin2", "status")


which_skipLogic_adm2 <- which_skipLogic_admin2%>%
  select(-admin1)%>%
  group_by(admin2, status)%>%
  summarise_all(funs(sum(.)))%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "Skipped")%>%
  left_join(n_adm2, by = c("admin2", "status"))%>%
  mutate(perc_Skipped = round(Skipped/n*100, 0))%>%
  select(-n)

which_skipLogic_adm2_var <- which_skipLogic_adm2 %>%
  group_by(variable) %>%
  summarise(Skipped = sum(Skipped, na.rm = T),
            perc_Skipped = sum(perc_Skipped, na.rm = T)) %>%
  mutate(subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))

summary_stats_admin_2_grp_final <- bind_rows(summary_stats_admin_2_grp)%>%
  left_join(which_subsets_adm2, by = c("admin2", "status", "variable"))%>%
  # left_join(which_skipLogic_adm2, by = c("admin2", "status", "variable"))%>%
  mutate(question_choice = case_when(is.na(variable_value) ~ variable,
                                     TRUE ~ paste0(variable, ".", variable_value))) %>%
  # ,
  # subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))%>%
  left_join(dico, by = "question_choice")%>%
  mutate(status = case_when(status == "pdi" ~ "PDI",
                            status == "host" ~ "Communauté hôte",
                            TRUE ~ NA_character_))%>%
  mutate(numbers = case_when(grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical"  ~ round(numbers*100,1),
                             !grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical" ~ round(numbers*100,0),
                             dependent.variable.type == "numerical"  ~ round(numbers,1)
  )
  )%>%
  select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, subset)%>%
  left_join(bfa_admin2, by = c("admin2" = "admin2name"))%>%
  distinct()%>%
  mutate(label_choice = case_when(is.na(label_choice) ~ label_indicator, 
                                  TRUE ~ paste(label_indicator, label_choice, sep = ": ")))%>%
  select(research.question_label, sub.research.question_label, admin2Name, status, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2Name, status, label_choice,subset, numbers)%>%
  distinct()%>%
  filter(!is.na(label_choice))%>%
  group_by(research.question_label, sub.research.question_label, admin2Name, status, label_choice)%>%
  pivot_wider(names_from = c(admin2Name), values_from = numbers)%>%
  # summarise(across(everything(), sum, na.rm=T))%>%
  ungroup()%>%
  mutate(sub.research.question_label = factor(sub.research.question_label, levels = target_sub_research_question_order ),
         research.question_label = factor(research.question_label, levels = target_research_question_order))%>%
  arrange(research.question_label,sub.research.question_label,status,label_choice)%>%
  filter(!is.na(label_choice)) %>%
  left_join(select(which_skipLogic_adm2_var, variable,subset) , by = "variable")%>%
  select(research.question_label, sub.research.question_label, status, label_choice,subset,everything())%>%
  select(-variable)

summary_stats_admin_2_grp_final_secal <- bind_rows(summary_stats_admin_2_grp)%>%
  left_join(which_subsets_adm2, by = c("admin2", "status", "variable"))%>%
  # left_join(which_skipLogic_adm2, by = c("admin2", "status", "variable"))%>%
  mutate(question_choice = case_when(is.na(variable_value) ~ variable,
                                     TRUE ~ paste0(variable, ".", variable_value))) %>%
  # ,
  # subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))%>%
  left_join(dico, by = "question_choice")%>%
  mutate(status = case_when(status == "pdi" ~ "PDI",
                            status == "host" ~ "Communauté hôte",
                            TRUE ~ NA_character_))%>%
  mutate(numbers = case_when(grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical"  ~ round(numbers*100,1),
                             !grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical" ~ round(numbers*100,0),
                             dependent.variable.type == "numerical"  ~ round(numbers,1)
  )
  )%>%
  select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, subset)%>%
  left_join(bfa_admin2, by = c("admin2" = "admin2name"))%>%
  distinct()%>%
  mutate(label_choice = case_when(is.na(label_choice) ~ label_indicator, 
                                  TRUE ~ paste(label_indicator, label_choice, sep = ": ")))%>%
  select(research.question_label, sub.research.question_label, admin2Name, status, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2Name, status, label_choice,subset, numbers)%>%
  distinct()%>%
  filter(!is.na(label_choice))%>%
  group_by(research.question_label, sub.research.question_label, admin2Name, status, label_choice)%>%
  pivot_wider(names_from = c(admin2Name), values_from = numbers)%>%
  # summarise(across(everything(), sum, na.rm=T))%>%
  ungroup()%>%
  mutate(sub.research.question_label = factor(sub.research.question_label, levels = target_sub_research_question_order ),
         research.question_label = factor(research.question_label, levels = target_research_question_order))%>%
  arrange(research.question_label,sub.research.question_label,status,label_choice)%>%
  filter(!is.na(label_choice)) %>%
  left_join(select(which_skipLogic_adm2_var, variable,subset) , by = "variable")%>%
  select(research.question_label, sub.research.question_label, status, label_choice,subset,everything())%>%
  select(-variable)


names(summary_stats_admin_2_grp_final)[1:5] <- c("Question de recherche", "Sous-question de recherche", "Groupe de population", "Indicator", "Sous-ensemble de donnée")
names(summary_stats_admin_1_grp_final)[1:5] <- c("Question de recherche", "Sous-question de recherche", "Groupe de population", "Indicator", "Sous-ensemble de donnée")

write_csv(summary_stats_admin_1_grp_final, "outputs/tables/summary_stats_admin_1_grp_secal.csv")
write_csv(summary_stats_admin_2_grp_final, "outputs/tables/summary_stats_admin_2_grp_secal.csv")

library(srvyr)

fcs_check_admin1_srvyr <- as_survey(cleaned_data_adm1, weights = weights_sampling)%>%
  select(admin1, status, fcs2_thresholds)%>%
  mutate(fcs2_thresholds = as.factor(fcs2_thresholds))%>%
  group_by(admin1, status,fcs2_thresholds)%>%
  summarise(prop = survey_mean())

library(questionr)
fcs_check_admin1_qustr <- questionr::tabs(cleaned_data_adm1%>%filter(admin1=="centre_est"),
                x = "fcs2_thresholds",
                y = "admin1",
                weight = "weights_sampling",
                na.rm = TRUE)

library(dplyr)
fcs_check_admin1_dplyr <- cleaned_data_adm1%>%
  filter(!is.na(fcs2_thresholds),admin1=="centre_est" )%>%
  group_by(admin1)%>%
  count(fcs2_thresholds,
        wt = weights_sampling)%>%
  mutate(prop = n/sum(n))
  
