
source("analysisplan_factory.R")
source("analysis_functions.R")


source("utils.R")

# source("00_loadCleanWeight.R", encoding = "UTF-8")


### Loading necessary files

template_analysisplan_file <- "./data/analysis_plan_allVars_template.csv"
analysisplanTemplate <- read_csv(template_analysisplan_file, locale = locale(encoding = "UTF-8"))

cleaned_data_adm1 <- read_csv("outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM1.csv", locale = locale(encoding = "latin1"))
cleaned_data_adm2 <- read_csv("outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM2_all.csv", locale = locale(encoding = "latin1"))
choices <- read_csv("data/choices.csv", locale = locale(encoding = "UTF-8"))
survey <- read_csv("data/survey.csv", locale = locale(encoding = "UTF-8"))

questionnaire <- load_questionnaire(cleaned_data_adm1, choices = choices, questions = survey)

# analysisplan_all_vars <- make_analysisplan_all_vars(df = cleaned_data, questionnaire = questionnaire, repeat.for.variable = "admin0")
# 
# human_questions <- survey%>%
#   select(name, label)
# 
# analysisplan_all_vars <- analysisplan_all_vars%>%
#   left_join(human_questions, by = c("dependent.variable" = "name"))
# 
# write_csv(analysisplan_all_vars, "analysis_plan_allVars_new.csv")

added_indicators <- analysisplanTemplate%>%
  filter(!dependent.variable %in% names(cleaned_data_adm1) | !dependent.variable %in% unique(survey$name))%>%
  mutate(choices = choices_name,
    type = case_when(     !is.na(qtype) & !is.na(list_name) ~ paste(qtype, list_name),
                          is.na(qtype) & is.na(choices) & dependent.variable.type == "numerical" ~ "decimal",
                          is.na(qtype) & !is.na(choices) & dependent.variable.type == "categorical"~ paste0("select_multiple liste_", dependent.variable),
                          TRUE ~ choices
                          ))%>%
  separate(type, into=c("qtype", "list_name"), sep = " ", remove = FALSE)

added_indicators_survey <- added_indicators%>%
  mutate(
         hint = NA, required = NA, relevant = NA, choice_filter = NA, calculation = NA, constraint_message = NA,
         constraint = NA,`$given_name`  = NA, repeat_count = NA, default = NA, appearance = NA
         )%>%
  dplyr::rename(name = dependent.variable, label = label_indicator)%>%
  select(type,name,label,hint,required,relevant,choice_filter,calculation,constraint_message,
         constraint,`$given_name`,repeat_count,default, appearance)%>%
  distinct()

added_indicators_survey[grep("taille_menage", added_indicators_survey$name), "type"] <- "select_one liste_taille_menage"
added_indicators_survey[grep("revenu_mensuel", added_indicators_survey$name), "type"] <- "select_one liste_revenu_mensuel"
added_indicators_survey[grep("ic_age", added_indicators_survey$name), "type"] <- "select_one liste_ic_age"
added_indicators_survey[grep("age_chef_menage", added_indicators_survey$name), "type"] <- "select_one liste_age_chef_menage"


survey_full <- rbind(survey, added_indicators_survey)

add_choices <- function(choices,data, question, name_list = NULL, type = "select_one"){
  
  unique_choices <- as.character(unique(as.data.frame(data)[, question]))
  
  choices%>%
    add_row(list_name = rep(name_list, length(unique_choices)),
            name = unique_choices,
            label = unique_choices,
            info_admin3 = NA, info_admin2 = NA,  info_admin1 = NA,  info_base = NA
    )
  
}


added_indicators_choices <- added_indicators%>%
  dplyr::rename(name = choices_name, label = choices_label )%>%
  mutate(info_admin3 = NA, info_admin2 = NA, info_admin1 = NA, info_base = NA)%>%
  select(names(choices))%>%
  distinct()%>%
  add_choices( data = cleaned_data_adm1, question = "taille_menage",name_list = "liste_taille_menage", type = "select_one")%>%
  add_choices( data = cleaned_data_adm1, question = "revenu_mensuel",name_list = "liste_revenu_mensuel", type = "select_one")%>%
  add_choices( data = cleaned_data_adm1, question = "ic_age",name_list = "liste_ic_age", type = "select_one")%>%
  add_choices( data = cleaned_data_adm1, question = "age_chef_menage",name_list = "liste_age_chef_menage", type = "select_one")

  
choices_full <- rbind(choices, added_indicators_choices)
  

dico <- form_dictionnary(cleaned_data_adm1, survey_full, choices_full, analysisplanTemplate)

questionnaire <- load_questionnaire(cleaned_data_adm1, choices = choices_full, questions = survey_full)
