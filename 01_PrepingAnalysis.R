
source("analysisplan_factory.R")

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
  filter(!dependent.variable %in% names(cleaned_data_adm1))%>%
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
