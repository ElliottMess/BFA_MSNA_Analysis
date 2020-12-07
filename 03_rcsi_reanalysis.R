
source("utils.R")

## Creating list of packages to load
package_list <- c("elliottmess/butteR", "hypegrammaR","ellieallien/cleaninginspectoR","hunzikp/rtree", "impact-initiatives/koboloops",
                  "tidyr","dplyr", "ggplot2", "readr", "stringr", "lubridate", "readxl", "rgdal", "sf", "purrr", "sdcMicro")

## Running the packaging loading function
loadInstall_package(package_list)


source("01_PrepingAnalysis.R", encoding = "UTF-8")
source("utils.R")
source("analysisplan_factory.R", encoding = "UTF-8")
source("analysis_functions.R", encoding = "UTF-8")


# analysisplan_admin_2_grp <- make_analysis_plan_template(df= cleaned_data_adm2,
#                                                         questionnaire = questionnaire,
#                                                         repeat.for.variable = "admin2",
#                                                         independent.variable = "status",
#                                                         hypothesis.type = "group_difference",
#                                                         template_file = template_analysisplan_file
# )
# analysisplan_admin_2_grp <- analysisplan_admin_2_grp[!is.na(analysisplan_admin_2_grp$dependent.variable.type),]
# write.csv(analysisplan_admin_2_grp, "data/analysis_plans/analysisplan_admin_2_grp.csv")
# analysisplan_admin_2_grp_rcsi <- read.csv("data/analysis_plans/analysisplan_admin_2_grp.csv")%>%
#   filter(dependent.variable == "rcsi_thresholds")
# final_result_admin_2_grp_rcsi <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                             analysisplan = analysisplan_admin_2_grp_rcsi,
#                                                             weighting = combined_weights_adm2,
#                                                             questionnaire = questionnaire)

# saveRDS(final_result_admin_2_grp_rcsi, "outputs/final_result_admin_2_grp_rcsi.rds")

final_result_admin_2_grp_rcsi <- readRDS("outputs/final_result_admin_2_grp_rcsi.rds")

summary_stats_admin_2_final_grp_rcsi <- format_results(cleaned_data_adm2, final_result_admin_2_grp_rcsi,
                                                       aggregate_level = "admin2", pop_grp = "status", weights = "weights_sampling",
                                                       csv = TRUE, questionnaire = questionnaire)%>%
  filter(str_detect(Indicator, "rCSI seuils"))%>%
  write_csv("outputs/tables/summary_stats_admin2_grp.csv")


# 
# analysisplan_admin_2_rcsi <- read_csv("data/analysis_plans/analysisplan_admin_2.csv")%>%
#   filter(dependent.variable == "rcsi_thresholds")

# final_result_admin_2_rcsi <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                         analysisplan = analysisplan_admin_2_rcsi,
#                                                         weighting = combined_weights_adm2,
#                                                         questionnaire = questionnaire)

# saveRDS(final_result_admin_2_rcsi, "outputs/final_result_admin_2_rcsi.RDS")
# final_result_admin_2_rcsi <- readRDS("outputs/final_result_admin_2_rcsi.RDS")



# summary_stats_admin_2_rcsi <- format_results(cleaned_data_adm2, final_result_admin_2_rcsi, pop_grp = NULL, aggregate_level = "admin2", weights = "weights_sampling", csv = TRUE)
# %>%
#   filter(str_detect(Indicator, "rCSI seuils"))%>%
#   write_csv("outputs/tables/summary_stats_admin2_rcsi.csv")


analysisplan_admin_1_rcsi <- read_csv("data/analysis_plans/analysisplan_admin_1.csv")%>%
    filter(dependent.variable == "rcsi_thresholds")
  
# final_result_admin_1_rcsi <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_1_rcsi,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_1_rcsi, "outputs/final_result_admin_1_rcsi.RDS")

final_result_admin_1_rcsi <- readRDS("outputs/final_result_admin_1_rcsi.RDS")

summary_stats_admin_1_final_rcsi <- format_results(cleaned_data_adm1, final_result_admin_1_rcsi, 
                                              aggregate_level = "admin1", weights = "weights_sampling", csv = TRUE, questionnaire = questionnaire)%>%
  filter(str_detect(Indicator, "rCSI seuils"))%>%
  write_csv("outputs/tables/summary_stats_admin_1_final_rcsi.csv")


analysisplan_admin_1_grp_rcsi <- read_csv("data/analysis_plans/analysisplan_admin_1_grp.csv")%>%
  filter(dependent.variable == "rcsi_thresholds")

# final_result_admin_1_grp_rcsi <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_1_grp_rcsi,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_1_grp_rcsi, "outputs/final_result_admin_1_grp_rcsi.RDS")

final_result_admin_1_grp_rcsi <- readRDS("outputs/final_result_admin_1_grp_rcsi.RDS")


summary_stats_admin_1_final_grp <- format_results(cleaned_data_adm1, final_result_admin_1_grp_rcsi,
                                                  aggregate_level = "admin1", pop_grp = "status", weights = "weights_sampling",
                                                  csv = FALSE, questionnaire = questionnaire)%>%
  filter(str_detect(Indicator, "rCSI seuils"))%>%
  write_csv("outputs/tables/summary_stats_admin_1_grp_final_rcsi.csv")


