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


### The commented lines below should be uncommented to run the script from scratch
# analysisplan_admin_2 <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                     questionnaire = questionnaire,
#                                                     repeat.for.variable = "admin2",
#                                                     hypothesis.type = "direct_reporting",
#                                                     template_file = template_analysisplan_file
# )
# analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]
# write.csv(analysisplan_admin_2, "data/analysis_plans/analysisplan_admin_2.csv")
# Calculating the analysis plan 
# final_result_admin_2 <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                         analysisplan = analysisplan_admin_2,
#                                                         weighting = combined_weights_adm2,
#                                                         questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_2, "outputs/final_result_admin_2.RDS")


final_result_admin_2 <- readRDS("outputs/final_result_admin_2.rds")

summary_stats_admin_2_final <- format_results(cleaned_data_adm2, final_result_admin_2, aggregate_level = "admin2", weights = "weights_sampling", csv = TRUE)
