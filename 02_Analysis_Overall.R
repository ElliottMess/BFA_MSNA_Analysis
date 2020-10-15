source("01_PrepingAnalysis.R", encoding = "UTF-8")
source("analysisplan_factory.R", encoding = "UTF-8")
source("analysis_functions.R", encoding = "UTF-8")

cleaned_data_adm1$admin0 <- "BFA"

### The commented lines below should be uncommented to run the script from scratch

# analysisplan_admin_0 <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                     questionnaire = questionnaire,
#                                                     repeat.for.variable = "admin0",
#                                                     hypothesis.type = "direct_reporting",
#                                                     template_file = template_analysisplan_file
# )
# analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]
# write_csv(analysisplan_admin_0, "data/analysis_plans/analysisplan_admin_0.csv")
# 
# 
# 
# final_result_admin_0 <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_0,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_0, "outputs/final_result_admin_0.RDS")

cleaned_data_adm1$admin0 <- "BFA"

final_result_admin_0 <- readRDS("outputs/final_result_admin_0.RDS")

summary_stats_admin_0_final <- format_results(cleaned_data_adm1, final_result_admin_0, aggregate_level = "admin0", weights = "weights_sampling", csv = TRUE)

