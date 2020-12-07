### The commented lines below should be uncommented to run the script from scratch


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
