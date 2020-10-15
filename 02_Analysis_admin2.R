### The commented lines below should be uncommented to run the script from scratch

# source("01_PrepingAnalysis.R", encoding = "UTF-8")


# analysisplan_admin_2 <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                     questionnaire = questionnaire,
#                                                     repeat.for.variable = "admin2",
#                                                     hypothesis.type = "direct_reporting",
#                                                     template_file = template_analysisplan_file
# )
# analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]
# 
# 
# 
# final_result_admin_2 <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                         analysisplan = analysisplan_admin_2,
#                                                         weighting = combined_weights_adm2,
#                                                         questionnaire = questionnaire)


saveRDS(final_result_admin_2, "outputs/final_result_admin_2.RDS")


final_result_admin_2 <- readRDS("outputs/final_result_admin_2.rds")

summary_stats_admin_2_final <- format_results(cleaned_data_adm2, final_result_admin_2, aggregate_level = "admin2", weights = "weights_sampling", csv = TRUE)
