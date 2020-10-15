source("01_PrepingAnalysis.R", encoding = "UTF-8")

# 
# analysisplan_admin_1_grp <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                         questionnaire = questionnaire,
#                                                         repeat.for.variable = "admin1",
#                                                         independent.variable = "status",
#                                                         hypothesis.type = "group_difference",
#                                                         template_file = template_analysisplan_file
# )
# 
# analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!is.na(analysisplan_admin_1_grp$dependent.variable.type),]
# 
# 
# 
# final_result_admin_1_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_1_grp,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_1_grp, "outputs/final_result_admin_1_grp.RDS")

final_result_admin_1_grp <- readRDS("outputs/final_result_admin_1_grp.RDS")


summary_stats_admin_1_final_grp <- format_results(cleaned_data_adm1, final_result_admin_0_grp, aggregate_level = "admin1", pop_grp = "status", weights = "weights_sampling", csv = TRUE)
