
source("01_PrepingAnalysis.R", encoding = "UTF-8")



analysisplan_admin_2_grp <- make_analysis_plan_template(df= cleaned_data_adm2,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin2",
                                                        independent.variable = "status",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

analysisplan_admin_2_grp <- analysisplan_admin_2_grp[!is.na(analysisplan_admin_2_grp$dependent.variable.type),]
final_result_admin_2_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
                                                            analysisplan = analysisplan_admin_2_grp,
                                                            weighting = combined_weights_adm2,
                                                            questionnaire = questionnaire)

saveRDS(final_result_admin_2_grp, "outputs/final_result_admin_2_grp.rds")

final_result_admin_2_grp <- readRDS("outputs/final_result_admin_2_grp.rds")

summary_stats_admin_2_final_grp <- format_results(cleaned_data_adm2, final_result_admin_2_grp, aggregate_level = "admin2", pop_grp = "status", weights = "weights_sampling", csv = TRUE)
