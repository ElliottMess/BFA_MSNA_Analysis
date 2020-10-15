### The commented lines below should be uncommented to run the script from scratch


# source("01_PrepingAnalysis.R", encoding = "UTF-8")


analysisplan_admin_1 <- make_analysis_plan_template(df= cleaned_data_adm1,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin1",
                                                        hypothesis.type = "direct_reporting",
                                                        template_file = template_analysisplan_file
)

analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]
final_result_admin_1 <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
                                                            analysisplan = analysisplan_admin_1,
                                                            weighting = combined_weights_adm1,
                                                            questionnaire = questionnaire)

saveRDS(final_result_admin_1, "outputs/final_result_admin_1.RDS")

final_result_admin_1 <- readRDS("outputs/final_result_admin_1.RDS")

summary_stats_admin_1_final_grp <- format_results(cleaned_data_adm2, final_result_admin_1, aggregate_level = "admin2", pop_grp = "status", weights = "weights_sampling", csv = TRUE)
