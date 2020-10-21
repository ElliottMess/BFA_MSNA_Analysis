### The commented lines below should be uncommented to run the script from scratch


source("01_PrepingAnalysis.R", encoding = "UTF-8")

cleaned_data_adm1$admin0 <- "BFA"

cleaned_data_adm1_hrp <- cleaned_data_adm1%>%
  filter(admin1%in% c("centre_est", "boucle_du_mouhoun", "est", "sahel", "cascades", "nord", "centre_nord"))

analysisplan_admin_0 <- make_analysis_plan_template(df= cleaned_data_adm1_hrp,
                                                    questionnaire = questionnaire,
                                                    repeat.for.variable = "admin0",
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]
write_csv(analysisplan_admin_0, "data/analysis_plans/analysisplan_admin_0.csv")



final_result_admin_0_hrp_admin1 <- from_analysisplan_map_to_output(data = cleaned_data_adm1_hrp,
                                                            analysisplan = analysisplan_admin_0,
                                                            weighting = combined_weights_adm1,
                                                            questionnaire = questionnaire)
saveRDS(final_result_admin_0_hrp_admin1, "outputs/final_result_admin_0_hrp_admin1.RDS")

final_result_admin_0_hrp_admin1 <- readRDS("outputs/final_result_admin_0_hrp_admin1.RDS")

summary_stats_admin_0_hrp_admin1 <- format_results(cleaned_data_adm1_hrp, final_result_admin_0_hrp_admin1, aggregate_level = "admin0", weights = "weights_sampling", csv = TRUE)
