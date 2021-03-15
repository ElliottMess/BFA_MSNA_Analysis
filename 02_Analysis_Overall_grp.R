### The commented lines below should be uncommented to run the script from scratch


source("01_PrepingAnalysis.R", encoding = "UTF-8")
source("utils.R")
source("analysisplan_factory.R", encoding = "UTF-8")
source("analysis_functions.R", encoding = "UTF-8")


cleaned_data_adm1$admin0 <- "BFA"

# analysisplan_admin_0_grp <- make_analysis_plan_template(df= cleaned_data_adm1,
#                                                         questionnaire = questionnaire,
#                                                         repeat.for.variable = "admin0",
#                                                         independent.variable = "status",
#                                                         hypothesis.type = "direct_reporting",
#                                                         template_file = template_analysisplan_file
# )
# analysisplan_admin_0_grp <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]
# write_csv(analysisplan_admin_0_grp, "data/analysis_plans/analysisplan_admin_0_grp.csv")
# 
# final_result_admin_0_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm1,
#                                                             analysisplan = analysisplan_admin_0_grp,
#                                                             weighting = combined_weights_adm1,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_0_grp, "outputs/final_result_admin_0_grp.RDS")

final_result_admin_0_grp <- readRDS("outputs/final_result_admin_0_grp.RDS")


summary_stats_admin_0_final_grp <- format_results(cleaned_data_adm1, final_result_admin_0_grp, aggregate_level = "admin0", pop_grp = "status", weights = "weights_sampling", csv = TRUE)
