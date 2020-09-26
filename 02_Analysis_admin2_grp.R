
source("01_PrepingAnalysis.R", encoding = "UTF-8")



analysisplan_admin_2_grp <- make_analysis_plan_template(df= cleaned_data_adm2,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin2",
                                                        independent.variable = "status",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

analysisplan_admin_2_grp <- analysisplan_admin_2_grp[!is.na(analysisplan_admin_2_grp$dependent.variable.type),]



# final_result_admin_2_grp <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                             analysisplan = analysisplan_admin_2_grp,
#                                                             weighting = combined_weights_adm2,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_2_grp, "final_result_admin_2_grp.rds")

# final_result_admin_2_grp_weights_adm2 <- from_analysisplan_map_to_output(data = cleaned_data_adm2,
#                                                             analysisplan = analysisplan_admin_2_grp,
#                                                             weighting = admin2_wght_adm2,
#                                                             questionnaire = questionnaire)
# 
# saveRDS(final_result_admin_2_grp_weights_adm2, "final_result_admin_2_grp_weights_adm2.rds")

final_result_admin_2_grp <- readRDS("final_result_admin_2_grp.rds")

summary_stats_admin_2_grp <- final_result_admin_2_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  select(repeat.var.value, independent.var.value,dependent.var, dependent.var.value, numbers)%>%
  rename(admin2 = repeat.var.value, status = independent.var.value, variable = dependent.var, variable_value = dependent.var.value)

freq_admin2_grp <- cleaned_data_adm2%>%
  group_by(admin2, status)%>%
  summarise(
    #Naissances
    freq_lieu_accouchement.centre_sante = sum(lieu_accouchement.centre_sante * weights_sampling, na.rm = T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_lieu_accouchement.maison = sum(lieu_accouchement.maison * weights_sampling, na.rm = T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_lieu_accouchement.autre = sum(lieu_accouchement.autre * weights_sampling, na.rm = T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.accouche_assiste_domicil = sum(raison_dominicile.accouche_assiste_domicil * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.accouche_centre_ferme = sum(raison_dominicile.centre_ferme * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.accouche_centre_financ_inacc = sum(raison_dominicile.centre_financ_inacc * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.centre_surpeuple = sum(raison_dominicile.centre_surpeuple * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.maternite_pas_sur = sum(raison_dominicile.maternite_pas_sur * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.physique_mere = sum(raison_dominicile.physique_mere * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.rejoindre_centre = sum(raison_dominicile.rejoindre_centre * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    freq_raison_domicil.nsp = sum(raison_dominicile.nsp * weights_sampling, na.rm=T)/sum(total_naissance * weights_sampling, na.rm = T),
    #Enfants malades
    freq_enfants_5ans_maladie.palu = sum(`maladie_moins_5ans/palu` * weights_sampling, na.rm=T)/sum(total_moins_5ans * weights_sampling, na.rm = T),
    freq_enfants_5ans_maladie.infect_respiratoire  = sum(`maladie_moins_5ans/infect_respiratoire` * weights_sampling, na.rm=T)/sum(total_moins_5ans * weights_sampling, na.rm = T),
    freq_enfants_5ans_maladie.diarrhee = sum(`maladie_moins_5ans/diarrhee` * weights_sampling, na.rm=T)/sum(total_moins_5ans * weights_sampling, na.rm = T),
    freq_enfants_5ans_maladie.autre = sum(`maladie_moins_5ans/autre` * weights_sampling, na.rm=T)/sum(total_moins_5ans * weights_sampling, na.rm = T),
    freq_enfants_5ans_maladie.nsp = sum(`maladie_moins_5ans/nsp` * weights_sampling, na.rm=T)/sum(total_moins_5ans * weights_sampling, na.rm = T),
    #Marche
    freq_marche_dif.garcons_moins5 = sum(marche_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_marche_dif.filles_moins5 = sum(marche_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_marche_dif.garcons_5_18 = sum(marche_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_marche_dif.filles_5_18 = sum(marche_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_marche_dif.hommes_18_64 = sum(marche_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_marche_dif.femmes_18_64 = sum(marche_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_marche_dif.hommes_65plus = sum(marche_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_marche_dif.femmes_65plus = sum(marche_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    #Soins
    freq_soins_dif.garcons_moins5 = sum(soins_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_soins_dif.filles_moins5 = sum(soins_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_soins_dif.garcons_5_18 = sum(soins_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_soins_dif.filles_5_18 = sum(soins_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_soins_dif.hommes_18_64 = sum(soins_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_soins_dif.femmes_18_64 = sum(soins_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_soins_dif.hommes_65plus = sum(soins_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_soins_dif.femmes_65plus = sum(soins_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    #Concentration
    freq_concentration_dif.garcons_moins5 = sum(concentration_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_concentration_dif.filles_moins5 = sum(concentration_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_concentration_dif.garcons_5_18 = sum(concentration_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_concentration_dif.filles_5_18 = sum(concentration_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_concentration_dif.hommes_18_64 = sum(concentration_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_concentration_dif.femmes_18_64 = sum(concentration_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_concentration_dif.hommes_65plus = sum(concentration_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_concentration_dif.femmes_65plus = sum(concentration_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    #Vision
    freq_vision_dif.garcons_moins5  = sum(vision_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_vision_dif.filles_moins5 = sum(vision_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_vision_dif.garcons_5_18 = sum(vision_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_vision_dif.filles_5_18 = sum(vision_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T), 
    freq_vision_dif.hommes_18_64 = sum(vision_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_vision_dif.femmes_18_64 = sum(vision_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_vision_dif.hommes_65plus = sum(vision_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_vision_dif.femmes_65plus = sum(vision_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    #Entendre
    freq_entendre_dif.garcons_moins5 = sum(entendre_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_entendre_dif.filles_moins5 = sum(entendre_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_entendre_dif.garcons_5_18 = sum(entendre_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_entendre_dif.filles_5_18 = sum(entendre_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_entendre_dif.hommes_18_64 = sum(entendre_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_entendre_dif.femmes_18_64 = sum(entendre_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_entendre_dif.hommes_65plus = sum(entendre_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_entendre_dif.femmes_65plus = sum(entendre_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    # Communication
    freq_communication_dif.garcons_moins5 = sum(communication_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_communication_dif.filles_moins5 = sum(communication_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_communication_dif.garcons_5_18 = sum(communication_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_communication_dif.filles_5_18 = sum(communication_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_communication_dif.hommes_18_64 = sum(communication_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_communication_dif.femmes_18_64 = sum(communication_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_communication_dif.hommes_65plus = sum(communication_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_communication_dif.femmes_65plus = sum(communication_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    
    #Deces
    freq_deces_dif.garcons_moins5 = sum(deces_dif_garcons_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_hommes * weights_sampling, na.rm = T),
    freq_deces_dif.filles_moins5 = sum(deces_dif_filles_moins5 * weights_sampling, na.rm=T)/sum(agegrp_0_5_femmes * weights_sampling, na.rm = T),
    freq_deces_dif.garcons_5_18 = sum(deces_dif_garcons_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_hommes * weights_sampling, na.rm = T),
    freq_deces_dif.filles_5_18 = sum(deces_dif_filles_5_18 * weights_sampling, na.rm=T)/sum(agegrp_5_18_femmes * weights_sampling, na.rm = T),
    freq_deces_dif.hommes_18_64 = sum(deces_dif_hommes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_hommes * weights_sampling, na.rm = T),
    freq_deces_dif.femmes_18_64 = sum(deces_dif_femmes_18_64 * weights_sampling, na.rm=T)/sum(agegrp_18_65_femmes * weights_sampling, na.rm = T),
    freq_deces_dif.hommes_65plus = sum(deces_dif_hommes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_hommes * weights_sampling, na.rm = T),
    freq_deces_dif.femmes_65plus = sum(deces_dif_femmes_65plus * weights_sampling, na.rm=T)/sum(agegrp_65plus_femmes * weights_sampling, na.rm = T),
    
    freq_raison_deces.diarrhee = sum(raison_deces.diarrhee * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.autre_maladie = sum(raison_deces.autre_maladie * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.morsure = sum(raison_deces.morsure * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.deces_natu = sum(raison_deces.deces_natu * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.faim = sum(raison_deces.faim * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.accident_travail = sum(raison_deces.accident_travail * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.problemes_respi = sum(raison_deces.problemes_respi * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.autre = sum(raison_deces.autre * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.catastrophe_natu = sum(raison_deces.catastrophe_natu * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.accident_conflit = sum(raison_deces.accident_conflit * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.en_couche = sum(raison_deces.en_couche * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    freq_raison_deces.accident_route = sum(raison_deces.accident_route * weights_sampling, na.rm=T)/sum(total_deces * weights_sampling, na.rm = T),
    # Education
    freq_educ.3_5an_garcon = sum(total_educ_3_5an_garcon*weights_sampling, na.rm = T)/sum(total_3_5_hommes*weights_sampling, na.rm = T),
    freq_infor_educ.3_5an_garcon = sum(total_infor_educ_3_5an_garcon * weights_sampling, na.rm = T)/sum(total_3_5_hommes*weights_sampling, na.rm = T),
    freq_non_for_educ.3_5an_garcon = sum(total_non_for_educ_3_5an_garcon * weights_sampling, na.rm = T)/sum(total_3_5_hommes*weights_sampling, na.rm = T),
    freq_aucune_educ.3_5an_garcon = sum(total_aucune_educ_3_5an_garcon * weights_sampling, na.rm = T)/sum(total_3_5_hommes*weights_sampling, na.rm = T),
    freq_educ.3_5an_fille = sum(total_educ_3_5an_fille * weights_sampling, na.rm = T)/sum(total_3_5_femmes * weights_sampling, na.rm = T),
    freq_infor_educ.3_5an_fille = sum(total_infor_educ_3_5an_fille * weights_sampling, na.rm = T)/sum(total_3_5_femmes * weights_sampling, na.rm = T),
    freq_non_for_educ.3_5an_fille = sum(total_non_for_educ_3_5an_fille * weights_sampling, na.rm = T)/sum(total_3_5_femmes * weights_sampling, na.rm = T),
    freq_aucune_educ.3_5an_fille = sum(total_aucune_educ_3_5an_fille * weights_sampling, na.rm = T)/sum(total_3_5_femmes * weights_sampling, na.rm = T),
    
    freq_educ.3_5an_total = sum(educ.3_5an_total*weights_sampling, na.rm=T)/sum(total_3_5*weights_sampling, na.rm = T),
    freq_infor_educ.3_5an_total = sum(infor_educ.3_5an_total*weights_sampling, na.rm = T)/sum(total_3_5*weights_sampling, na.rm = T),
    freq_non_for_educ.3_5an_total = sum(non_for_educ.3_5an_total*weights_sampling, na.rm = T)/sum(total_3_5*weights_sampling, na.rm = T),
    freq_aucune_educ.3_5an_total = sum(aucune_educ.3_5an_total*weights_sampling, na.rm = T)/sum(total_3_5*weights_sampling, na.rm = T),
    
    freq_educ.6_12an_garcon  = sum(total_educ_6_12an_garcon * weights_sampling, na.rm = T)/sum(total_6_12_hommes * weights_sampling, na.rm = T),
    freq_infor_educ.6_12an_garcon   = sum(total_infor_educ_6_12an_garcon * weights_sampling, na.rm = T)/sum(total_6_12_hommes * weights_sampling, na.rm = T),
    freq_non_for_educ.6_12an_garcon  = sum(total_non_for_educ_6_12an_garcon * weights_sampling, na.rm = T)/sum(total_6_12_hommes * weights_sampling, na.rm = T),
    freq_aucune_educ.6_12an_garcon = sum(total_aucune_educ_6_12an_garcon * weights_sampling, na.rm = T)/sum(total_6_12_hommes * weights_sampling, na.rm = T),
    freq_educ.6_12an_fille = sum(total_educ_6_12an_fille * weights_sampling, na.rm = T)/sum(total_6_12_femmes * weights_sampling, na.rm = T),
    freq_infor_educ.6_12an_fille = sum(total_infor_educ_6_12an_fille * weights_sampling, na.rm = T)/sum(total_6_12_femmes * weights_sampling, na.rm = T),
    freq_non_for_educ.6_12an_fille = sum(total_non_for_educ_6_12an_fille * weights_sampling, na.rm = T)/sum(total_6_12_femmes * weights_sampling, na.rm = T),
    freq_aucune_educ.6_12an_fille = sum(total_aucune_educ_6_12an_fille * weights_sampling, na.rm = T)/sum(total_6_12_femmes * weights_sampling, na.rm = T),
    
    freq_educ.6_12an_total = sum(educ.6_12an_total*weights_sampling, na.rm=T)/sum(total_6_12*weights_sampling, na.rm = T),
    freq_infor_educ.6_12an_total = sum(infor_educ.6_12an_total*weights_sampling, na.rm = T)/sum(total_6_12*weights_sampling, na.rm = T),
    freq_non_for_educ.6_12an_total = sum(non_for_educ.6_12an_total*weights_sampling, na.rm = T)/sum(total_6_12*weights_sampling, na.rm = T),
    freq_aucune_educ.6_12an_total = sum(aucune_educ.6_12an_total*weights_sampling, na.rm = T)/sum(total_6_12*weights_sampling, na.rm = T),
    
    freq_educ.13_17an_garcon = sum(total_educ_13_17an_garcon * weights_sampling, na.rm = T)/sum(total_13_17_hommes * weights_sampling, na.rm = T),
    freq_infor_educ.13_17an_garcon = sum(total_infor_educ_13_17an_garcon * weights_sampling, na.rm = T)/sum(total_13_17_hommes * weights_sampling, na.rm = T),
    freq_non_for_educ.13_17an_garcon = sum(total_non_for_educ_13_17an_garcon * weights_sampling, na.rm = T)/sum(total_13_17_hommes * weights_sampling, na.rm = T),
    freq_aucune_educ.13_17an_garcon = sum(total_aucune_educ_13_17an_garcon * weights_sampling, na.rm = T)/sum(total_13_17_hommes * weights_sampling, na.rm = T),
    freq_educ.13_17an_fille = sum(total_educ_13_17an_fille * weights_sampling, na.rm = T)/sum(total_13_17_femmes * weights_sampling, na.rm = T),
    freq_infor_educ.13_17an_fille = sum(total_infor_educ_13_17an_fille * weights_sampling, na.rm = T)/sum(total_13_17_femmes * weights_sampling, na.rm = T),
    freq_non_for_educ.13_17an_fille = sum(total_non_for_educ_13_17an_fille * weights_sampling, na.rm = T)/sum(total_13_17_femmes * weights_sampling, na.rm = T),
    freq_aucune_educ.13_17an_fille = sum(total_aucune_educ_13_17an_fille * weights_sampling, na.rm = T)/sum(total_13_17_femmes * weights_sampling, na.rm = T),
    
    freq_educ.13_17an_total = sum(educ.13_17an_total*weights_sampling, na.rm=T)/sum(total_13_17*weights_sampling, na.rm = T),
    freq_infor_educ.13_17an_total = sum(infor_educ.13_17an_total*weights_sampling, na.rm = T)/sum(total_13_17*weights_sampling, na.rm = T),
    freq_non_for_educ.13_17an_total = sum(non_for_educ.13_17an_total*weights_sampling, na.rm = T)/sum(total_13_17*weights_sampling, na.rm = T),
    freq_aucune_educ.13_17an_total = sum(aucune_educ.13_17an_total*weights_sampling, na.rm = T)/sum(total_13_17*weights_sampling, na.rm = T),
    
    freq_scolarise = sum(total_scolarise * weights_sampling, na.rm = T)/sum(enfant_scolarisable * weights_sampling, na.rm = T),
    freq_non_scolarise = sum(total_non_scolarise * weights_sampling, na.rm = T)/sum(enfant_scolarisable * weights_sampling, na.rm = T),
    
    freq_enfants_moins5_diarrhee = sum(enfant_selle*weights_sampling, na.rm = T)/sum(total_moins_5ans*weights_sampling, na.rm = T),
    
    freq_travail_fille = sum(travail_fille*weights_sampling, na.rm = T)/sum(total_0_17_femmes*weights_sampling, na.rm = T),
    freq_travail_garcon = sum(travail_garcon*weights_sampling, na.rm = T)/sum(total_0_17_hommes*weights_sampling, na.rm = T),
    .groups = "drop"
  )%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  select(admin2, status,variable, variable_value, numbers)

bfa_admin2 <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
  select(admin1Name, admin2Name)%>%
  mutate(admin2name = tolower(admin2Name))%>%
  distinct()

n_adm2 <- cleaned_data_adm1%>%
  group_by(admin2, status)%>%
  summarise(n = n(), .groups="drop")%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "n")%>%
  select(-variable)

which_subsets_adm2 <- cleaned_data_adm1%>%
  group_by(admin2, status)%>%
  summarise_all(funs(sum(is.na(.))))%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "NAs")%>%
  left_join(n_adm2, by = c("admin2", "status"))%>%
  mutate(perc_NAs = round(NAs/n*100, 0))%>%
  select(-n)

which_skipLogic_admin2 <- map(names(cleaned_data_adm2%>%select(-eau_propre, -admin1, -admin2, -status)), function(x)questionnaire$question_is_skipped(question.name = x, data = cleaned_data_adm2%>%select(-eau_propre)))%>%
  as.data.frame()%>%
  mutate(admin1 = cleaned_data_adm2$admin1, admin2 = cleaned_data_adm2$admin2, status = cleaned_data_adm2$status)

names(which_skipLogic_admin2) <- c(names(cleaned_data_adm2%>%select(-eau_propre,  -admin1, -admin2, -status)), "admin1", "admin2", "status")


which_skipLogic_adm2 <- which_skipLogic_admin2%>%
  select(-admin1)%>%
  group_by(admin2, status)%>%
  summarise_all(funs(sum(.)))%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "Skipped")%>%
  left_join(n_adm2, by = c("admin2", "status"))%>%
  mutate(perc_Skipped = round(Skipped/n*100, 0))%>%
  select(-n)

which_skipLogic_adm2_var <- which_skipLogic_adm2 %>%
  group_by(variable) %>%
  summarise(Skipped = sum(Skipped, na.rm = T),
            perc_Skipped = sum(perc_Skipped, na.rm = T)) %>%
  mutate(subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))

summary_stats_admin_2_grp_final <- bind_rows(summary_stats_admin_2_grp, freq_admin2_grp)%>%
  left_join(which_subsets_adm2, by = c("admin2", "status", "variable"))%>%
  # left_join(which_skipLogic_adm2, by = c("admin2", "status", "variable"))%>%
  mutate(question_choice = case_when(is.na(variable_value) ~ variable,
                                     TRUE ~ paste0(variable, ".", variable_value))) %>%
  # ,
  # subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))%>%
  left_join(dico, by = "question_choice")%>%
  mutate(status = case_when(status == "pdi" ~ "PDI",
                            status == "host" ~ "Communauté hôte",
                            TRUE ~ NA_character_))%>%
  mutate(numbers = case_when(grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical"  ~ round(numbers*100,1),
                             !grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical" ~ round(numbers*100,0),
                             dependent.variable.type == "numerical"  ~ round(numbers,1)
  )
  )%>%
  select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2, status, label_indicator, label_choice, numbers, subset)%>%
  left_join(bfa_admin2, by = c("admin2" = "admin2name"))%>%
  distinct()%>%
  mutate(label_choice = case_when(is.na(label_choice) ~ label_indicator, 
                                  TRUE ~ paste(label_indicator, label_choice, sep = ": ")))%>%
  select(research.question_label, sub.research.question_label, admin2Name, status, label_choice, numbers, variable)%>%
  # select(research.question_label, sub.research.question_label, admin2Name, status, label_choice,subset, numbers)%>%
  distinct()%>%
  filter(!is.na(label_choice))%>%
  group_by(research.question_label, sub.research.question_label, admin2Name, status, label_choice)%>%
  pivot_wider(names_from = c(admin2Name), values_from = numbers)%>%
  # summarise(across(everything(), sum, na.rm=T))%>%
  ungroup()%>%
  mutate(sub.research.question_label = factor(sub.research.question_label, levels = target_sub_research_question_order ),
         research.question_label = factor(research.question_label, levels = target_research_question_order))%>%
  arrange(research.question_label,sub.research.question_label,status,label_choice)%>%
  filter(!is.na(label_choice)) %>%
  left_join(select(which_skipLogic_adm2_var, variable,subset) , by = "variable")%>%
  select(research.question_label, sub.research.question_label, status, label_choice,subset,everything())%>%
  select(-variable)


names(summary_stats_admin_2_grp_final)[1:5] <- c("Question de recherche", "Sous-question de recherche", "Groupe de population", "Indicator", "Sous-ensemble de donnée")
write_csv(summary_stats_admin_2_grp_final, "outputs/tables/summary_stats_admin_2_grp.csv")
