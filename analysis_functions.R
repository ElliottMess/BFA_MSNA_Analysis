#' A function to calculate additional frequencies over the whole dataset. 
#'
#' @param data data.frame A dataframe with the data to aggregate 
#' @param aggregate_level string. Column in data to use as the aggregation level.
#' @param pop_grp string. Population groups to be analysed.
#' @param weights scalar. A vector/scalar containing the weights to be applied
#'
#' @return a dataframe containing the analysed data.
#' @export
#'
#' @examples
freq_sum <- function(data, aggregate_level, pop_grp = NULL, weights = NULL){
  
  if(!is.null(aggregate_level)){aggregate_level <- sym(aggregate_level)}
  if(!is.null(pop_grp)){pop_grp <- sym(pop_grp)}
  if(!is.null(weights)){weights <- sym(weights)}
  
  freq <- data%>%
    group_by(!!aggregate_level, !!pop_grp)%>%
    summarise(
      freq_detres_adult = sum(detres_adult* !!weights, na.rm = T)/sum(sum(femme,homme, na.rm = T)*!!weights, na.rm = T),
      freq_detres_enfants = sum(detres_enft*!!weights, na.rm = T)/sum(enfant*!!weights, na.rm = T),
      #Naissances
      freq_lieu_accouchement.centre_sante = sum(lieu_accouchement.centre_sante * !!weights, na.rm = T)/sum(total_naissance * !!weights, na.rm = T),
      freq_lieu_accouchement.maison = sum(lieu_accouchement.maison * !!weights, na.rm = T)/sum(total_naissance * !!weights, na.rm = T),
      freq_lieu_accouchement.autre = sum(lieu_accouchement.autre * !!weights, na.rm = T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.accouche_assiste_domicil = sum(raison_dominicile.accouche_assiste_domicil * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.accouche_centre_ferme = sum(raison_dominicile.centre_ferme * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.accouche_centre_financ_inacc = sum(raison_dominicile.centre_financ_inacc * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.centre_surpeuple = sum(raison_dominicile.centre_surpeuple * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.maternite_pas_sur = sum(raison_dominicile.maternite_pas_sur * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.physique_mere = sum(raison_dominicile.physique_mere * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.rejoindre_centre = sum(raison_dominicile.rejoindre_centre * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      freq_raison_domicil.nsp = sum(raison_dominicile.nsp * !!weights, na.rm=T)/sum(total_naissance * !!weights, na.rm = T),
      #Enfants malades
      freq_enfants_5ans_maladie.palu = sum(`maladie_moins_5ans/palu` * !!weights, na.rm=T)/sum(total_moins_5ans * !!weights, na.rm = T),
      freq_enfants_5ans_maladie.infect_respiratoire  = sum(`maladie_moins_5ans/infect_respiratoire` * !!weights, na.rm=T)/sum(total_moins_5ans * !!weights, na.rm = T),
      freq_enfants_5ans_maladie.diarrhee = sum(`maladie_moins_5ans/diarrhee` * !!weights, na.rm=T)/sum(total_moins_5ans * !!weights, na.rm = T),
      freq_enfants_5ans_maladie.autre = sum(`maladie_moins_5ans/autre` * !!weights, na.rm=T)/sum(total_moins_5ans * !!weights, na.rm = T),
      freq_enfants_5ans_maladie.nsp = sum(`maladie_moins_5ans/nsp` * !!weights, na.rm=T)/sum(total_moins_5ans * !!weights, na.rm = T),
      #Marche
      freq_marche_dif.garcons_moins5 = sum(marche_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_marche_dif.filles_moins5 = sum(marche_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_marche_dif.garcons_5_18 = sum(marche_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_marche_dif.filles_5_18 = sum(marche_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_marche_dif.hommes_18_64 = sum(marche_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_marche_dif.femmes_18_64 = sum(marche_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_marche_dif.hommes_65plus = sum(marche_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_marche_dif.femmes_65plus = sum(marche_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      #Soins
      freq_soins_dif.garcons_moins5 = sum(soins_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_soins_dif.filles_moins5 = sum(soins_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_soins_dif.garcons_5_18 = sum(soins_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_soins_dif.filles_5_18 = sum(soins_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_soins_dif.hommes_18_64 = sum(soins_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_soins_dif.femmes_18_64 = sum(soins_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_soins_dif.hommes_65plus = sum(soins_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_soins_dif.femmes_65plus = sum(soins_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      #Concentration
      freq_concentration_dif.garcons_moins5 = sum(concentration_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_concentration_dif.filles_moins5 = sum(concentration_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_concentration_dif.garcons_5_18 = sum(concentration_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_concentration_dif.filles_5_18 = sum(concentration_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_concentration_dif.hommes_18_64 = sum(concentration_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_concentration_dif.femmes_18_64 = sum(concentration_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_concentration_dif.hommes_65plus = sum(concentration_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_concentration_dif.femmes_65plus = sum(concentration_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      #Vision
      freq_vision_dif.garcons_moins5  = sum(vision_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_vision_dif.filles_moins5 = sum(vision_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_vision_dif.garcons_5_18 = sum(vision_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_vision_dif.filles_5_18 = sum(vision_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T), 
      freq_vision_dif.hommes_18_64 = sum(vision_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_vision_dif.femmes_18_64 = sum(vision_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_vision_dif.hommes_65plus = sum(vision_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_vision_dif.femmes_65plus = sum(vision_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      #Entendre
      freq_entendre_dif.garcons_moins5 = sum(entendre_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_entendre_dif.filles_moins5 = sum(entendre_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_entendre_dif.garcons_5_18 = sum(entendre_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_entendre_dif.filles_5_18 = sum(entendre_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_entendre_dif.hommes_18_64 = sum(entendre_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_entendre_dif.femmes_18_64 = sum(entendre_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_entendre_dif.hommes_65plus = sum(entendre_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_entendre_dif.femmes_65plus = sum(entendre_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      # Communication
      freq_communication_dif.garcons_moins5 = sum(communication_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_communication_dif.filles_moins5 = sum(communication_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_communication_dif.garcons_5_18 = sum(communication_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_communication_dif.filles_5_18 = sum(communication_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_communication_dif.hommes_18_64 = sum(communication_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_communication_dif.femmes_18_64 = sum(communication_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_communication_dif.hommes_65plus = sum(communication_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_communication_dif.femmes_65plus = sum(communication_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      
      #Deces
      freq_deces_dif.garcons_moins5 = sum(deces_dif_garcons_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_hommes * !!weights, na.rm = T),
      freq_deces_dif.filles_moins5 = sum(deces_dif_filles_moins5 * !!weights, na.rm=T)/sum(agegrp_0_5_femmes * !!weights, na.rm = T),
      freq_deces_dif.garcons_5_18 = sum(deces_dif_garcons_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_hommes * !!weights, na.rm = T),
      freq_deces_dif.filles_5_18 = sum(deces_dif_filles_5_18 * !!weights, na.rm=T)/sum(agegrp_5_18_femmes * !!weights, na.rm = T),
      freq_deces_dif.hommes_18_64 = sum(deces_dif_hommes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_hommes * !!weights, na.rm = T),
      freq_deces_dif.femmes_18_64 = sum(deces_dif_femmes_18_64 * !!weights, na.rm=T)/sum(agegrp_18_65_femmes * !!weights, na.rm = T),
      freq_deces_dif.hommes_65plus = sum(deces_dif_hommes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_hommes * !!weights, na.rm = T),
      freq_deces_dif.femmes_65plus = sum(deces_dif_femmes_65plus * !!weights, na.rm=T)/sum(agegrp_65plus_femmes * !!weights, na.rm = T),
      
      freq_raison_deces.diarrhee = sum(raison_deces.diarrhee * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.autre_maladie = sum(raison_deces.autre_maladie * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.morsure = sum(raison_deces.morsure * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.deces_natu = sum(raison_deces.deces_natu * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.faim = sum(raison_deces.faim * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.accident_travail = sum(raison_deces.accident_travail * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.problemes_respi = sum(raison_deces.problemes_respi * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.autre = sum(raison_deces.autre * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.catastrophe_natu = sum(raison_deces.catastrophe_natu * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.accident_conflit = sum(raison_deces.accident_conflit * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.en_couche = sum(raison_deces.en_couche * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      freq_raison_deces.accident_route = sum(raison_deces.accident_route * !!weights, na.rm=T)/sum(total_deces * !!weights, na.rm = T),
      # Education
      freq_educ.3_5an_garcon = sum(total_educ_3_5an_garcon*!!weights, na.rm = T)/sum(total_3_5_hommes*!!weights, na.rm = T),
      freq_infor_educ.3_5an_garcon = sum(total_infor_educ_3_5an_garcon * !!weights, na.rm = T)/sum(total_3_5_hommes*!!weights, na.rm = T),
      freq_non_for_educ.3_5an_garcon = sum(total_non_for_educ_3_5an_garcon * !!weights, na.rm = T)/sum(total_3_5_hommes*!!weights, na.rm = T),
      freq_aucune_educ.3_5an_garcon = sum(total_aucune_educ_3_5an_garcon * !!weights, na.rm = T)/sum(total_3_5_hommes*!!weights, na.rm = T),
      freq_educ.3_5an_fille = sum(total_educ_3_5an_fille * !!weights, na.rm = T)/sum(total_3_5_femmes * !!weights, na.rm = T),
      freq_infor_educ.3_5an_fille = sum(total_infor_educ_3_5an_fille * !!weights, na.rm = T)/sum(total_3_5_femmes * !!weights, na.rm = T),
      freq_non_for_educ.3_5an_fille = sum(total_non_for_educ_3_5an_fille * !!weights, na.rm = T)/sum(total_3_5_femmes * !!weights, na.rm = T),
      freq_aucune_educ.3_5an_fille = sum(total_aucune_educ_3_5an_fille * !!weights, na.rm = T)/sum(total_3_5_femmes * !!weights, na.rm = T),
      
      freq_educ.3_5an_total = sum(educ.3_5an_total*!!weights, na.rm=T)/sum(total_3_5*!!weights, na.rm = T),
      freq_infor_educ.3_5an_total = sum(infor_educ.3_5an_total*!!weights, na.rm = T)/sum(total_3_5*!!weights, na.rm = T),
      freq_non_for_educ.3_5an_total = sum(non_for_educ.3_5an_total*!!weights, na.rm = T)/sum(total_3_5*!!weights, na.rm = T),
      freq_aucune_educ.3_5an_total = sum(aucune_educ.3_5an_total*!!weights, na.rm = T)/sum(total_3_5*!!weights, na.rm = T),
      
      freq_educ.6_12an_garcon  = sum(total_educ_6_12an_garcon * !!weights, na.rm = T)/sum(total_6_12_hommes * !!weights, na.rm = T),
      freq_infor_educ.6_12an_garcon   = sum(total_infor_educ_6_12an_garcon * !!weights, na.rm = T)/sum(total_6_12_hommes * !!weights, na.rm = T),
      freq_non_for_educ.6_12an_garcon  = sum(total_non_for_educ_6_12an_garcon * !!weights, na.rm = T)/sum(total_6_12_hommes * !!weights, na.rm = T),
      freq_aucune_educ.6_12an_garcon = sum(total_aucune_educ_6_12an_garcon * !!weights, na.rm = T)/sum(total_6_12_hommes * !!weights, na.rm = T),
      freq_educ.6_12an_fille = sum(total_educ_6_12an_fille * !!weights, na.rm = T)/sum(total_6_12_femmes * !!weights, na.rm = T),
      freq_infor_educ.6_12an_fille = sum(total_infor_educ_6_12an_fille * !!weights, na.rm = T)/sum(total_6_12_femmes * !!weights, na.rm = T),
      freq_non_for_educ.6_12an_fille = sum(total_non_for_educ_6_12an_fille * !!weights, na.rm = T)/sum(total_6_12_femmes * !!weights, na.rm = T),
      freq_aucune_educ.6_12an_fille = sum(total_aucune_educ_6_12an_fille * !!weights, na.rm = T)/sum(total_6_12_femmes * !!weights, na.rm = T),
      
      freq_educ.6_12an_total = sum(educ.6_12an_total*!!weights, na.rm=T)/sum(total_6_12*!!weights, na.rm = T),
      freq_infor_educ.6_12an_total = sum(infor_educ.6_12an_total*!!weights, na.rm = T)/sum(total_6_12*!!weights, na.rm = T),
      freq_non_for_educ.6_12an_total = sum(non_for_educ.6_12an_total*!!weights, na.rm = T)/sum(total_6_12*!!weights, na.rm = T),
      freq_aucune_educ.6_12an_total = sum(aucune_educ.6_12an_total*!!weights, na.rm = T)/sum(total_6_12*!!weights, na.rm = T),
      
      freq_educ.13_17an_garcon = sum(total_educ_13_17an_garcon * !!weights, na.rm = T)/sum(total_13_17_hommes * !!weights, na.rm = T),
      freq_infor_educ.13_17an_garcon = sum(total_infor_educ_13_17an_garcon * !!weights, na.rm = T)/sum(total_13_17_hommes * !!weights, na.rm = T),
      freq_non_for_educ.13_17an_garcon = sum(total_non_for_educ_13_17an_garcon * !!weights, na.rm = T)/sum(total_13_17_hommes * !!weights, na.rm = T),
      freq_aucune_educ.13_17an_garcon = sum(total_aucune_educ_13_17an_garcon * !!weights, na.rm = T)/sum(total_13_17_hommes * !!weights, na.rm = T),
      freq_educ.13_17an_fille = sum(total_educ_13_17an_fille * !!weights, na.rm = T)/sum(total_13_17_femmes * !!weights, na.rm = T),
      freq_infor_educ.13_17an_fille = sum(total_infor_educ_13_17an_fille * !!weights, na.rm = T)/sum(total_13_17_femmes * !!weights, na.rm = T),
      freq_non_for_educ.13_17an_fille = sum(total_non_for_educ_13_17an_fille * !!weights, na.rm = T)/sum(total_13_17_femmes * !!weights, na.rm = T),
      freq_aucune_educ.13_17an_fille = sum(total_aucune_educ_13_17an_fille * !!weights, na.rm = T)/sum(total_13_17_femmes * !!weights, na.rm = T),
      
      freq_educ.13_17an_total = sum(educ.13_17an_total*!!weights, na.rm=T)/sum(total_13_17*!!weights, na.rm = T),
      freq_infor_educ.13_17an_total = sum(infor_educ.13_17an_total*!!weights, na.rm = T)/sum(total_13_17*!!weights, na.rm = T),
      freq_non_for_educ.13_17an_total = sum(non_for_educ.13_17an_total*!!weights, na.rm = T)/sum(total_13_17*!!weights, na.rm = T),
      freq_aucune_educ.13_17an_total = sum(aucune_educ.13_17an_total*!!weights, na.rm = T)/sum(total_13_17*!!weights, na.rm = T),
      
      freq_scolarise = sum(total_scolarise * !!weights, na.rm = T)/sum(enfant_scolarisable * !!weights, na.rm = T),
      freq_non_scolarise = sum(total_non_scolarise * !!weights, na.rm = T)/sum(enfant_scolarisable * !!weights, na.rm = T),
      
      freq_enfants_moins5_diarrhee = sum(enfant_selle*!!weights, na.rm = T)/sum(total_moins_5ans*!!weights, na.rm = T),
      
      freq_travail_fille = sum(travail_fille*!!weights, na.rm = T)/sum(total_0_17_femmes*!!weights, na.rm = T),
      freq_travail_garcon = sum(travail_garcon*!!weights, na.rm = T)/sum(total_0_17_hommes*!!weights, na.rm = T),
      .groups = "drop"
    )%>%
    pivot_longer(c(-!!aggregate_level, - !!pop_grp), names_to = "variable", values_to = "numbers")%>%
    separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
    distinct()%>%
    select(!!aggregate_level,!!pop_grp,variable, variable_value, numbers)
  
  return(freq)
}





#' A function to format the results from hypegrammaR to a more digestable tables
#'
#' @param data dataset the original dataset
#' @param result the results from from_analysisplan_map_to_output
#' @param csv logical (TRUE/FALSE) to indicate if the table should be written to a CSV file in outputs/tables
#' @inheritParams freq_sum
#' @return a dataset with the results. 
#' @export
#'
#' @examples
format_results <- function(data, result, aggregate_level=NULL, pop_grp= NULL, weights = NULL, csv = FALSE, questionnaire = questionnaire){
  
  if(!is.null(aggregate_level)){aggregate_level <- sym(aggregate_level)}
  if(!is.null(pop_grp)){pop_grp <- sym(pop_grp)}
  if(!is.null(weights)){weights <- sym(weights)}
  
  if(!is.null(pop_grp)){
    summary_stats <- result$results %>%
      lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
      select(repeat.var.value, dependent.var, dependent.var.value, independent.var.value, numbers)%>%
      dplyr::rename(!!aggregate_level := repeat.var.value, variable = dependent.var, variable_value = dependent.var.value, !!pop_grp := independent.var.value)
  }else{
    summary_stats <- result$results %>%
      lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
      select(repeat.var.value, dependent.var, dependent.var.value, numbers)%>%
      dplyr::rename(!!aggregate_level := repeat.var.value, variable = dependent.var, variable_value = dependent.var.value)
    
  }
  
  n_admin <- data%>%
    group_by(!!aggregate_level)%>%
    summarise(n = n(), .groups="drop")%>%
    pivot_longer(c(-!!aggregate_level), names_to = "variable", values_to = "n")%>%
    select(-variable)
  
  which_subsets_admin <- data%>%
    summarise_all(funs(sum(is.na(.))))%>%
    pivot_longer(everything(), names_to = "variable", values_to = "NAs")%>%
    # left_join(n_adm0, by = c("admin0"))%>%
    mutate(perc_NAs = round(NAs/nrow(data)*100, 0))
  
  which_skipLogic <- map(names(data%>%select(-eau_propre, -!!aggregate_level)), function(x){
    questionnaire$question_is_skipped(question.name = x, data = data%>%select(-eau_propre, -!!aggregate_level))
    })%>%
    as.data.frame()%>%
    mutate(!!aggregate_level := data[,aggregate_level])
  
  names(which_skipLogic) <- c(names(data%>%select(-eau_propre, -!!aggregate_level)), aggregate_level)
  
  which_skipLogic_admin <- which_skipLogic%>%
    group_by(!!aggregate_level)%>%
    summarise(across(everything(), sum))  %>%
    pivot_longer(c(-!!aggregate_level), names_to = "variable", values_to = "Skipped")%>%
    # left_join(n_adm1, by = c("admin0"))%>%
    mutate(perc_Skipped = round(Skipped/nrow(data)*100, 0))
  
  freq_admin <- freq_sum(data, aggregate_level = aggregate_level, pop_grp = pop_grp, weights = weights)
  
  bfa_admin <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
    select(admin1Name, admin2Name)%>%
    mutate(admin2 = tolower(admin2Name),
           admin1 = tolower(admin1Name),
           admin1 = gsub("-","_", admin1),
           admin1 = gsub(" ", "_", admin1),
           admin0 = "Burkina Faso")%>%
    select(!!aggregate_level)%>%
    distinct()
  
  choices <- read_csv("data/choices.csv", locale = locale(encoding = "UTF-8"))
  survey <- read_csv("data/survey.csv", locale = locale(encoding = "UTF-8"))
  
  questionnaire <- load_questionnaire(data, choices = choices, questions = survey)
  analysisplanTemplate <- read_csv("./data/analysis_plan_allVars_template.csv", locale = locale(encoding = "latin1"))
  
  dico <- form_dictionnary(data, survey, choices, analysisplanTemplate)
  
  
  target_research_question_order <- unique(dico$research.question_label)
  target_sub_research_question_order <- unique(dico$sub.research.question_label)
  
  which_skipLogic_admin_var <- which_skipLogic_admin %>%
    group_by(variable) %>%
    summarise(Skipped = sum(Skipped, na.rm = T),
              perc_Skipped = sum(perc_Skipped, na.rm = T), .groups = "drop") %>%
    mutate(subset = if_else(perc_Skipped > 0, "Sous-ensemble de donnée", NA_character_))
  
  summary_stats_final <- bind_rows(summary_stats, freq_admin)%>%
    left_join(which_subsets_admin, by = c( "variable"))%>%
    mutate(question_choice = case_when(is.na(variable_value) ~ variable,
                                       TRUE ~ paste0(variable, ".", variable_value)))%>%
    left_join(dico, by = "question_choice")%>%
    mutate(numbers = case_when(grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical"  ~ round(numbers*100,1),
                               !grepl("%.*?ayant de la difficulté à.*", label_indicator) & dependent.variable.type == "categorical" ~ round(numbers*100,0),
                               dependent.variable.type == "numerical"  ~ round(numbers,1)
      )
    )%>%
    select(research.question_label, sub.research.question_label, !!aggregate_level, label_indicator, label_choice, numbers, variable, !!pop_grp)%>%
    left_join(bfa_admin, by = as.character(aggregate_level))%>%
    distinct()%>%
    mutate(label_choice = case_when(is.na(label_choice) ~ label_indicator, 
                                    TRUE ~ paste(label_indicator, label_choice, sep = ": ")))%>%
    select(research.question_label, sub.research.question_label, !!aggregate_level, label_choice, numbers, variable, !!pop_grp)%>%
    distinct()%>%
    filter(!is.na(label_choice))
  
  if(is.null(pop_grp)){
    summary_stats_final <- summary_stats_final%>%
      mutate(group_pop = "Total")
  }
  if(!is.null(pop_grp)){
    summary_stats_final <- summary_stats_final%>%
      mutate(group_pop = !!pop_grp)%>%
      select(-!!pop_grp)
  }
  
  
  summary_stats_final <- summary_stats_final%>%
    group_by(research.question_label, sub.research.question_label, !!aggregate_level, group_pop, label_choice)%>%
    pivot_wider(names_from = c(!!aggregate_level), values_from = numbers)%>%
    # summarise(across(everything(), sum, na.rm=T))%>%
    ungroup()%>%
    mutate(sub.research.question_label = factor(sub.research.question_label, levels = target_sub_research_question_order),
           research.question_label = factor(research.question_label, levels = target_research_question_order))%>%
    arrange(research.question_label,sub.research.question_label,label_choice)%>%
    filter(!is.na(label_choice)) %>%
    left_join(select(which_skipLogic_admin_var, variable,subset) , by = "variable")%>%
    select(research.question_label, sub.research.question_label, group_pop, label_choice,subset,everything())%>%
    select(-variable)%>%
    mutate(group_pop = case_when(group_pop == "host" ~ "Communauté hôte",
                                 group_pop == "pdi" ~ "PDI",
                                 TRUE ~ group_pop
    )
    )
  
  
  
  names(summary_stats_final)[1:5] <- c("Question de recherche", "Sous-question de recherche", "Groupe de population", "Indicator", "Sous-ensemble de donnée")
  
  if(csv == TRUE){
    if(is.null(aggregate_level) & is.null(pop_grp)){
      path_csv <- paste0("outputs/tables/summary_stats_Overall.csv")
    }else if(is.null(aggregate_level)){
      path_csv <- paste0("outputs/tables/summary_stats_Overall_grp.csv")
    }else if(is.null(pop_grp)){
      path_csv <- paste0("outputs/tables/summary_stats_",aggregate_level,".csv")
    }else{
      path_csv <- paste0("outputs/tables/summary_stats_",aggregate_level,"_grp.csv")
    }
    
    write_csv(summary_stats_final, path_csv)
  }
  
  return(summary_stats_final)
  
}
