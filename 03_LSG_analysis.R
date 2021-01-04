source("utils.R")

## Creating list of packages to load
package_list <- c("elliottmess/butteR", "hypegrammaR","ellieallien/cleaninginspectoR","hunzikp/rtree", "impact-initiatives/koboloops",
                  "tidyr","dplyr", "ggplot2", "readr", "stringr", "lubridate", "readxl", "rgdal", "sf", "purrr", "sdcMicro", "srvyr", "questionr",
                  "elliottmess/msni19", "forcats")

## Running the packaging loading function
loadInstall_package(package_list)


source("01_PrepingAnalysis.R", encoding = "UTF-8")

source("utils.R")
source("analysisplan_factory.R", encoding = "UTF-8")
source("analysis_functions.R", encoding = "UTF-8")

cleaned_data_adm1$admin0 <- "BFA"

lsg_analysis <- function(data){
  df <- data%>%
    mutate(
          # Genera Capacity gaps
          rev_besoins = case_when(
            integra_besoin == "oui" ~ 0L,
            integra_besoin == "non" ~ 1L,
            TRUE ~ NA_integer_
          ),
          cap_gap = case_when(
            (rev_besoins) >= 1L ~ 1L,
            (rev_besoins) == 0L ~ 0L,
            TRUE ~ NA_integer_
          ),
          cap_gap_critique = case_when(
            lcsi == "emergency" ~ "4+",
            lcsi == "crisis" ~ "4",
            lcsi == "stress" ~ "3",
            lcsi == "none" ~ "1",
            TRUE ~ NA_character_
          ),
          # Vulnerabilité
          depl_plus6m = case_when(
            duree_deplacement %in% c("6_1_an", "1_an_plus") ~ 1L,
            duree_deplacement %in% c("moins_1_ans", "1_2_mois","3_5_mois") ~ 0L,
            status == "host" ~ 0L,
            TRUE ~ NA_integer_
          ),
          femme_cheffe_menage = case_when(
            genre_chef_menage == "femme" ~ 1L,
            genre_chef_menage == "homme" ~ 0L,
            TRUE ~ NA_integer_
          ),
          enfant_chef_menage = case_when(
            age_chef_menage == "moins_18ans" ~ 1L,
            age_chef_menage %in% c("18_65ans", "plus_65ans") ~ 0L,
            TRUE ~ NA_integer_
          ),
          pers_agee_chef_menage = case_when(
            age_chef_menage == "plus_65ans"  ~ 1L,
            age_chef_menage %in% c("moins_18ans", "18_65ans") ~ 0L,
            TRUE ~ NA_integer_
          ))
  
  df <- df%>%
    mutate(
          vulnerabilite = case_when(
            rowSums(dplyr::select(., depl_plus6m, auMoinsUneWG, femme_cheffe_menage, enfant_chef_menage, pers_agee_chef_menage), na.rm = T) >=1 ~ 1L,
            rowSums(dplyr::select(., depl_plus6m, auMoinsUneWG, femme_cheffe_menage, enfant_chef_menage, pers_agee_chef_menage), na.rm = T) == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          vuln_profil_chef_menage = case_when(
            femme_cheffe_menage == 1 & enfant_chef_menage == 1 & pers_agee_chef_menage == 0 ~ "fille_cheffe",
            femme_cheffe_menage == 0 & enfant_chef_menage == 1 & pers_agee_chef_menage == 0 ~ "garcon_chef",
            femme_cheffe_menage == 1 & pers_agee_chef_menage == 1 ~ "femme_agee_chef_menage",
            femme_cheffe_menage == 0 & pers_agee_chef_menage == 1 ~ "homme_age_chef_menage",
            femme_cheffe_menage == 1 & enfant_chef_menage == 0 & pers_agee_chef_menage == 0 ~ "femme_chef",
            femme_cheffe_menage == 0 & enfant_chef_menage == 0 & pers_agee_chef_menage == 0 ~ "homme_chef",
            TRUE ~ NA_character_
          ),
          #ABNA
          dommage_abri = case_when(
            etat_abri == "bonne_etat" ~ 0L,
            etat_abri %in% c("endommage","degat","detruit") ~ 1L,
            TRUE ~ NA_integer_
          ),
          probleme_isolation = probleme_abri.isolation,
          modalite_occup = case_when(
            situation_menage %in% c("location", "proprietaire") ~ 0L,
            situation_menage %in% c("famille_accueil", "dur", "banco", "bois_planche", "paille", "occupation","pret") ~ 1L,
            TRUE ~ NA_integer_
            ),
          dorme_ext = case_when(
            dorme_exterieur > 0 ~ 1L,
            dorme_exterieur == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          surface_pers = case_when(
            personne_m2 >= 3.5 ~ 0L,
            personne_m2 < 3.5 ~ 1L,
            TRUE ~ NA_integer_
            
          ))
  
      df <- df%>%
        mutate(
          abna_NC_prop = rowSums(dplyr::select(., dommage_abri, probleme_isolation, surface_pers, modalite_occup), na.rm = T)/4,
          abna_NC_score = case_when(
            abna_NC_prop >=0 & abna_NC_prop <=1/3 ~ 1L,
            abna_NC_prop >1/3 & abna_NC_prop <=2/3 ~ 2L,
            abna_NC_prop >2/3 & abna_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          abna_DT = case_when(
            type_abri == "pas_abri" | etat_abri == "detruit" ~ 5L,
            etat_abri == "degat" | type_abri %in% c("fortune","inacheve") ~ 4L,
            etat_abri == "endommage" | type_abri %in% c("non_destine_au_logement", "communautaire", "urgence") ~ 3L,
            etat_abri == "bonne_etat" & type_abri == "construit" ~ 1L,
            TRUE ~ NA_integer_
          ),
          abna_final_score = pmax(abna_DT, abna_NC_score, na.rm = T),
          abna_lsg = case_when(
            abna_final_score >= 3 ~ 1L,
            abna_final_score >=1 & abna_final_score <= 2 ~ 0L,
            TRUE ~ NA_integer_
          ),
          abna_cg = case_when(
            dorme_ext + cap_gap >= 1 ~ 1L,
            dorme_ext + cap_gap == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          abna_lsg_vln = case_when(
            abna_lsg == 1 & vulnerabilite == 1 ~ 1L,
            abna_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          #WASH
          latrine_shared = case_when(
             infra_sanitaire  %in% c("lat_prive", "toilette") & is.na(type_latrine) ~ "ind",
             type_latrine == "lat_ind" ~ "ind",
             TRUE ~ type_latrine
          ),
          latrine_moins20 = case_when(
            infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared %in% c("ind", "lat_com_20") ~ 0L,
            infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com_50" ~ 1L,
            infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") & latrine_shared == "lat_com" ~ 1L,
            infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ 1L,
            TRUE ~ NA_integer_
          ),
          suffisamment_eau = case_when(
            eau_suffi %in% c("plus_suffisant", "suffisant", "juste_suffisant") ~ 0L,
            eau_suffi %in% c("insuffisant", "pas_suffisant") ~ 1L,
            TRUE ~ NA_integer_
          ),
          temps_eau = case_when(
            temps_total_eau %in% c("eau_concession", "moins_5mn", "entre_5_15mn", "entre_16_30mn") ~ 0L,
            temps_total_eau %in% c("entre_31_45mn", "plus_46mn") ~ 1L
          ),
          savon = case_when(
            acces_savon == "oui" ~ 0L,
            acces_savon == "non" ~ 1L,
            TRUE ~ NA_integer_
          ),
          dispo_lavageMain = case_when(
            dispo_lave_main %in% c("tippy", "evier", "seau_rob", "bouilloire") ~ 0L,
            dispo_lave_main %in% c("pas_dispositif") ~ 1L,
            TRUE ~ NA_integer_
          ),
          latrine_pashygenique =
            case_when(lat_hygiene == "oui" ~ 0L,
                      lat_hygiene == "non" | infra_sanitaire %in% c("dal_eau", "dal_precis", "dal_zonep_am", "dal_zonep") ~ 1L,
                      TRUE ~ NA_integer_
                      ),
          strat_cop_eha = case_when(
            redu_quant_eau == "oui" | eau_sure == "oui" ~ 1L,
            redu_quant_eau == "non" & eau_sure == "non" ~ 0L,
            redu_quant_eau == "nsp" | eau_sure == "nsp" ~ NA_integer_,
            TRUE ~ NA_integer_
          ),
          diarr = case_when(
            enfant_selle >0 ~ 1L,
            enfant_selle == 0 ~ 0L,
            TRUE ~ NA_integer_
          ))
      
      df <- df%>%
        mutate(
          eha_NC_prop = rowSums(select(., latrine_moins20, suffisamment_eau, temps_eau, savon, dispo_lavageMain, auMoins_aprLat_avantMan, latrine_pashygenique, diarr), na.rm = T)/8,
          eha_NC_score = case_when(
            eha_NC_prop >=0 & eha_NC_prop <=1/3 ~ 1L,
            eha_NC_prop >1/3 & eha_NC_prop <=2/3 ~ 2L,
            eha_NC_prop >2/3 & eha_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          eha_DT = case_when(
            typologie_source_eau == "surface" ~ 5L,
            typologie_source_eau == "non_amelioree" | infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_zonep_am", "dal_eau")  ~ 4L,
            (typologie_source_eau == "amelioree" & temps_total_eau %in% c("entre_31_45mn", "plus_46mn")) | latrine_shared %in% c("lat_com_50", "lat_com") ~ 3L,
            typologie_source_eau == "amelioree" & temps_total_eau %in% c("eau_concession", "moins_5mn", "entre_5_15mn", "entre_16_30mn") & latrine_shared %in% c("ind", "lat_com_20") ~ 1L,
            TRUE ~ NA_integer_
          ),
          # eha_DT2 = case_when(
          #   typologie_source_eau == "surface" ~ 5L,
          #   typologie_source_eau == "non_amelioree" | infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_zonep_am", "dal_eau")  ~ 4L,
          #   (typologie_source_eau == "amelioree" & temps_total_eau %in% c("entre_31_45mn", "plus_46mn")) | latrine_shared %in% c("lat_com_50", "lat_com") ~ 3L,
          #   (typologie_source_eau == "amelioree" & temps_total_eau %in% c("eau_concession", "moins_5mn", "entre_5_15mn", "entre_16_30mn")) | latrine_shared %in% c("ind", "lat_com_20") ~ 1L,
          #   TRUE ~ NA_integer_
          # ),
          eha_final_score = pmax(eha_DT, eha_NC_score, na.rm = TRUE),
          eha_lsg = case_when(
            eha_final_score >= 3 ~ 1L,
            eha_final_score >= 1 & eha_final_score <= 2~ 0L,
            TRUE ~ NA_integer_
          ),
          eha_cg = case_when(
            strat_cop_eha + cap_gap >= 1 ~ 1L,
            strat_cop_eha + cap_gap == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          eha_lsg_vln = case_when(
            eha_lsg == 1 & vulnerabilite == 1 ~ 1L,
            eha_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          
          # Education
          enfants_scol = case_when(
            total_non_scolarise == 0 ~ 0L,
            total_non_scolarise > 0 ~ 1L,
            TRUE ~ NA_integer_
          ),
          barrieres_edu = case_when(
            barriere_edu %in% c(
              "frais_scolaires", "hors_systeme", "pas_priorite", "type_enseignement", "mariage_enfant", "barriere_langue", "barriere_nomadisme", "travail_maison",
              "mendicite", "travail_champ", "travail_hors_maison", "handicap", "non_fonctionnelle", "ecole_loin", "classes_pleines", "enseignement_faible", "pas_latrine",
              "pas_enseignants", "insecurite_ecole", "insecurite_trajet", "groupes_armes_ecole", "gane_enfant"
              ) ~ 1L,
            barriere_edu == "barriere_non" ~ 0L,
            TRUE ~ NA_integer_
          )
      )
      
      df <- df%>%
          mutate(
          educ_NC_prop = rowSums(select(., barrieres_edu, enfants_scol), na.rm = T)/2,
          educ_NC_score = case_when(
            educ_NC_prop >=0 & educ_NC_prop <=1/3 ~ 1L,
            educ_NC_prop >1/3 & educ_NC_prop <=2/3 ~ 2L,
            educ_NC_prop >2/3 & educ_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          
          educ_DT = case_when(
            enfants_scol == 1 & barriere_edu %in% c("insecurite_ecole", "insecurite_trajet","groupes_armes_ecole", "mariage_enfant", "mendicite") ~ 4L,
            enfants_scol == 1 & barriere_edu %in% c("travail_champ", "travail_hors_maison") ~ 3L,
            enfants_scol == 0 ~ 1L,
            TRUE ~ NA_integer_
          ),
          educ_final_score = pmax(educ_DT, educ_NC_score, na.rm = TRUE),
          educ_lsg = case_when(
            educ_final_score >= 3 ~ 1L,
            educ_final_score >= 1 & educ_final_score <= 2 ~ 0L,
            TRUE ~ NA_integer_
          ),
          educ_cg = cap_gap,
          educ_lsg_vln = case_when(
            educ_lsg == 1 & vulnerabilite == 1 ~ 1L,
            educ_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          
          
          #Santé nut
          lieu_accouch = case_when(
            lieu_accouchement.centre_sante >= 1 ~ 0L,
            lieu_accouchement.centre_sante == 0 ~ 1L,
            TRUE ~ NA_integer_
          ),
          temps_service_sante = case_when(
            temps_centre_sante %in% c("15_30mn", "moins_15mn", "30mn_1h") ~ 0L,
            temps_centre_sante %in% c("1h_3h", "3h_et_plus") ~ 1L,
            TRUE ~ NA_integer_
          )
          )
      
      df <- df%>%
        mutate(
          sante_nut_NC_prop = rowSums(select(., lieu_accouch, temps_service_sante), na.rm = T)/3,
          sante_nut_NC_score = case_when(
            sante_nut_NC_prop >=0 & sante_nut_NC_prop <=1/3 ~ 1L,
            sante_nut_NC_prop >1/3 & sante_nut_NC_prop <=2/3 ~ 2L,
            sante_nut_NC_prop >2/3 & sante_nut_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          raison_deces_vieillesse = case_when(
            raison_deces.deces_natu == 1 ~ "naturelle",
            rowSums(select(., raison_deces.accident_conflit , raison_deces.accident_route , raison_deces.accident_travail ,
               raison_deces.catastrophe_natu , raison_deces.faim))  >0 ~ "violente",
            rowSums(select(., raison_deces.autre_maladie , raison_deces.diarrhee , raison_deces.en_couche , raison_deces.morsure,
            raison_deces.problemes_respi)) > 0 ~ "autre_cause",
            TRUE ~ NA_character_
            ),
          enfant_malade_struct_sante = case_when(
            malade_5ans > 0 & temps_centre_sante %in% c("15_30mn", "moins_15mn", "entre_31mn_1h") ~ "malade_m60",
            malade_5ans > 0 & temps_centre_sante %in% c("plus_1h","plus_3h") ~ "malade_p60",
            malade_5ans == 0 ~ "pas_malade",
            TRUE ~ NA_character_
          ),
          sante_nut_DT = case_when(
            raison_deces_vieillesse == "violente" | enfant_malade_struct_sante == "malade_p60" ~ 4L,
            raison_deces_vieillesse == "autre_cause" | enfant_malade_struct_sante == "malade_m60" ~ 3L,
            enfant_malade_struct_sante == "pas_malade" ~ 1L,
            TRUE ~ NA_integer_
          ),
          sante_nut_final_score = pmax(sante_nut_DT, sante_nut_NC_score, na.rm = T),
          sante_nut_lsg = case_when(
            sante_nut_final_score >= 3 ~ 1L,
            sante_nut_final_score >= 1 & sante_nut_final_score <= 2 ~ 0L,
            TRUE ~ NA_integer_
          ),
          sante_nut_cg = cap_gap,
          sante_nut_lsg_vln = case_when(
            sante_nut_lsg == 1 & vulnerabilite == 1 ~ 1L,
            sante_nut_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          
          # Protection
          membres_doc = case_when(
            document_identite == "all" ~ 0L,
            document_identite %in% c("partie", "aucun") ~ 1L,
            TRUE ~ NA_integer_
            ),
          expo_risqueSec = case_when(
            incident_secur == "non" ~ 0L,
            incident_secur == "oui" ~ 1L,
            TRUE ~ NA_integer_
          ),
          enfant_detress_psy = case_when(
            detress == "non" ~ 0L,
            detress == "oui" ~ 1L,
            TRUE ~ NA_integer_
          ),
          travail_enfant = case_when(
            travail_eft == 0 ~ 0L,
            travail_eft >= 1 ~ 1L,
            TRUE ~ NA_integer_
          ),
          titres = case_when(
            titre_propriete == "aucun"  ~ 1L,
            titre_propriete %in% c("all", "logement", "terre") ~ 0L,
            TRUE ~ NA_integer_
          )
        )
      
      df <- df%>%
        mutate(
          protection_NC_prop = rowSums(select(., membres_doc, expo_risqueSec, enfant_detress_psy, travail_enfant, vbg, titres), na.rm = T)/6,
          protection_NC_score = case_when(
            protection_NC_prop >=0 & protection_NC_prop <=1/3 ~ 1L,
            protection_NC_prop >1/3 & protection_NC_prop <=2/3 ~ 2L,
            protection_NC_prop >2/3 & protection_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          exp_mines_explosions = case_when(rowSums(select(., risque_fem.incident, risque_hom.incident, risque_garcon.incident,
                                               risque_fille.incident), na.rm = TRUE) > 0 ~ 1L,
                                           rowSums(select(., risque_fem.incident, risque_hom.incident, risque_garcon.incident,
                                              risque_fille.incident), na.rm = TRUE) == 0 ~ 0L, 
                                          TRUE ~ NA_integer_),
          travail_enfant_mines_rectures = case_when(
                                            rowSums(select(., type_trav_enft.carriere, type_trav_enft.recrue, type_trav_enft.prostitutiom), na.rm = TRUE) >0 ~ 1L,
                                            rowSums(select(., type_trav_enft.carriere, type_trav_enft.recrue, type_trav_enft.prostitutiom), na.rm = TRUE) <= 0 ~ 0L, 
                                            TRUE ~ NA_integer_
          ),
          exp_hardcore_stuff = case_when(
            rowSums(select(., vbg, risque_fem.enlevements_gene,risque_fem.enlevements, risque_fem.meurtre_grp,risque_fem.meurtre,
                risque_hom.enlevements_gene,risque_hom.enlevements, risque_hom.meurtre_grp,risque_hom.meurtre,
                risque_fille.enlevements_gene,risque_fille.enlevements, risque_fille.meurtre_grp,risque_fille.meurtre,
                risque_garcon.enlevements_gene,risque_garcon.enlevements, risque_garcon.meurtre_grp,risque_garcon.meurtre), na.rm = TRUE) >0 ~ 1L,
            rowSums(select(., vbg, risque_fem.enlevements_gene,risque_fem.enlevements, risque_fem.meurtre_grp,risque_fem.meurtre,
                risque_hom.enlevements_gene,risque_hom.enlevements, risque_hom.meurtre_grp,risque_hom.meurtre,
                risque_fille.enlevements_gene,risque_fille.enlevements, risque_fille.meurtre_grp,risque_fille.meurtre,
                risque_garcon.enlevements_gene,risque_garcon.enlevements, risque_garcon.meurtre_grp,risque_garcon.meurtre), na.rm = TRUE) <= 0 ~ 0L, 
            TRUE ~ NA_integer_
          ),
          protection_DT = case_when(
            exp_mines_explosions == 1 | travail_enfant_mines_rectures == 1 ~ 5L,
            exp_hardcore_stuff == 1 ~ 4L,
            expo_risqueSec == 1 ~ 3L,
            expo_risqueSec == 0 ~ 1L,
            TRUE ~ NA_integer_
          ),
          protection_final_score = pmax(protection_DT, protection_NC_score, na.rm = T),
          protection_lsg = case_when(
            protection_final_score >= 3 ~ 1L,
            protection_final_score < 3 ~ 0L,
            TRUE ~ NA_integer_
          ),
          protection_cg = cap_gap,
          protection_lsg_vln = case_when(
            protection_lsg == 1 & vulnerabilite == 1 ~ 1L,
            protection_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          # Sec Al
          acces_marche = case_when(
            marche_fonctionel == "prix_abordable" ~ 0L,
            marche_fonctionel %in% c("marche_distant", "insecurite", "transport", "deplace", "confinement", "prix", "bien_alimentaire", "bien_alimentaires") ~ 1L,
            TRUE ~ NA_integer_
          ),
          principale_source_rev = case_when(
            source_revenu1 %in% c("agri", "peche", "elevage", "propr_terrien", "commerce", "petit_com", "petit_metier", "journa_agri", "journa", "transport",
                                  "fonctionnaire", "prod_natu"
            ) ~ 0L,
            source_revenu1 %in% c("argent" ) ~1L,
            TRUE ~ NA_integer_
          ),
          fsc_acceptable = case_when(
            fcs2_thresholds == "acceptable" ~ 0L,
            fcs2_thresholds %in% c("limite", "pauvre") ~ 1L,
            TRUE ~ NA_integer_
          ),
          rcsi_acceptable = case_when(
            rcsi_thresholds == "low" ~ 0L,
            rcsi_thresholds %in% c("high", "medium") ~ 1L,
            TRUE ~ NA_integer_
          ),
          lcsi_acceptable = case_when(
            lcsi == "none" ~ 0L,
            lcsi %in% c("stress", "crisis", "emergency") ~ 1L,
            TRUE ~ NA_integer_
          )
        )
          
          
          
        df <- df%>%
          mutate(
          secal_NC_prop = rowSums(select(., acces_marche, principale_source_rev, fsc_acceptable), na.rm = T)/3,
          secal_NC_score = case_when(
            secal_NC_prop >=0 & secal_NC_prop <=1/3 ~ 1L,
            secal_NC_prop >1/3 & secal_NC_prop <=2/3 ~ 2L,
            secal_NC_prop >2/3 & secal_NC_prop <= 1 ~ 3L,
            TRUE ~ NA_integer_
          ),
          secal_DT = case_when(
            hhs_score >= 5 ~ 5L,
            hhs_score == 4 ~ 4L,
            hhs_score >= 1 & hhs_score <= 3 ~ 3L,
            hhs_score == 0 ~ 1L,
            TRUE ~ NA_integer_
          ),
          secal_final_score = pmax(secal_DT, secal_NC_score, na.rm = TRUE),
          secal_lsg = case_when(
            secal_final_score >= 3 ~ 1L,
            secal_final_score >=1 & secal_final_score <= 2 ~ 0L,
            TRUE ~ NA_integer_
          ),
          secal_cg = case_when(
            rcsi_acceptable + lcsi_acceptable + cap_gap >= 1 ~ 1L,
            rcsi_acceptable + lcsi_acceptable + cap_gap == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          secal_lsg_vln = case_when(
            secal_lsg == 1 & vulnerabilite == 1 ~ 1L,
            secal_lsg == 0 | vulnerabilite == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          
          #Has LSG
          has_lsg = case_when(
            (secal_lsg + protection_lsg + sante_nut_lsg + educ_lsg + eha_lsg + abna_lsg) >0 ~ 1L,
            (secal_lsg + protection_lsg + sante_nut_lsg + educ_lsg + eha_lsg + abna_lsg) == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          #Has CG
          has_cap_gap = case_when(
            (cap_gap + abna_cg + eha_cg + educ_cg + sante_nut_cg + protection_cg + secal_cg) >= 1 ~ 1L,
            (cap_gap + abna_cg + eha_cg + educ_cg + sante_nut_cg + protection_cg + secal_cg) == 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          #Has LSG and vulnerabilities
          has_lsg_vuln = case_when(
            has_lsg == 1L & vulnerabilite == 1L ~ 1L,
            has_lsg == 0L & vulnerabilite == 0L ~ 0L,
            TRUE ~ NA_integer_
          ),
          # No LSG but CG
          no_lsg_cap_gap = case_when(
            has_lsg == 0L & has_cap_gap == 1L ~ 1L,
            has_lsg == 0L & has_cap_gap >= 0L ~ 0L,
            TRUE ~ NA_integer_
          ),
          
          no_lsg_vuln = case_when(
            has_lsg == 0L & vulnerabilite == 1L ~ 1L,
            has_lsg == 0L & vulnerabilite >= 0L ~ 0L,
            TRUE ~ NA_integer_
          ),
          no_lsg_cap_gap_vuln = case_when(
            no_lsg_cap_gap == 1L & vulnerabilite == 1L ~ 1L,
            no_lsg_cap_gap == 0L & vulnerabilite >= 0L ~ 0L,
            TRUE ~ NA_integer_
          ),
          # MSNI
          msni = pmax(abna_final_score, eha_final_score, educ_final_score, sante_nut_final_score, protection_final_score, secal_final_score, na.rm = T),
          msni_needs = case_when(
            msni >= 3 ~ 1L,
            msni >= 1 & msni < 3 ~ 0L,
            TRUE ~ NA_integer_
          )
          )
        df <- df%>%
          mutate(
          nb_lsg = rowSums(dplyr::select(., secal_lsg, protection_lsg, sante_nut_lsg, educ_lsg, eha_lsg, abna_lsg), na.rm = T)
        )
    return(df)
}

cleaned_data_adm1 <- lsg_analysis(cleaned_data_adm1)
msni_3plus_regions <- cleaned_data_adm1%>%
  as_survey(weights = weights_sampling, cluster_id = sampling_id)%>%
  group_by(admin1)%>%
  summarise(msni3plus = survey_mean(msni_needs))
write_csv(msni_3plus_regions, "outputs/LSG/MSNI/admin0/msni_3plus_regions.csv")

msni_4plus_regions <- cleaned_data_adm1%>%
  mutate(msni_4plus = case_when(msni >= 4 ~ 1L,
                                msni < 4 ~ 0L,
                                TRUE ~ NA_integer_))%>%
  as_survey(weights = weights_sampling, cluster_id = sampling_id)%>%
  group_by(admin1)%>%
  summarise(msni_4plus = survey_mean(msni_needs))

cleaned_data_adm1%>%
  as_survey(weights = weights_sampling, cluster_id = sampling_id)%>%
  summarise(eha_mean = mean(eha_lsg, na.rm = T))

write_csv(msni_4plus_regions, "outputs/LSG/MSNI/admin0/msni_4plus_regions.csv")

# IICI

non_critiques <- list(abna = list("dommage_abri", "probleme_isolation", "surface_pers", "modalite_occup"),
                   eha = list("latrine_moins20", "suffisamment_eau", "temps_eau", "savon", "dispo_lavageMain",
                                      "auMoins_aprLat_avantMan", "latrine_pashygenique", "diarr"),
                   educ = list("enfants_scol", "barrieres_edu"),
                   sante_nut = list("lieu_accouch", "temps_service_sante"),
                   protection = list("membres_doc", "expo_risqueSec", "enfant_detress_psy", "travail_enfant", "vbg","titres"),
                   secal = list("acces_marche", "principale_source_rev", "fsc_acceptable")
                   )

NC_scores <- c("abna_NC_score", "eha_NC_score", "educ_NC_score", "sante_nut_NC_score", "protection_NC_score", "secal_NC_score", "protection_DT")
DT_scores <- c("abna_DT", "eha_DT", "educ_DT", "sante_nut_DT", "secal_DT")
final_scores <- c("abna_final_score", "eha_final_score", "educ_final_score", "sante_nut_final_score", "protection_final_score", "secal_final_score")
final_scores_labels <- c("ABNA", "EHA", "Education", "Santé", "Protection", "SécuritéAlimentaire")

sectors <- c("abna", "eha", "educ", "sante_nut", "protection", "secal")

vulnerabilites <- c("depl_plus6m","auMoinsUneWG","femme_cheffe_menage","enfant_chef_menage","pers_agee_chef_menage")


results_sector <- function(data, sector, group = "status", lsgs_list = final_scores, lsgs_labels_list = final_scores_labels, list_indic = non_critiques,
                           weights = "weights_sampling", weights_fun = combined_weights_adm1, strata = "sampling_id",
                           write_files = F, admin_level = "admin0"){
  
  if(!dir.exists(paste0("outputs/LSG/", sector, "/", admin_level))){
    dir.create(paste0("outputs/LSG/", sector, "/", admin_level))
  }
  print(sector)
  
  final_score <- sym(paste0(sector, "_final_score"))
  sector_lsg <- sym(paste0(sector, "_lsg"))
  lsg_vln <- sym(paste0(sector, "_lsg_vln"))
  group <- sym(group)
  admin_level <- sym(admin_level)
  
  indic_NC <- unlist(list_indic[[sector]])

  data <- data%>%
    mutate(!!final_score := fct_recode(as.factor(!!final_score), "1" = "1", "2" = "2", "3" = "3", "4" = "4", "4+" = "5"))
    
  
  data_survey <- as_survey(data, weights = !!weights)

  results <- list()
  
  results$lsg <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(
      perc_menage_LSG = survey_mean(!!sector_lsg, na.rm = T)
    )%>%
    select(-perc_menage_LSG_se)
  
  results$lsg_grp <- data_survey%>%
    group_by(!!group, !!admin_level)%>%
    summarise(
      perc_menage_LSG = survey_mean(!!sector_lsg, na.rm = T)
    )%>%
    select(-perc_menage_LSG_se)

  results$lsg_seuils <- data_survey%>%
    group_by(!!final_score, !!admin_level)%>%
    summarise(prop_LSG_score = survey_mean(na.rm = T))%>%
    select(-prop_LSG_score_se)
  
  results$lsg_seuils_grp <- data_survey%>%
    group_by(!!group, !!final_score, !!admin_level)%>%
    summarise(prop_LSG_score = survey_mean(na.rm = T))%>%
    select(-prop_LSG_score_se)%>%
    pivot_wider(names_from = !!final_score, values_from = prop_LSG_score)
  
  results$no_lsg_vuln <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(
      perc_menage_no_lsg_vuln = survey_mean(no_lsg_vuln, na.rm = T)
    )%>%
    select(-perc_menage_no_lsg_vuln_se)
    
  
  results$no_lsg_vuln_grp <- data_survey%>%
    group_by(!!group, !!admin_level)%>%
    summarise(
      perc_menage_no_lsg_vuln = survey_mean(no_lsg_vuln, na.rm = T)
    )%>%
    select(-perc_menage_no_lsg_vuln_se)
  
  results$lsg_vln <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(
      perc_menage_lsg_vuln = survey_mean(!!lsg_vln, na.rm = T)
    )%>%
    select(-perc_menage_lsg_vuln_se)
  
  results$lsg_vln_grp <- data_survey%>%
    group_by(!!admin_level, !!group)%>%
    summarise(
      perc_menage_lsg_vuln = survey_mean(!!lsg_vln, na.rm = T)
    )%>%
    select(-perc_menage_lsg_vuln_se)
  
  results$top_5_admin1 <- data_survey%>%
    group_by(admin1)%>%
    summarise(
      perc_menage_LSG = survey_mean(!!sector_lsg, na.rm = T)
    )%>%
    select(-perc_menage_LSG_se)%>%
    top_n(5, perc_menage_LSG)
  
  results$top_5_admin2 <- data_survey%>%
    group_by(admin2)%>%
    summarise(
      perc_menage_LSG = survey_mean(!!sector_lsg, na.rm = T)
    )%>%
    select(-perc_menage_LSG_se)%>%
    top_n(5, perc_menage_LSG )
  
  results$lsg_indic <- lapply(indic_NC, function(x){
    x <- sym(x)
    result <- data_survey%>%
      group_by(!!admin_level)%>%
      summarise(!!x := survey_mean(!!x, na.rm =T))%>%
      select(-paste0(x, "_se"))
    return(result)
  })%>%
    bind_cols()%>%
    rename(!!admin_level := !!paste0(admin_level, "...1"))%>%
    select(-contains("..."))
  
  names(results$lsg_indic) <- gsub("\\..*$", "", names(results$lsg_indic))

  if(write_files == TRUE){
    sapply(names(results), function(x){write_csv(results[[x]], paste0("outputs/LSG/", sector,"/",admin_level,"/", sector, "_",x, ".csv"))})
  }
  
  contrib_NC_indic <- results$lsg_indic%>%
    group_by(!!admin_level)%>%
    pivot_longer(-!!admin_level, names_to = "variables", values_to = "values")
  
  if(length(indic_NC)>2){
    plot_set_percentages <- Setviz::plot_set_percentages(
      data, indic_NC, weighting_function = combined_weights_adm1, 
      nintersects = 12, exclude_unique = T, mutually_exclusive_sets = FALSE, 
      label = NULL, round_to_1_percent = TRUE
    )
    png(paste0("outputs/LSG/", sector,"/contribution_LSG_set_", sector, ".png"), width = 6, height = 5, units = "in", res = 1200)
    plot_set_percentages
    dev.off()
  }
  
  if(admin_level == "admin0"){
    contrib_NC_indic_grap <- ggplot(contrib_NC_indic, aes(x = reorder(variables, values, sum), y = values))+
      geom_bar(stat= "identity")+
      coord_flip()+
      ggsave(paste0("outputs/LSG/", sector,"/contribution_LSG_", sector, ".png"))
    
      
    
    index_fill <- c("#EE5A59", "#F7ACAC", "#FACDCD", "#A7A9AC", "#58585A")
    index_labels <- c("Extrême+ (4+)", "Extrême (4)", "Sévère (3)", "Stress (2)", "Minimal (1)")
    
    
    index_chart_overall <- ggplot(results$lsg_seuils, aes(x = prop_LSG_score, y = ""))+
      geom_bar(aes(fill = forcats::fct_rev(!!final_score)), stat = "identity")+
      labs(fill = "", x = "", y = "") +
      theme_minimal() + 
      scale_fill_manual(values = index_fill, labels = index_labels) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1, 
                                                         scale = 1))+
      ggsave(paste0("outputs/LSG/", sector,"/severity_bar_chart_", sector, ".png"))
    
    lsg_seuils_grp_barchart <- results$lsg_seuils_grp%>%
      pivot_longer(c(-!!group, -!!admin_level), names_to = "seuils", values_to = "prop_LSG_score")%>%
      mutate(seuils = as.factor(seuils))%>%
      filter(seuils != "NA")
      
    index_chart_grp <- ggplot(lsg_seuils_grp_barchart, aes(x = prop_LSG_score, y = !!group))+
      geom_bar(aes(fill = forcats::fct_rev(seuils)), stat = "identity")+
      labs(fill = "", x = "", y = "") +
      theme_minimal() + 
      scale_fill_manual(values = index_fill, labels = index_labels)+
      scale_x_continuous(labels = scales::percent_format(accuracy = 1, 
                                                         scale = 1))+
      ggsave(paste0("outputs/LSG/", sector,"/severity_bar_chart_grp", sector, ".png"))
  }
  
  return(results)
}


a <- lapply(sectors, results_sector, data = cleaned_data_adm1, write_files = T)
b <- lapply(sectors, admin_level = "admin1", results_sector, data = cleaned_data_adm1, write_files = T)

cleaned_data_adm1%>%
  mutate(eha_no_other = case_when(
    eha_final_score >= 3 & any(lsgs_list_noEHA >=3) ~ 1L,
    eha_final_score < 3 & any(lsgs_list_noEHA <3) ~ 0L,
    TRUE ~ NA_integer_
  ))%>%
  summarise(eha_no_other = mean(eha_no_other, na.rm = T))


msni_analysis <- function(data, group = "status", lsgs_list = final_scores, lsgs_labels_list = final_scores_labels, list_indic = non_critiques,
                          weights = "weights_sampling", weights_fun = combined_weights_adm1, strata = "sampling_id", cap_gap_var = "has_cap_gap",
                          admin_level = "admin0"){
  
  if(!dir.exists(paste0("outputs/LSG/MSNI/", admin_level))){
    dir.create(paste0("outputs/LSG/MSNI/", admin_level))
  }

  group <- sym(group)
  cap_gap_var <- sym(cap_gap_var)
  admin_level <- sym(admin_level)
  
  msni_intersection <- msni19::index_intersections(data,
                                                   lsg =  lsgs_list,
                                                   lsg_labels = lsgs_labels_list,
                                                   weighting_function = weights_fun,
                                                   y_label = "% dans le besoin par combinaison des secteurs",
                                                   exclude_unique = T,
                                                   mutually_exclusive_sets = F,
                                                   round_to_1_percent = T,
                                                   print_plot = T,
                                                   plot_name = "intersection",
                                                   path = "outputs/LSG/MSNI"
  )
  msni_intersection_withUnique <- msni19::index_intersections(data,
                                                   lsg =  lsgs_list,
                                                   lsg_labels = lsgs_labels_list,
                                                   weighting_function = weights_fun,
                                                   y_label = "% dans le besoin par combinaison des secteurs",
                                                   exclude_unique = F,
                                                   mutually_exclusive_sets = F,
                                                   round_to_1_percent = T,
                                                   print_plot = T,
                                                   plot_name = "intersection_withUnique",
                                                   path = "outputs/LSG/MSNI"
  )
  
  
  msni_radar_grp <- msni19::radar_graph(data,
                                        lsg =  lsgs_list,
                                        lsg_labels = lsgs_labels_list,
                                        weighting_function = weights_fun,
                                        group = group,
                                        group_order = c("host", "pdi"),
                                        group_labels = c("Communauté hôte", "PDI"),
                                        print_plot = T,
                                        plot_name = "radar_grp",
                                        path = "outputs/LSG/MSNI"
  )
  
  msni_radar <- msni19::radar_graph(data,
                                        lsg =  lsgs_list,
                                        lsg_labels = lsgs_labels_list,
                                        weighting_function = weights_fun,
                                        print_plot = T,
                                        plot_name = "radar",
                                        path = "outputs/LSG/MSNI"
  )
  

  data <- data%>%
    mutate(cap_gap_scaled = case_when(!!cap_gap_var == 1 ~ 3L,
                               !!cap_gap_var == 0 ~ 1L,
                               TRUE ~ NA_integer_))
  
  msni_venn <- msni19::venn_msni(
    data,
    lsg =  lsgs_list,
    capacity_gaps = "cap_gap_scaled",
    weighting_function = combined_weights_adm1,
    print_plot = T,
    plot_name = "venn",
    path = "outputs/LSG/MSNI"
  )
  
  msni19::severity_lines(data,
                         lsg = final_scores,
                         group = "status",
                         group_order = c("host", "pdi"),
                         group_labels = c("Communauté hôte", "PDI"),
                         weighting_function = combined_weights_adm1,
                         print_plot = T,
                         plot_name = "severity_lines",
                         path = "outputs/LSG/MSNI",
                         language = "fr"
  )
  
  msni_sunburst <- msni19::sunburst_msni(df = cleaned_data_adm1,
                          msni = "msni",
                          fsl_lsg = "secal_final_score",
                          health_lsg = "sante_nut_final_score",
                          protection_lsg = "protection_final_score",
                          shelter_lsg = "abna_final_score",
                          wash_lsg = "eha_final_score",
                          capacity_gaps = "has_cap_gap",
                          impact = "impact",
                          msni_filter = c(3, 4, 5),
                          weighting_function = combined_weights_adm1,
                          fsl_wash_branch = F,
                          health_prot_shelt_branch = F,
                          impact_branch = F,
                          print_plot = F,
                          plot_name = "sunburst",
                          path = "outputs/LSG/MSNI",
                          language = "fr"
  )
  png(paste0("outputs/LSG/MSNI/msni_sunburst.png"), width = 6, height = 5, units = "in", res = 1200)
  msni_sunburst
  dev.off()
  
  data_survey <- data%>%
    mutate(msni := fct_recode(as.factor(msni), "1" = "1", "2" = "2", "3" = "3", "4" = "4", "4+" = "5"))%>%
    as_survey(weights = !!weights)
  
  results <- list()
  
  results$msni_seuils <- data_survey%>%
    group_by(!!admin_level, msni)%>%
    summarise(prop_msni_score = survey_mean(na.rm = T))%>%
    select(-prop_msni_score_se)
  
  write_csv(results$msni_seuils, paste0("outputs/LSG/MSNI/",admin_level, "/msni_overall_score.csv"))
  
  results$msni_seuils_grp <- data_survey%>%
    group_by(!!admin_level, !!group, msni)%>%
    summarise(prop_msni_score = survey_mean(na.rm = T))%>%
    select(-prop_msni_score_se)%>%
    pivot_wider(names_from = msni, values_from = prop_msni_score)
  
  write_csv(results$msni_seuils_grp, paste0("outputs/LSG/MSNI/", admin_level, "/msni_grp_score.csv"))
  
  results$nb_lsg_groups <- data_survey%>%
    group_by(!!admin_level, !!group, nb_lsg)%>%
    summarise(prop_nb_lsg = survey_mean(na.rm = T))%>%
    select(-prop_nb_lsg_se)%>%
    pivot_wider(names_from = nb_lsg, values_from = prop_nb_lsg)
  
  write_csv(results$nb_lsg_groups, paste0("outputs/LSG/MSNI/",admin_level,"/nb_lsg_groups.csv"))
  
  return(results)
}

msni_resutls <- msni_analysis(cleaned_data_adm1)
msni_resutls_admin1 <- msni_analysis(cleaned_data_adm1, admin_level = "admin1")


cg_analysis <- function(data, group = "status", lsgs_list = final_scores, lsgs_labels_list = final_scores_labels, list_indic = non_critiques,
                        weights = "weights_sampling", weights_fun = combined_weights_adm1, strata = "sampling_id", cap_gap_var = "has_cap_gap",
                        write_files = FALSE, admin_level = "admin0"){
  
  if(!dir.exists(paste0("outputs/LSG/CG/",admin_level))){
    dir.create(paste0("outputs/LSG/CG/",admin_level))
  }
  admin_level <- sym(admin_level)
  
  data <- data%>%
    mutate(cap_gap_scaled = case_when(!!cap_gap_var == 1 ~ 3L,
                                                !!cap_gap_var == 0 ~ 1L,
                                                TRUE ~ NA_integer_))
  group <- sym(group)
  data_survey <- data%>%
    as_survey(weights = !!weights)
  
  results <- list()
  
  results$no_lsg_cap_gap <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(prop_no_lsg_cap_gap = survey_mean(no_lsg_cap_gap, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_se)
  
  results$no_lsg_cap_gap_grp <- data_survey%>%
    group_by(!!admin_level, !!group)%>%
    summarise(prop_no_lsg_cap_gap = survey_mean(no_lsg_cap_gap, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_se)
  
  results$no_lsg_cap_gap_vuln <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(prop_no_lsg_cap_gap_vuln = survey_mean(no_lsg_cap_gap_vuln, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_vuln_se)
  
  results$no_lsg_cap_gap_vuln_grp <- data_survey%>%
    group_by(!!admin_level,!!group)%>%
    summarise(prop_no_lsg_cap_gap_vuln = survey_mean(no_lsg_cap_gap_vuln, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_vuln_se)
  
  results$no_lsg_cap_gap_admin1 <- data_survey%>%
    group_by(admin1)%>%
    summarise(prop_no_lsg_cap_gap = survey_mean(no_lsg_cap_gap, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_se)
  
  results$no_lsg_cap_gap_admin2 <- data_survey%>%
    group_by(admin2)%>%
    summarise(prop_no_lsg_cap_gap = survey_mean(no_lsg_cap_gap, na.rm = T))%>%
    select(-prop_no_lsg_cap_gap_se)
  
  msni_venn <- msni19::venn_msni(
    data,
    lsg =  lsgs_list,
    capacity_gaps = "cap_gap_scaled",
    weighting_function = combined_weights_adm1,
    print_plot = T,
    plot_name = "venn",
    path = "outputs/LSG/CG"
  )
  
  results$venn_diag <-
    data.frame(
      has_LSG_no_CG = msni_venn$data$fitted.values["A"]/sum(msni_venn$data$fitted.values[1:3]),
      no_LSG_has_CG = msni_venn$data$fitted.values["B"]/sum(msni_venn$data$fitted.values[1:3]),
      has_lsg_has_CG = msni_venn$data$fitted.values["A&B"]/sum(msni_venn$data$fitted.values[1:3])
    )
  
  
  
  if(write_files == TRUE){
    sapply(names(results), function(x){write_csv(results[[x]], paste0("outputs/LSG/CG/",admin_level,"/cg_",x, ".csv"))})
  }
  
  return(results)
  
}

cg_results <- cg_analysis(cleaned_data_adm1, write_files = TRUE)
cg_results_admin1 <- cg_analysis(cleaned_data_adm1, write_files = TRUE, admin_level = "admin1")


vulnerabilites_analysis <- function(data, group = "status", lsgs_list = final_scores, lsgs_labels_list = final_scores_labels, list_indic = non_critiques,
                                    weights = "weights_sampling", weights_fun = combined_weights_adm1, strata = "sampling_id", cap_gap_var = "has_cap_gap",
                                    list_vuln = vulnerabilites, sectors_list = sectors,
                                    write_files = FALSE, admin_level = "admin0"){
  if(!dir.exists(paste0("outputs/LSG/vulnerabilites/", admin_level))){
    dir.create(paste0("outputs/LSG/vulnerabilites/", admin_level))
  }
  data <- data%>%
    mutate(cap_gap_scaled = case_when(!!cap_gap_var == 1 ~ 3L,
                                      !!cap_gap_var == 0 ~ 1L,
                                      TRUE ~ NA_integer_),
           )
  
  group <- sym(group)
  admin_level <- sym(admin_level)
  data_survey <- data%>%
    as_survey(weights = !!weights)
  
  results <- list()
  
  results$has_lsg_vuln <- data_survey%>%
    group_by(!!admin_level)%>%
    summarise(prop_has_lsg_vuln = survey_mean(has_lsg_vuln, na.rm = T))%>%
    select(-prop_has_lsg_vuln_se)
  
  results$has_lsg_vuln_grp <- data_survey%>%
    group_by(!!admin_level, !!group)%>%
    summarise(prop_has_lsg_vuln = survey_mean(has_lsg_vuln, na.rm = T))%>%
    select(-prop_has_lsg_vuln_se)
  
  results$vuln_indic <- lapply(list_vuln, function(x){
    x <- sym(x)
    result <- data_survey%>%
      group_by(!!admin_level)%>%
      summarise(!!x := survey_mean(!!x, na.rm =T))%>%
      select(-paste0(x, "_se"))
    return(result)
  })%>%
    bind_cols()%>%
    rename(!!admin_level := !!paste0(admin_level, "...1"))%>%
    select(-contains("..."))
  
  
  names(results$vuln_indic) <- gsub("\\..*$", "", names(results$vuln_indic))
  

  contrib_vuln_indic <- results$vuln_indic%>%
    pivot_longer(-!!admin_level, names_to = "variables", values_to = "values")
  
  if(length(list_vuln)>2){
    plot_set_percentages <- Setviz::plot_set_percentages(
      data, list_vuln, weighting_function = combined_weights_adm1, 
      nintersects = 12, exclude_unique = T, mutually_exclusive_sets = FALSE, 
      label = NULL, round_to_1_percent = TRUE
    )
    png("outputs/LSG/vulnerabilites/vuln_contrib_factor.png", width = 6, height = 5, units = "in", res = 1200)
    plot_set_percentages
    dev.off()
  }
  

  sectors_lsgs <- paste0(sectors_list, "_lsg")
  
  sectors_lsgs_lsg_cg <- c(sectors_lsgs, "has_lsg", "has_cap_gap")
  
  has_lsg_sectors_profil_chef_menage <-data_survey %>%
    group_by(!!admin_level, vuln_profil_chef_menage)%>%
    summarise_at(sectors_lsgs_lsg_cg, survey_mean, na.rm = T)%>%
    select(- ends_with("_se"))%>%
    rename(profil_vuln = vuln_profil_chef_menage )
  
  has_lsg_sectors_profil_depl_plus6m <- data_survey %>%
    group_by(!!admin_level, depl_plus6m)%>%
    summarise_at(sectors_lsgs_lsg_cg, survey_mean, na.rm = T)%>%
    select(- ends_with("_se"), -contains("SRVYR_WITHIN"))%>%
    filter(depl_plus6m == 1)%>%
    mutate(depl_plus6m = "depl_plus6m")%>%
    rename(profil_vuln = depl_plus6m)
  
  has_lsg_sectors_profil_auMoinsUneWG <- data_survey %>%
    group_by(!!admin_level, auMoinsUneWG)%>%
    summarise_at(sectors_lsgs_lsg_cg, survey_mean, na.rm = T)%>%
    select(- ends_with("_se"), -contains("SRVYR_WITHIN"))%>%
    filter(auMoinsUneWG == 1)%>%
    mutate(auMoinsUneWG = "auMoinsUneWG")%>%
    rename(profil_vuln = auMoinsUneWG)
  
  results$has_lsg_sectors_profil <- bind_rows(has_lsg_sectors_profil_chef_menage, has_lsg_sectors_profil_depl_plus6m, has_lsg_sectors_profil_auMoinsUneWG) 
  
  
  
  if(write_files == TRUE){
    sapply(names(results), function(x){write_csv(results[[x]], paste0("outputs/LSG/vulnerabilites/",admin_level, "/vuln_",x, ".csv"))})
  }
  
}


result_vulnerabilites <- vulnerabilites_analysis(cleaned_data_adm1, write_files = T)
result_vulnerabilites_admin1 <- vulnerabilites_analysis(cleaned_data_adm1, admin_level = "admin1", write_files = T)
