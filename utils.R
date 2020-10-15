

loadInstall_package <- function(package){
  new_pkg_list <- gsub(".*/", "",package)
  new_pkg <- package[!(new_pkg_list %in% installed.packages()[, "Package"])]
  
  if (length(new_pkg)>0){
    install.packages(new_pkg[!grepl("\\/", new_pkg)], dependencies = TRUE)
    devtools::install_github(new_pkg[grepl("\\/", new_pkg)], dependencies = TRUE)
  }
  invisible(lapply(new_pkg_list, library, character.only = TRUE))
}

cleaning_chr <- function(string){
  new_string <- stringi::stri_trans_general(string, "Any-ASCII")
}

#' Replace indices by UUID to ease retrieval of records in dataset in cleaninginspectoR results
#'
#' @param inspectorResult a dataframe with cleaninginspectoR results
#' @param data a dataframe
#' @param uuid.column.name The name of the column containing the uuids. Default: "uuid".

#' @details Matches indices from cleaninginspectoR results with the UUIDs present in the data.
#' @return A dataframe with the cleaninginspectoR results and an additional UUID column.
#' @export
index_toUUID <- function(inspectorResult, data, uuid.column.name = "uuid") {
  
  if(any(!names(inspectorResult) %in% c("index","value","variable","has_issue","issue_type"))){
    extra_cols <- names(inspectorResult)[!names(inspectorResult) %in% c("index","value","variable","has_issue","issue_type")]
    warning(paste("Column not in standard cleaninginspectoR output: ",
                  extra_cols,
                  collapse  = "\n"
    )
    )
  }
  
  if(!is.data.frame(data)){stop("Input a dataframe as data ")}
  if(!is.data.frame(inspectorResult)){stop("Input a dataframe as inspectorResult ")}
  if(!is.character(uuid.column.name)){stop("Input a character vector as uuid.column.name ")}
  
  
  
  data$row_number <- row.names(data)
  
  data_uuidIndex <- dplyr::select(data, !!uuid.column.name, row_number)
  
  inspectorResult$index <- as.character(inspectorResult$index)
  inspectorResult%>%
    left_join(data_uuidIndex, by = c("index" = "row_number"))
  
}

remove_delParents_fromLoop <- function(loop, parent, uuid.name.loop = NULL, uuid.name.parent = NULL){
  if (is.data.frame(loop) == FALSE) 
    stop("loop parameter has to be a dataframe")
  if (is.data.frame(loop) == FALSE) 
    stop("parent parameter has to be a dataframe")
  if (is.null(uuid.name.parent) == TRUE) {
    uuid.name.parent <- grep("uuid", names(parent), value = T, 
                             ignore.case = T)
    if (length(uuid.name.parent) == 0) {
      stop("Could not find the uuid automatically in the parent dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.parent) > 1) {
      uuid.name.parent <- uuid.name.parent[1]
    }
  }
  if (is.null(uuid.name.loop) == TRUE) {
    uuid.name.loop <- grep("uuid", names(loop), value = T, 
                           ignore.case = T)
    if (length(uuid.name.loop) == 0) {
      stop("Could not find the uuid automatically in the loop dataset. Please provide the name of the uuid column as a parameter")
    }
    if (length(uuid.name.loop) > 1) {
      uuid.name.loop <- uuid.name.parent[1]
    }
  }
  
  loop[loop[[uuid.name.loop]] %in% parent[[uuid.name.parent]],]
  
}

output_csv <- function(data, names, folder_path){ 
  write_csv(data, paste0(folder_path, names, ".csv"))
}

create_objects_from_df <- function(dat) {
  purrr::map(names(dat), ~assign(.x, dat[.x][[1]], envir = .GlobalEnv))
}

levels_asBinaryColumns <- function(data, factor_col){
  if(!is.data.frame(data)){
    stop("data must be a dataframe")
  } else{
    data <- as.data.frame(data)
  }
  if(!factor_col %in% names(data)){
    stop("factor_col must be a column in data")
  }
  
  cols_toAdd <- levels(as.factor(data[,factor_col]))
  
  names_cols_toAdd <- paste(factor_col, cols_toAdd, sep = ".")
  
  for(i in seq_along(names_cols_toAdd)){
    data[[names_cols_toAdd[i]]] <- if_else(data[,factor_col] == as.character(cols_toAdd[i]), TRUE, FALSE)
  }
  
  return(data)
}


hhs_recoding <- function(q_yes_no, q_freq, new_var.name){
  
    case_when(
      !!q_yes_no == "non" ~ 0,
      !!q_freq == "rarement" | !!q_freq == "parfois" ~ 1,
      !!q_freq == "souvent" ~ 2
      )
}

lcs_recoding <- function(question){
  case_when(
    !!question == "vendu" | !!question == "oui" ~ 1,
    !!question == "besoin" ~ 0  )
}



form_dictionnary <- function(data, survey, choices, analysisplanTemplate =NULL){
  if(!is.data.frame(data))stop("data must be a data frame")
  if(!is.data.frame(survey))stop("survey must be a data frame")
  if(!is.data.frame(choices))stop("choices must be a data frame")
  if(!is.data.frame(analysisplanTemplate) & !is.null(analysisplanTemplate))stop("analysisplanTemplate must be a data frame")
  
  
  survey_try <- survey%>%
    mutate(type = case_when(
      type == "end repeat" ~ "end_repeat",
      type == "end group" ~ "end_group",
      type == "begin repeat" ~ "begin_repeat",
      type == "begin group" ~ "begin_group",
      TRUE ~ type
    ))%>%
    separate(type, into = c("qtype", "list_name"), sep = " ", remove = FALSE)%>%
    mutate(list_name = case_when(is.na(list_name)~ paste0("liste_",name),
                                 TRUE ~ list_name))
  
  dico <- left_join(survey_try, choices, by = "list_name")%>%
    distinct()%>%
    rename(name_question = name.x, label_question = label.x, name_choice = name.y, label_choice = label.y)%>%
    mutate(question_choice = case_when(is.na(name_choice) ~ name_question,
                                       TRUE ~ paste0(name_question, ".", name_choice))
           )%>%
    select(type, qtype, list_name, name_question, label_question, name_choice, label_choice, question_choice)
  
  if(!is.null(analysisplanTemplate)){
    
    dico <- dico%>%
      filter(name_question %in% analysisplanTemplate$dependent.variable)%>%
      left_join(analysisplanTemplate, by = c("name_question" = "dependent.variable"))%>%
      select(research.question_label, sub.research.question_label, type, list_name, name_question, label_question, name_choice, label_choice, question_choice, label_indicator, dependent.variable.type)%>%
      distinct()
  }
  
  return(dico)
  
}

match_SL <-   function(skipped,value){
  if(skipped == TRUE) {
    value <- "NA.SL"
    return(value)
  }else{
    return(value)
  }
}

value_skipped_col <- function(col_value, col_skipped){
  stopifnot(length(col_value)==length(col_skipped))
  SLs <- map2(col_skipped,col_value, match_SL)%>% unlist()
  
  return(SLs)
}


#' A 
#'
#' @param data data.frame A dataframe with the data to aggregate 
#' @param aggregate_level 
#' @param pop_grp 
#' @param weights 
#'
#' @return
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
      freq_detres_adult = sum(detres_adult* weights, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights, na.rm = T),
      freq_detres_enfants = sum(detres_enft*weights, na.rm = T)/sum(enfant*weights, na.rm = T),
      #Naissances
      freq_lieu_accouchement.centre_sante = sum(lieu_accouchement.centre_sante * weights, na.rm = T)/sum(total_naissance * weights, na.rm = T),
      freq_lieu_accouchement.maison = sum(lieu_accouchement.maison * weights, na.rm = T)/sum(total_naissance * weights, na.rm = T),
      freq_lieu_accouchement.autre = sum(lieu_accouchement.autre * weights, na.rm = T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.accouche_assiste_domicil = sum(raison_dominicile.accouche_assiste_domicil * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.accouche_centre_ferme = sum(raison_dominicile.centre_ferme * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.accouche_centre_financ_inacc = sum(raison_dominicile.centre_financ_inacc * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.centre_surpeuple = sum(raison_dominicile.centre_surpeuple * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.maternite_pas_sur = sum(raison_dominicile.maternite_pas_sur * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.physique_mere = sum(raison_dominicile.physique_mere * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.rejoindre_centre = sum(raison_dominicile.rejoindre_centre * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      freq_raison_domicil.nsp = sum(raison_dominicile.nsp * weights, na.rm=T)/sum(total_naissance * weights, na.rm = T),
      #Enfants malades
      freq_enfants_5ans_maladie.palu = sum(`maladie_moins_5ans/palu` * weights, na.rm=T)/sum(total_moins_5ans * weights, na.rm = T),
      freq_enfants_5ans_maladie.infect_respiratoire  = sum(`maladie_moins_5ans/infect_respiratoire` * weights, na.rm=T)/sum(total_moins_5ans * weights, na.rm = T),
      freq_enfants_5ans_maladie.diarrhee = sum(`maladie_moins_5ans/diarrhee` * weights, na.rm=T)/sum(total_moins_5ans * weights, na.rm = T),
      freq_enfants_5ans_maladie.autre = sum(`maladie_moins_5ans/autre` * weights, na.rm=T)/sum(total_moins_5ans * weights, na.rm = T),
      freq_enfants_5ans_maladie.nsp = sum(`maladie_moins_5ans/nsp` * weights, na.rm=T)/sum(total_moins_5ans * weights, na.rm = T),
      #Marche
      freq_marche_dif.garcons_moins5 = sum(marche_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_marche_dif.filles_moins5 = sum(marche_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_marche_dif.garcons_5_18 = sum(marche_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_marche_dif.filles_5_18 = sum(marche_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_marche_dif.hommes_18_64 = sum(marche_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_marche_dif.femmes_18_64 = sum(marche_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_marche_dif.hommes_65plus = sum(marche_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_marche_dif.femmes_65plus = sum(marche_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      #Soins
      freq_soins_dif.garcons_moins5 = sum(soins_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_soins_dif.filles_moins5 = sum(soins_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_soins_dif.garcons_5_18 = sum(soins_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_soins_dif.filles_5_18 = sum(soins_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_soins_dif.hommes_18_64 = sum(soins_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_soins_dif.femmes_18_64 = sum(soins_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_soins_dif.hommes_65plus = sum(soins_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_soins_dif.femmes_65plus = sum(soins_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      #Concentration
      freq_concentration_dif.garcons_moins5 = sum(concentration_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_concentration_dif.filles_moins5 = sum(concentration_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_concentration_dif.garcons_5_18 = sum(concentration_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_concentration_dif.filles_5_18 = sum(concentration_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_concentration_dif.hommes_18_64 = sum(concentration_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_concentration_dif.femmes_18_64 = sum(concentration_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_concentration_dif.hommes_65plus = sum(concentration_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_concentration_dif.femmes_65plus = sum(concentration_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      #Vision
      freq_vision_dif.garcons_moins5  = sum(vision_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_vision_dif.filles_moins5 = sum(vision_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_vision_dif.garcons_5_18 = sum(vision_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_vision_dif.filles_5_18 = sum(vision_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T), 
      freq_vision_dif.hommes_18_64 = sum(vision_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_vision_dif.femmes_18_64 = sum(vision_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_vision_dif.hommes_65plus = sum(vision_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_vision_dif.femmes_65plus = sum(vision_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      #Entendre
      freq_entendre_dif.garcons_moins5 = sum(entendre_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_entendre_dif.filles_moins5 = sum(entendre_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_entendre_dif.garcons_5_18 = sum(entendre_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_entendre_dif.filles_5_18 = sum(entendre_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_entendre_dif.hommes_18_64 = sum(entendre_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_entendre_dif.femmes_18_64 = sum(entendre_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_entendre_dif.hommes_65plus = sum(entendre_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_entendre_dif.femmes_65plus = sum(entendre_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      # Communication
      freq_communication_dif.garcons_moins5 = sum(communication_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_communication_dif.filles_moins5 = sum(communication_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_communication_dif.garcons_5_18 = sum(communication_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_communication_dif.filles_5_18 = sum(communication_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_communication_dif.hommes_18_64 = sum(communication_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_communication_dif.femmes_18_64 = sum(communication_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_communication_dif.hommes_65plus = sum(communication_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_communication_dif.femmes_65plus = sum(communication_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      
      #Deces
      freq_deces_dif.garcons_moins5 = sum(deces_dif_garcons_moins5 * weights, na.rm=T)/sum(agegrp_0_5_hommes * weights, na.rm = T),
      freq_deces_dif.filles_moins5 = sum(deces_dif_filles_moins5 * weights, na.rm=T)/sum(agegrp_0_5_femmes * weights, na.rm = T),
      freq_deces_dif.garcons_5_18 = sum(deces_dif_garcons_5_18 * weights, na.rm=T)/sum(agegrp_5_18_hommes * weights, na.rm = T),
      freq_deces_dif.filles_5_18 = sum(deces_dif_filles_5_18 * weights, na.rm=T)/sum(agegrp_5_18_femmes * weights, na.rm = T),
      freq_deces_dif.hommes_18_64 = sum(deces_dif_hommes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_hommes * weights, na.rm = T),
      freq_deces_dif.femmes_18_64 = sum(deces_dif_femmes_18_64 * weights, na.rm=T)/sum(agegrp_18_65_femmes * weights, na.rm = T),
      freq_deces_dif.hommes_65plus = sum(deces_dif_hommes_65plus * weights, na.rm=T)/sum(agegrp_65plus_hommes * weights, na.rm = T),
      freq_deces_dif.femmes_65plus = sum(deces_dif_femmes_65plus * weights, na.rm=T)/sum(agegrp_65plus_femmes * weights, na.rm = T),
      
      freq_raison_deces.diarrhee = sum(raison_deces.diarrhee * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.autre_maladie = sum(raison_deces.autre_maladie * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.morsure = sum(raison_deces.morsure * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.deces_natu = sum(raison_deces.deces_natu * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.faim = sum(raison_deces.faim * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.accident_travail = sum(raison_deces.accident_travail * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.problemes_respi = sum(raison_deces.problemes_respi * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.autre = sum(raison_deces.autre * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.catastrophe_natu = sum(raison_deces.catastrophe_natu * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.accident_conflit = sum(raison_deces.accident_conflit * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.en_couche = sum(raison_deces.en_couche * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      freq_raison_deces.accident_route = sum(raison_deces.accident_route * weights, na.rm=T)/sum(total_deces * weights, na.rm = T),
      # Education
      freq_educ.3_5an_garcon = sum(total_educ_3_5an_garcon*weights, na.rm = T)/sum(total_3_5_hommes*weights, na.rm = T),
      freq_infor_educ.3_5an_garcon = sum(total_infor_educ_3_5an_garcon * weights, na.rm = T)/sum(total_3_5_hommes*weights, na.rm = T),
      freq_non_for_educ.3_5an_garcon = sum(total_non_for_educ_3_5an_garcon * weights, na.rm = T)/sum(total_3_5_hommes*weights, na.rm = T),
      freq_aucune_educ.3_5an_garcon = sum(total_aucune_educ_3_5an_garcon * weights, na.rm = T)/sum(total_3_5_hommes*weights, na.rm = T),
      freq_educ.3_5an_fille = sum(total_educ_3_5an_fille * weights, na.rm = T)/sum(total_3_5_femmes * weights, na.rm = T),
      freq_infor_educ.3_5an_fille = sum(total_infor_educ_3_5an_fille * weights, na.rm = T)/sum(total_3_5_femmes * weights, na.rm = T),
      freq_non_for_educ.3_5an_fille = sum(total_non_for_educ_3_5an_fille * weights, na.rm = T)/sum(total_3_5_femmes * weights, na.rm = T),
      freq_aucune_educ.3_5an_fille = sum(total_aucune_educ_3_5an_fille * weights, na.rm = T)/sum(total_3_5_femmes * weights, na.rm = T),
      
      freq_educ.3_5an_total = sum(educ.3_5an_total*weights, na.rm=T)/sum(total_3_5*weights, na.rm = T),
      freq_infor_educ.3_5an_total = sum(infor_educ.3_5an_total*weights, na.rm = T)/sum(total_3_5*weights, na.rm = T),
      freq_non_for_educ.3_5an_total = sum(non_for_educ.3_5an_total*weights, na.rm = T)/sum(total_3_5*weights, na.rm = T),
      freq_aucune_educ.3_5an_total = sum(aucune_educ.3_5an_total*weights, na.rm = T)/sum(total_3_5*weights, na.rm = T),
      
      freq_educ.6_12an_garcon  = sum(total_educ_6_12an_garcon * weights, na.rm = T)/sum(total_6_12_hommes * weights, na.rm = T),
      freq_infor_educ.6_12an_garcon   = sum(total_infor_educ_6_12an_garcon * weights, na.rm = T)/sum(total_6_12_hommes * weights, na.rm = T),
      freq_non_for_educ.6_12an_garcon  = sum(total_non_for_educ_6_12an_garcon * weights, na.rm = T)/sum(total_6_12_hommes * weights, na.rm = T),
      freq_aucune_educ.6_12an_garcon = sum(total_aucune_educ_6_12an_garcon * weights, na.rm = T)/sum(total_6_12_hommes * weights, na.rm = T),
      freq_educ.6_12an_fille = sum(total_educ_6_12an_fille * weights, na.rm = T)/sum(total_6_12_femmes * weights, na.rm = T),
      freq_infor_educ.6_12an_fille = sum(total_infor_educ_6_12an_fille * weights, na.rm = T)/sum(total_6_12_femmes * weights, na.rm = T),
      freq_non_for_educ.6_12an_fille = sum(total_non_for_educ_6_12an_fille * weights, na.rm = T)/sum(total_6_12_femmes * weights, na.rm = T),
      freq_aucune_educ.6_12an_fille = sum(total_aucune_educ_6_12an_fille * weights, na.rm = T)/sum(total_6_12_femmes * weights, na.rm = T),
      
      freq_educ.6_12an_total = sum(educ.6_12an_total*weights, na.rm=T)/sum(total_6_12*weights, na.rm = T),
      freq_infor_educ.6_12an_total = sum(infor_educ.6_12an_total*weights, na.rm = T)/sum(total_6_12*weights, na.rm = T),
      freq_non_for_educ.6_12an_total = sum(non_for_educ.6_12an_total*weights, na.rm = T)/sum(total_6_12*weights, na.rm = T),
      freq_aucune_educ.6_12an_total = sum(aucune_educ.6_12an_total*weights, na.rm = T)/sum(total_6_12*weights, na.rm = T),
      
      freq_educ.13_17an_garcon = sum(total_educ_13_17an_garcon * weights, na.rm = T)/sum(total_13_17_hommes * weights, na.rm = T),
      freq_infor_educ.13_17an_garcon = sum(total_infor_educ_13_17an_garcon * weights, na.rm = T)/sum(total_13_17_hommes * weights, na.rm = T),
      freq_non_for_educ.13_17an_garcon = sum(total_non_for_educ_13_17an_garcon * weights, na.rm = T)/sum(total_13_17_hommes * weights, na.rm = T),
      freq_aucune_educ.13_17an_garcon = sum(total_aucune_educ_13_17an_garcon * weights, na.rm = T)/sum(total_13_17_hommes * weights, na.rm = T),
      freq_educ.13_17an_fille = sum(total_educ_13_17an_fille * weights, na.rm = T)/sum(total_13_17_femmes * weights, na.rm = T),
      freq_infor_educ.13_17an_fille = sum(total_infor_educ_13_17an_fille * weights, na.rm = T)/sum(total_13_17_femmes * weights, na.rm = T),
      freq_non_for_educ.13_17an_fille = sum(total_non_for_educ_13_17an_fille * weights, na.rm = T)/sum(total_13_17_femmes * weights, na.rm = T),
      freq_aucune_educ.13_17an_fille = sum(total_aucune_educ_13_17an_fille * weights, na.rm = T)/sum(total_13_17_femmes * weights, na.rm = T),
      
      freq_educ.13_17an_total = sum(educ.13_17an_total*weights, na.rm=T)/sum(total_13_17*weights, na.rm = T),
      freq_infor_educ.13_17an_total = sum(infor_educ.13_17an_total*weights, na.rm = T)/sum(total_13_17*weights, na.rm = T),
      freq_non_for_educ.13_17an_total = sum(non_for_educ.13_17an_total*weights, na.rm = T)/sum(total_13_17*weights, na.rm = T),
      freq_aucune_educ.13_17an_total = sum(aucune_educ.13_17an_total*weights, na.rm = T)/sum(total_13_17*weights, na.rm = T),
      
      freq_scolarise = sum(total_scolarise * weights, na.rm = T)/sum(enfant_scolarisable * weights, na.rm = T),
      freq_non_scolarise = sum(total_non_scolarise * weights, na.rm = T)/sum(enfant_scolarisable * weights, na.rm = T),
      
      freq_enfants_moins5_diarrhee = sum(enfant_selle*weights, na.rm = T)/sum(total_moins_5ans*weights, na.rm = T),
      
      freq_travail_fille = sum(travail_fille*weights, na.rm = T)/sum(total_0_17_femmes*weights, na.rm = T),
      freq_travail_garcon = sum(travail_garcon*weights, na.rm = T)/sum(total_0_17_hommes*weights, na.rm = T),
      .groups = "drop"
    )%>%
    pivot_longer(c(-!!aggregate_level, - !!pop_grp), names_to = "variable", values_to = "numbers")%>%
    separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
    distinct()%>%
    select(!!aggregate_level,!!pop_grp,variable, variable_value, numbers)
  
  return(freq)
}


