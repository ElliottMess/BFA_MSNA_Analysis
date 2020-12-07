

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
    dplyr::rename(name_question = name.x, label_question = label.x, name_choice = name.y, label_choice = label.y)%>%
    mutate(question_choice = case_when(is.na(name_choice) ~ name_question,
                                       TRUE ~ paste0(name_question, ".", name_choice))
           )%>%
    select(type, qtype, list_name, name_question, label_question, name_choice, label_choice, question_choice)
  
  if(!is.null(analysisplanTemplate)){
    
    analysisplanTemplate <- select(analysisplanTemplate, - qtype, -list_name)
    
    dico <- dico%>%
      filter(name_question %in% analysisplanTemplate$dependent.variable)%>%
      left_join(analysisplanTemplate, by = c("name_question" = "dependent.variable"))%>%
      filter(name_question %in% analysisplanTemplate$dependent.variable)%>%
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

