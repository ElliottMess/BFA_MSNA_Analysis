
# Loading packages

## Sourcing utils functions
source("utils.R")

## Creating list of packages to load
package_list <- c("oliviercecchi/butteR", "ellieallien/hypegrammaR","ellieallien/cleaninginspectoR","hunzikp/rtree",
                  "tidyr","dplyr", "ggplot2", "readr", "stringr", "lubridate", "readxl", "rgdal", "sf")

## Running the packaging loading function
loadInstall_package(package_list)


# Loading necessary data
## Loading raw_data
raw_data <- read_excel("data/bfa2002_msna_2020_final_cleaning_20200830.xlsx", sheet = "clean_data")%>%
    mutate(status = case_when(
      group_pop %in% c("pdi") ~ "pdi",
      group_pop %in% c("pop_local", "migrant_burkina", "rapatrie", "refugie", "migrant_int", "retourne") ~ "host",
      TRUE ~ NA_character_
    ))


### Loading sampling strategies 
samplingStrategies <- read_excel("data/Sampling_RU.xlsx", sheet = "Tableau Sampling Total", n_max = 663)%>%
  select(!starts_with("..."))%>%
  mutate_at(c("host_direct", "host_remote","idp_direct", "idp_telephone"), as.double)%>%
  pivot_longer(cols = c("host_direct", "host_remote","idp_direct", "idp_telephone"), names_to = "sampling_strat", values_to ="surveys_toBeDone")%>%
  filter(surveys_toBeDone >0, !is.na(surveys_toBeDone))%>%
  select(village_P_code, sampling_strat)%>%
  mutate(pcode_popGrp = paste(village_P_code, sampling_strat, sep = "_")
  )


### Cleaning pcodes for other localites

bfa_pcodes <- read_excel("data/sampling-resume-enq.xlsx", sheet = "ITOS ADMIN3 PCODES")%>%
  mutate(adm123 = cleaning_chr( gsub("-","_", tolower(paste0(ADM1_FR, ADM2_FR, ADM3_FR)))))

pcodes_probs <- raw_data%>%
  select(admin1, admin2, admin3, pcode, localite, autre_localite, "_gpsmenage_latitude", "_gpsmenage_longitude", uuid)%>%
  rename("gpsmenage_latitude" = "_gpsmenage_latitude", "gpsmenage_longitude"= "_gpsmenage_longitude")%>%
  filter(localite == 'autre')%>%
  mutate(
    localite = case_when(localite == 'autre' ~ autre_localite,
                         TRUE ~ localite),
    gpsmenage_latitude = case_when(gpsmenage_latitude == "NA" ~ NA_character_ ,
                                   TRUE ~ gpsmenage_latitude),
    gpsmenage_longitude = case_when(gpsmenage_longitude == "NA" ~ NA_character_ ,
                                    TRUE ~ gpsmenage_longitude)
    
  )%>%
  group_by(admin1, admin2, admin3,pcode, localite)%>%
  distinct()%>%
  filter(!is.na(gpsmenage_latitude))


### Loading clusters sampling frame and cleaning centers of clusters

clusterSample <- read_excel("data/Sampling_RU.xlsx", sheet = "sample_admin2_cluster_hexa500m_")%>%
  mutate(X = case_when(is.na(POINT_X) ~ as.double(NEAR_X),
                       TRUE ~ as.double(POINT_X)),
         Y = case_when(is.na(POINT_Y) ~ as.double(NEAR_Y),
                       TRUE ~ as.double(POINT_Y))
  )%>%
  select(-FID)%>%
  mutate(rowID = row_number())%>%
  group_by(id_sampl)%>%
  filter(rowID == min(rowID))%>%
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

### Loading 2stage sampling frame

samplingFrame <- read_excel("data/REACH_BFA_Pop_for_weighting_20201308.xlsx", sheet = "Population")%>%
  group_by(Admin1, Admin1Pcod, Admin2, Admin2Pcod)%>%
  summarise(host = sum(`Total local`, na.rm = TRUE),
            pdi = sum(`Total PDI`, na.rm = TRUE), .groups = "drop")%>%
  pivot_longer(cols = c("host", "pdi"), names_to = "strata", values_to = "Population")%>%
  mutate(strata = paste(Admin2Pcod, strata, sep = "_"))%>%
  filter(!is.na(Population), Population >0)


samplingFrame_adm2 <- samplingFrame%>%
  filter(Admin1 %in% c("Est", "Centre-Nord","Sahel","Boucle du Mouhoun","Nord"))%>%
  mutate(strata = paste(Admin2Pcod, strata, sep = "_"))%>%
  filter(!is.na(Population))



### Creating spatial objects for missingPcodes and clusters centers
cluster_sf <- sf::st_as_sf(clusterSample, coords = c("X", "Y"), crs = 4326)%>%
  select(POINT_X, POINT_Y, id_sampl, psu_id, strata_id, ADM3_PCODE, NEAR_pcode, NEAR_featureNam)%>%
  st_set_crs("EPSG:4326")

missingPoints_sf <- sf::st_as_sf(pcodes_probs, coords = c("gpsmenage_longitude", "gpsmenage_latitude"), crs = 4326)%>%
  st_set_crs("EPSG:4326")

### Loading OCHA's settlement layer

bfa_settlments <- read_csv("data/shapes/bfa_pplp_1m_nga_ocha/bfa_pplp_1m_nga_ocha.csv")%>%
  sf::st_as_sf(coords = c("Coord_X", "Coord_Y"), crs = 4326)
  
### Replacing missing pcodes by closest pcode

distance_check <- butteR::closest_distance_rtree(missingPoints_sf, bfa_settlments)%>%
  select(uuid, pcode.1)


raw_data <- raw_data%>%
  left_join(distance_check, by = "uuid")%>%
  mutate(pcode = case_when(is.na(pcode) ~ pcode.1,
                           TRUE ~ pcode))



### Creating and writting cleaning_log to log changes

cleaning_log_change <- pcodes_probs %>%
  select(uuid, pcode)%>%
  left_join(raw_data, by = "uuid")%>%
  mutate(Auteur = "Elliott Messeiller", Index = NA, uuid=uuid, Date = as.Date("01-09-2020"), Base = NA, Enqueteur = enumerator_id, Question = "Localité et PCode",
         Problème = "Autre choisi pour la localité. Les PCodes ne peuvent donc pas être calculé par ODK pour ces localités. Les localités ne sont pas non plus dans la base de donnée ITOS du COD OCHA pour les localités",
         "Anciennes valeurs" = pcode.x, "Nouvelles valeurs" = pcode.y, ddsagr = NA, "Retour Enquêteur" =NA, "Retour chargé" = NA, Action = "Modifié",
         "Commentaire RBB" =0L,"Commentaire CC" = 0L,	"Commentaire AO" =0L,	"Commentaire GIS" =0L,	"dffgawesd" =0L)%>%
  select(Auteur, Index, uuid, Date, Base, Enqueteur, Question, Problème, `Anciennes valeurs`, "Nouvelles valeurs", ddsagr, "Retour Enquêteur", "Retour chargé", Action,
         "Commentaire RBB","Commentaire CC" = 0L,	"Commentaire AO",	"Commentaire GIS",	"dffgawesd")

write_csv(cleaning_log_change, "outputs/logs/cleaning_log_missingPcodes.csv")


# WEIGHTING #

coords <- c("gpsmenage_longitude", "gpsmenage_latitude")

raw_data_ClusterSmpl <- raw_data%>%
  rename("gpsmenage_latitude" = "_gpsmenage_latitude", "gpsmenage_longitude"= "_gpsmenage_longitude")%>%
  mutate_at(vars(all_of(coords)), as.numeric)%>%
  filter(!is.na(gpsmenage_latitude) & !is.na(gpsmenage_longitude), modalite == "direct" & status == "host")%>%
  sf::st_as_sf(coords = coords, crs = 4326)%>%
  st_set_crs("EPSG:4326")

## Finding closest cluster to all direct host interviews

all_closest_cluster <- butteR::closest_distance_rtree(raw_data_ClusterSmpl, cluster_sf)

closest_cluster <- all_closest_cluster%>%
  filter(dist_m <=1500)%>%
  select(uuid, id_sampl)

raw_data <- raw_data%>%
  left_join(closest_cluster, by = "uuid")%>%
  mutate(pcode = case_when(id_sampl == "id_45382" ~ NA_character_,
                           id_sampl == "id_45749" ~ NA_character_,
                           TRUE ~ pcode
  ))
  

raw_data <- raw_data%>%
  mutate(admin1Pcode = gsub(".{8}$", "", pcode),
         admin2Pcode = gsub(".{6}$", "", pcode),
         admin3Pcode = gsub(".{4}$", "", pcode),
         sampling_id = paste(admin2Pcode, status, sep = "_")
  )


### Weighting cluster sample

raw_data$sampling_strat <- paste(raw_data$status, raw_data$modalite, sep = "_")

clusterSample_adm2 <- clusterSample%>%
  filter(ADM1_PCODE %in% c("BF46", "BF49", "BF52", "BF54", "BF56"))


raw_data_representative <- raw_data%>%
  filter(sampling_strat == "host_direct", id_sampl %in% clusterSample_adm2$id_sampl, sampling_id != "BF5002_host")

cluster_wght_representative <- map_to_weighting(sampling.frame = clusterSample, 
                                              data = raw_data_representative,
                                              sampling.frame.population.column = "population",
                                              sampling.frame.stratum.column = "id_sampl",
                                              data.stratum.column = "id_sampl"
                                              )

admin2_repesentative_wght <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
                                              data = raw_data_representative,
                                              sampling.frame.population.column = "Population",
                                              sampling.frame.stratum.column = "strata",
                                              data.stratum.column = "sampling_id"
)

combined_weights_representative <- combine_weighting_functions(cluster_wght_representative, admin2_repesentative_wght)

raw_data_representative$weights <- combined_weights_representative(raw_data_representative)

write.csv(raw_data_representative, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_representativeData.csv")


### Weighting for quota admin 2 only

raw_data_adm2_quota <- raw_data%>%
  filter(admin1Pcode %in% c("BF46", "BF49", "BF52", "BF54", "BF56"),
         sampling_strat != "host_direct",
         sampling_id %in% samplingFrame_adm2$strata
         )


admin2_affected_wght <- map_to_weighting(sampling.frame = samplingFrame_adm2, 
                                                    data = raw_data_adm2_quota,
                                                    sampling.frame.population.column = "Population",
                                                    sampling.frame.stratum.column = "strata",
                                                    data.stratum.column = "sampling_id"
                                                    )

raw_data_adm2_quota$weights <- admin2_affected_wght(raw_data_adm2_quota)

write.csv(raw_data_adm2_quota, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_quota_ADM2affected.csv")


### Combining cluster weights with admin2 quota weights for 5 most affected regions

raw_data_adm2_affected_all <- rbind(raw_data_representative, raw_data_adm2_quota)

write.csv(raw_data_adm2_affected_all, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM2affected.csv")

### Weighting for admin1 ###

#### Weighting admin1 cluster ####

raw_data_ClusterSmpl_adm1 <- raw_data%>%
  filter(sampling_strat == "host_direct", id_sampl %in% clusterSample$id_sampl,
         !sampling_id %in%  c("NA_host", "NA_pdi"))

cluster_wght_admin1 <- map_to_weighting(sampling.frame = clusterSample,
                                        data = raw_data_ClusterSmpl_adm1,
                                        sampling.frame.population.column = "population",
                                        sampling.frame.stratum.column = "id_sampl",
                                        data.stratum.column = "id_sampl"
)

sf_cluster_wght_admin1 <- map_to_weighting(sampling.frame = samplingFrame, 
                                           data = raw_data_ClusterSmpl_adm1,
                                           sampling.frame.population.column = "Population",
                                           sampling.frame.stratum.column = "strata",
                                           data.stratum.column = "sampling_id"
)

combined_weights_adm1_cluster <- combine_weighting_functions(cluster_wght_admin1, sf_cluster_wght_admin1)

raw_data_ClusterSmpl_adm1$weights <- combined_weights_adm1_cluster(raw_data_ClusterSmpl_adm1)

#### Weighting admin1 quotas ####

raw_data_quota_adm1 <- raw_data%>%
  filter(sampling_strat != "host_direct", sampling_id %in% samplingFrame$strata,
         !sampling_id %in%  c("NA_host", "NA_pdi"))

sf_wght_admin1 <- map_to_weighting(sampling.frame = samplingFrame, 
                                              data = raw_data_quota_adm1,
                                              sampling.frame.population.column = "Population",
                                              sampling.frame.stratum.column = "strata",
                                              data.stratum.column = "sampling_id"
)

raw_data_quota_adm1$weights <- sf_wght_admin1(raw_data_quota_adm1)

### Combining cluster weights with admin1 quota weights

raw_data_adm1_all <- rbind(raw_data_ClusterSmpl_adm1, raw_data_quota_adm1)

write.csv(raw_data_adm1_all, "outputs/datasets/BFA_MSNA_2020_dataset_cleanedWeighted_ADM1.csv")

