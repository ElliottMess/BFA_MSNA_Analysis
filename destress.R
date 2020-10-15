
source("01_PrepingAnalysis.R", encoding = "UTF-8")

cleaned_data_adm1$admin0 <- "BFA"


freq_admin0 <- freq_sum(cleaned_data_adm1, aggregate_level = "admin0", weights = "weights_sampling")

freq_admin0 <- cleaned_data_adm1%>%
  group_by(admin0)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin0), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  mutate(status = "Total")%>%
  select(admin0,status,variable, variable_value, numbers)%>%
  rename(admin_level = admin0)


freq_admin0_grp <- cleaned_data_adm1%>%
  group_by(admin0, status)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin0,-status), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  select(admin0,status,variable, variable_value, numbers)%>%
  rename(admin_level = admin0)

freq_admin1 <- cleaned_data_adm1%>%
  group_by(admin1)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin1), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  mutate(status = "Total")%>%
  select(admin1,status,variable, variable_value, numbers)%>%
  rename(admin_level = admin1)

freq_admin1_grp <- cleaned_data_adm1%>%
  group_by(admin1, status)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin1, -status), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  select(admin1,status,variable, variable_value, numbers)%>%
  rename(admin_level = admin1)


freq_admin2 <- cleaned_data_adm2%>%
  group_by(admin2)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin2), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  mutate(status = "Total")%>%
  select(admin2,status, variable, variable_value, numbers)%>%
  rename(admin_level = admin2)


freq_admin2_grp <- cleaned_data_adm2%>%
  group_by(admin2, status)%>%
  summarise(
    #Détresse psy
    freq_detres_adult = sum(detres_adult* weights_sampling, na.rm = T)/sum(sum(femme,homme, na.rm = T)*weights_sampling, na.rm = T),
    freq_detres_enfants = sum(detres_enft*weights_sampling, na.rm = T)/sum(enfant*weights_sampling, na.rm = T)
  )%>%
  pivot_longer(c(-admin2, -status), names_to = "variable", values_to = "numbers")%>%
  separate(col=variable, into = c("variable", "variable_value"), sep = "\\.")%>%
  distinct()%>%
  select(admin2,status,variable, variable_value, numbers)%>%
  rename(admin_level = admin2)


detress_freq_total <- rbind(freq_admin0,freq_admin0,freq_admin0_grp, freq_admin1,freq_admin1_grp, freq_admin2,freq_admin2_grp)

dfs <- list(freq_admin0,freq_admin0,freq_admin0_grp, freq_admin1,freq_admin1_grp, freq_admin2,freq_admin2_grp)
names(dfs) <- c("freq_admin0freq_admin0","freq_admin0_grp", "freq_admin1","freq_admin1_grp", "freq_admin2","freq_admin2_grp")

write.csv(detress_freq_total, "detresse_psy_frequence.csv")
