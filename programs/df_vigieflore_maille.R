source("programs/library.R")

reg_dep = read.csv2("data/departements-france.csv", sep=",")
taxref13 = read.csv2("data/TAXREF_v13_2019/TAXREFv13_plantae.csv", sep = ";")

df_habitats = read.csv2(file = "data/Intitute_habitats_VF_CORINE_Biotope.csv", encoding = "utf-8") %>%
  left_join(data.frame(nom_ct = c("agricole", "aquatique", "arbustif", "forestier", "littoral",
                                  "prairie", "rocheux", "ruderal", "tourbeux", "urbain"),
                       color_ct = c("#FBAF00", "#007CBE", "#9CEC5B", "#00AF54", "#34E4EA",
                                    "#FFD639", "#E26D5A", "#EFA8B8", "#656176", "#74A4BC")),
            by = c("corineType" = "nom_ct"))

# Data frame stocké sur le serveur FTP
df_vigieflore = read.csv2(file = "data/export_vigie_flore.csv", sep = ",") %>%
  mutate(session_year = strftime(as.Date(session_date), "%Y"),
         cd_corine = as.integer(str_extract(cd_CB, "^[^\\.]*"))) %>%
  filter(session_year != "4", !is.na(taxon_cd_nom)) %>%
  mutate(session_year_factor_all = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))%>%
  left_join(reg_dep, by = c("cd_departement" = "code_departement")) %>%
  left_join(df_habitats, by = c("cd_corine" = "corine2")) %>%
  left_join(taxref13, by = c("taxon_cd_nom" = "CD_NOM"))

#cd_CB
if (!exists("maille_name")) {
  maille_name = "--050609-Chevalier-Adon-"
}

df_vigieflore_maille = df_vigieflore %>%
  filter(maille_id == maille_name)

df_sp_maille = df_vigieflore %>%
  filter(code_region == unique(df_vigieflore_maille$code_region)) %>%
  group_by(maille_id, taxon_cd_nom) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(maille_id) %>%
  summarise(n = n(), .groups = 'drop')

# Data frame des espèces avec leur nombre de fois où il sont été vus
# NB : on ne prend que les espèces, les individus avec seulement famille ou genre
# ne sont pas conservés
df_sp = df_vigieflore_maille %>%
  group_by(taxon, NOM_VERN) %>%
  summarise(sum_obs = n(), .groups = 'drop')

# On retient les 5 premiers noms 
lst_top_5 = (df_sp %>% filter(!is.na(taxon)) %>% arrange(desc(sum_obs)) %>%
               mutate(taxon_cmb = paste0(taxon, " (", as.character(sum_obs), ")")) %>%
               pull(taxon_cmb))[1:5]
# On supprime les NA s'il y a moins de 5 noms
lst_top_5 = lst_top_5[!is.na(lst_top_5)]

lst_top_5_coll = paste0("\n\n- ", paste(lst_top_5, collapse = "\n\n- "), "\n\n")

# Chaque espèce avec sa première année d'observation
df_sp_year = df_vigieflore_maille %>%
  group_by(taxon_cd_nom, taxon) %>%
  summarise(session_year = min(session_year),.groups = 'drop') %>%
  mutate(session_year_factor = factor(session_year, levels = as.character(min(session_year, na.rm = T):max(session_year, na.rm = T))))

# Richesse cumulée
df_rich_cum = df_sp_year %>%
  group_by(session_year, session_year_factor) %>%
  summarise(rich_sup = n(), .groups = 'drop') %>%
  arrange(session_year) %>%
  mutate(rich_cum = cumsum(rich_sup))

# Espèces observées lors de la dernière année de relevé
df_sp_lst_year = df_sp_year %>%
  filter(session_year == max(df_vigieflore_maille$session_year),
         !is.na(taxon))
if (nrow(df_sp_lst_year) > 0) {
  nb_new_sp = nrow(df_sp_lst_year)
  lst_sp_lst_year = paste0("\n\n- ", paste(sort(df_sp_lst_year$taxon), collapse = "\n\n- "), "\n\n")
}else{
  nb_new_sp = "Aucune"
  lst_sp_lst_year = paste0("\n\n")
}

df_nb_tax_one = df_sp %>% filter(sum_obs == 1) %>% select(!sum_obs) %>% arrange(taxon)

df_obs_last_year = df_vigieflore_maille %>%
  filter(session_year == max(df_vigieflore_maille$session_year)) %>%
  select(taxon, NOM_VERN, placette_id) %>%
  unique() %>%
  arrange(taxon)

# Espèces observées avec pondération pour le sunburst
df_obs_taxons = df_vigieflore_maille %>%
  group_by(maille_id, taxon_cd_nom, famille, genre, taxon) %>%
  summarise(nobs = n(), .groups = 'drop') %>%
  mutate(regne = "Plantae")

# Dataframe des habitats des placettes
bar_hab = df_vigieflore_maille %>%
  select(session_year, placette_id, corineType, color_ct) %>%
  unique() %>%
  # On ne garde que la dernière dénomination en date du transect
  group_by(placette_id) %>%
  filter(session_year == max(session_year)) %>%
  ungroup() %>%
  # On groupe par habitat pour savoir combien de transects sont comptés
  group_by(corineType, color_ct) %>%
  summarise(nb_hab = n(),
            placette_id_all = paste(placette_id, collapse = " ; "),
            .groups = 'drop') %>%
  arrange(nb_hab)

# Même dataframe adapté au treemap
bar_hab_tree <- bar_hab 
hab_absents = setdiff(df_habitats$corineType, unique(df_vigieflore_maille$corineType))
if (length(hab_absents)!=0) {
  colors_hab = (df_habitats %>% filter(corineType %in% hab_absents) %>%
                  select(corineType, color_ct) %>% unique())$color_ct
  bar_hab_tree = rbind(bar_hab_tree,
                       data.frame(corineType = hab_absents,
                                  color_ct = colors_hab,
                                  nb_hab = 0, placette_id_all = "")) %>%
    arrange(nb_hab)
}


## Sunburst des espèces 

df_sunburst <- data.frame()
all_levels <- c("regne", "famille", "genre", "taxon")

for (i in 2:length(all_levels)) {
  temp <- df_obs_taxons %>%
    group_by(across(all_levels[1:i])) %>%
    summarise(nobs = sum(nobs), .groups = "drop") %>%
    mutate(parent = .data[[all_levels[i - 1]]],
           label = .data[[all_levels[i]]]) %>%
    select(parent, label, nobs)
  
  df_sunburst <- bind_rows(df_sunburst, temp)
}

df_sunburst <- bind_rows(
  data.frame(parent = "", label = "Plantae", nobs = sum(df_obs_taxons$nobs)),
  df_sunburst
) %>%
  filter(!is.na(parent))


## Richesse

## - annuelle
df_richesse_maille = df_vigieflore_maille %>%
  select(session_year, taxon) %>%
  unique() %>%
  group_by(session_year) %>%
  summarise(Richesse = n()) %>%
  mutate(session_year = as.integer(session_year)) %>%
  rename(`Années` = session_year)


# Participation
# Data frame permettant de calculer les périodes de suivi des transects
# - En ligne
df_placette_tempo_line = df_vigieflore_maille %>%
  arrange(session_date) %>%
  select(maille_id, placette_id, session_year) %>%
  mutate(session_year = as.integer(session_year)) %>%
  unique() %>%
  group_by(placette_id) %>%
  mutate(index = cur_group_id(),
         index_y = index+cumsum(c(TRUE, diff(session_year) != 1)),
         groupe = paste0(placette_id, index_y))

# - En point
df_placette_tempo_point = df_placette_tempo_line %>%
  group_by(groupe) %>%
  filter((session_year == min(session_year) | session_year == max(session_year))) %>%
  ungroup()

# - Pour les noms
df_placette_tempo_name = df_placette_tempo_line %>%
  select(placette_id) %>%
  unique()


