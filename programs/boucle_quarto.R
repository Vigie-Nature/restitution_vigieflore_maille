
library(stringr)

df_vf = read.csv2(file = "data/export_vigie_flore.csv", sep = ",")
lst_maille = unique(df_vf$maille_id)

i = 1
for (name_maille in lst_maille) {
  name_maille_file = str_replace_all(name_maille, " ", "_")
  name_maille_file = str_replace_all(name_maille, "-", "_")
  filename = paste0("Restitution_maille_", name_maille_file, ".html")
  
  
  quarto::quarto_render(input = "Restitution_maille.qmd",
                        execute_params = list("maille_name" = name_maille),
                        output_file = filename)
  
  file.rename(filename, file.path("out", filename))
  print(paste0(i, "/", length(lst_maille)))
  i = i+1
}
