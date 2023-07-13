#require(remotes)
#remotes::install_github("james-thorson/FishLife",force=T)
#vignette("tutorial","FishLife")


require(FishLife); require(openxlsx); require(this.path); require(data.table); require(dplyr); require(stringr)
root_dir <- this.path::here(..=1)
dir.create(paste0(root_dir,"/Outputs"),recursive=T,showWarnings=F)

  Predict =  Plot_taxa( Search_species(Genus="Pristipomoides",Species="filamentosus")$match_taxonomy )
  
  Predict[[1]]$Mean_pred[13]       # Steepness
  exp(Predict[[1]]$Mean_pred[12])  # Sigma R


 
  