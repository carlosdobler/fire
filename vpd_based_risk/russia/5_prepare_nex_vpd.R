

# Aggregate NEX's daily VPD to monthly


library(tidyverse)
library(stars)
library(furrr)

options(future.fork.enable = T)
plan(multicore)


models <- c("GFDL-ESM4", 
            "MPI-ESM1-2-HR", 
            "MRI-ESM2-0", 
            "UKESM1-0-LL", 
            "IPSL-CM6A-LR")


for (model in models[-(1:2)]) {                                                     # ***********
  
  # model = models[1]
  
  print(str_glue("PROCESSING MODEL {model}"))
  
  # "/mnt/bucket_mine/cmip6/nex/monthly/{model}/vapor_pressure_deficit" %>% 
  #   str_glue() %>% 
  #   fs::dir_create()
  
  ff <- 
    "gsutil ls gs://clim_data_reg_useast1/cmip6/nex/daily/{model}/vapor_pressure_deficit/" %>%
    str_glue() %>% 
    system(intern = T) %>% 
    str_subset(".nc")
  
  if (length(ff) != 140) print(str_glue("   Not 140!"))
  
  chunks_id <- cut(seq(ff), breaks = 10, labels = F)
  
  for (i in unique(chunks_id)) {
    
    print(str_glue("   chunk {i} / 10"))
    
    ff_chunk <- ff[chunks_id == i] 
    
    future_walk(ff_chunk, function(f) {
      
      str_glue("gsutil cp {f} /mnt/pers_disk") %>% 
        system(ignore.stdout = T, ignore.stderr = T)
      
      f_local <- 
        str_glue("/mnt/pers_disk/{fs::path_file(f)}")
      
      yr <- str_sub(f_local, -7, -4)
      
      f_seas <- 
        str_glue("/mnt/pers_disk/vpd_mon_nex_{model}_{yr}.nc")
      
      str_glue("cdo monmean {f_local} {f_seas}") %>% 
        system(ignore.stdout = T, ignore.stderr = T)
      
      "gsutil mv {f_seas} gs://clim_data_reg_useast1/cmip6/nex/monthly/{model}/vapor_pressure_deficit/" %>% 
        str_glue() %>% 
        system(ignore.stdout = T, ignore.stderr = T)
      
      fs::file_delete(f_local)
      
    })
  }

}





