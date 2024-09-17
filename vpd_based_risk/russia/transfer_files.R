source("https://raw.github.com/carlosdobler/spatial-routines/master/download_data.R")

reticulate::use_condaenv("risk")

rt_download_era_monthly("2m_dewpoint_temperature",
                        2000:2020,
                        1:12,
                        dest_dir = "gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature/")


library(tidyverse)

ff <- 
  "gsutil ls gs://clim_data_reg_useast1/era/monthly/mean-dewpoint-temp" %>% 
  system(inter = T) %>% 
  str_subset(".nc") %>% 
  str_subset(str_flatten(1995:1999, "|"))

for (f in ff) {
  
  "gsutil cp {f} /mnt/pers_disk" %>% 
    str_glue() %>% 
    system()
  
  f %>% 
    str_sub(-7,-4) -> yr
  
  for (m in 1:12) {
    
    "cdo seltimestep,{m} /mnt/pers_disk/{fs::path_file(f)} /mnt/pers_disk/era5_2m-dewpoint-temperature_mon_{yr}-{str_pad(m, 2, 'left', '0')}-01.nc" %>% 
      str_glue() %>% 
      system()
    
  }
  
  "/mnt/pers_disk/" %>% 
    fs::dir_ls() %>% 
    str_subset("-01.nc") %>% 
    walk(function(fff){
      
      "gsutil mv {fff} gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature/" %>% 
        str_glue() %>% 
        system()
      
    })
  
  str_glue("/mnt/pers_disk/{fs::path_file(f)}") %>% 
    fs::file_delete()
  
  
}


"gsutil cp gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature/era5_2m-dewpoint_temperature_mon_2011-12-01.nc /mnt/pers_disk" %>% 
  str_glue() %>% 
  system()

"/mnt/pers_disk/era5_2m-dewpoint_temperature_mon_2011-12-01.nc" %>% 
  read_ncdf() -> a

"/mnt/pers_disk/era5_mon_mean-dewpoint-temp_2011.nc" %>% 
  read_ncdf() -> b

b %>% 
  slice(time, 12)





ff <- 
  "gsutil ls gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature" %>% 
  system(inter = T) %>% 
  str_subset(".nc") %>% 
  fs::path_file()

for (f in ff[-1]) {
  
  "gsutil mv gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature/{f} gs://clim_data_reg_useast1/era5/monthly_means/2m_dewpoint_temperature/{str_replace_all(f, 'dewpoint_temperature', 'dewpoint-temperature')}" %>% 
    str_glue() %>% 
    system()
  
}






library(stars)

s <- read_mdim("/vsicurl/https://ftp.cpc.ncep.noaa.gov/NMME/realtime_anom/ENSMEAN/2024080800/CFSv2.prate.202408.ENSMEAN.fcst.nc")

