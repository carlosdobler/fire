
library(tidyverse)
library(stars)
library(furrr)

options(future.fork.enable = T)
plan(multicore)

source("https://raw.github.com/carlosdobler/spatial-routines/master/cell_pos_and_import_subset.R")

fit <- read_rds("/mnt/pers_disk/data_russia/fit_logreg_inter.rds")

models <- c("GFDL-ESM4", 
            "MPI-ESM1-2-HR", 
            "MRI-ESM2-0", 
            "UKESM1-0-LL", 
            "IPSL-CM6A-LR")





biomass <- 
  "/mnt/pers_disk/data_russia/NASA-ORNL-biomass_carbon_density-v1-2010.tif" %>% 
  read_stars() %>% 
  setNames("biomass")


ss_ens <- 
  map(models, function(model){
    
    print(model)
    
    ff <- 
      "gsutil ls gs://clim_data_reg_useast1/cmip6/nex/monthly/{model}/vapor_pressure_deficit/" %>%
      str_glue() %>% 
      system(intern = T) %>% 
      str_subset(".nc") %>% 
      str_subset(str_flatten(2010:2050, "|"))
    
    ff %>% 
      future_walk(function(f) {
        
        "gsutil cp {f} /mnt/pers_disk/data_russia/" %>% 
          str_glue() %>% 
          system(ignore.stdout = T, ignore.stderr = T)
        
      })
    
    ff <- 
      ff %>% 
      fs::path_file() %>% 
      {str_glue("/mnt/pers_disk/data_russia/{.}")}
    
    s_proxy <- 
      ff[1] %>%  
      read_ncdf(proxy = T) %>% 
      suppressMessages()
    
    xmin <- fn_get_cell_pos(s_proxy, 1, 26)
    ymin <- fn_get_cell_pos(s_proxy, 2, 40)
    xmax <- fn_get_cell_pos(s_proxy, 1, -169+360)
    ymax <- fn_get_cell_pos(s_proxy, 2, 78)
    
    ss_vpd <- 
      ff %>% 
      future_map(function(f){
        
        read_ncdf(f, ncsub = cbind(start = c(xmin, ymin, 1),
                                   count = c(xmax-xmin+1,
                                             ymax-ymin+1,
                                             NA))) %>% 
          suppressMessages()
        
      }) %>% 
      do.call(c, .) %>% 
      units::drop_units()
    
    # ss_vpd_p <- 
    #   ss_vpd %>% 
    #   st_warp(crs = "+proj=laea +lon_0=99.84375 +lat_0=63.1259721 +datum=WGS84 +units=m +no_defs")
    
    
    
    
    biomass <-
      biomass %>% 
      st_warp(ss_vpd %>% slice(time, 1))
    
    ss_fire_prob <-
      map(seq(dim(ss_vpd)[3]), function(i) {
        
        ss_vpd %>% 
          slice(time, i) %>% 
          c(biomass) %>% 
          predict(fit, type = "prob") %>% 
          select(.pred_1)
        
      }) %>% 
      {do.call(c, c(., along = "time"))}
    
    st_dimensions(ss_fire_prob) <- st_dimensions(ss_vpd)
    
    
    
    ss_fire_danger_2010_2030 <- 
      ss_fire_prob %>%
      filter(year(time) >= 2010,
             year(time) <= 2030) %>% 
      st_apply(c(1,2), function(x) {
        
        if(all(is.na(x))){
          NA
        } else {
          
          mean(x >= 0.9)*100
          
        }
        
      },
      .fname = "perc_time",
      FUTURE = T)
    
    
    ss_fire_danger_2030_2050 <- 
      ss_fire_prob %>%
      filter(year(time) >= 2030,
             year(time) <= 2050) %>% 
      st_apply(c(1,2), function(x) {
        
        if(all(is.na(x))){
          NA
        } else {
          
          mean(x >= 0.9)*100
          
        }
        
      },
      .fname = "perc_time",
      FUTURE = T)
    
    
    c(ss_fire_danger_2010_2030,
      ss_fire_danger_2030_2050) %>% 
      setNames(c("p_2010_2030",
                 "p_2030_2050"))
    
  })



ss_ens_f <- 
  ss_ens %>% 
  # map(split, "period") %>% 
  map(mutate, delta = p_2030_2050 - p_2010_2030) %>% 
  map(merge, name = "periods") %>% 
  {do.call(c, c(., along = "models"))} %>% 
  st_apply(c(1,2,3), function(x) {
    
    if(any(is.na(x))) {
      NA
    } else {
      
      mean(x)
      
    }
    
  })



ss_ens_f %>% 
  setNames("perc") %>% 
  slice(periods, 1:2) %>% 
  as_tibble() %>% 
  ggplot(aes(lon, lat, fill = perc)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("plasma",
                                               rev = F, 
                                               limits = c(0,50),
                                               oob = scales::squish,
                                               na.value = "transparent",
                                               name = "perc. (%)\n",
                                               trans = "sqrt"
                                               ) +
  facet_wrap(~periods, ncol = 1) +
  theme(axis.title = element_blank())
  

ss_ens_f %>% 
  setNames("perc") %>% 
  slice(periods, 3) %>% 
  as_tibble() %>% 
  mutate(perc = if_else(perc < 0, 0, perc)) %>%
  ggplot(aes(lon, lat, fill = perc)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("rocket",
                                               rev = T, 
                                               na.value = "transparent",
                                               name = "perc.\npoints\n",
  ) +
  theme(axis.title = element_blank())


write_stars(ss_ens_f, "/mnt/bucket_mine/results/misc/russia_fire_prob.tif")

















