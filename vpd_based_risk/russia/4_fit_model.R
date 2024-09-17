
# Fit and validate fire probability model


library(tidyverse)
library(tidymodels)
library(sf)
library(spatialsample)


tb_f_model <- 
  "/mnt/pers_disk/data_russia/tb_f_model.csv" %>% 
  read_csv()

tb_f_model <- 
  tb_f_model %>% 
  uncount(w)


set.seed(111)
tb_f_model_sub <- 
  tb_f_model %>% 
  slice_sample(prop = 0.05) %>% 
  mutate(fire = factor(fire),
         # w = frequency_weights(w) # no need: using uncount() now
         )
  
# tb_sp <- 
#   tb_f_model_sub %>% 
#   st_as_sf(coords = c("x", "y"),
#            crs = 4326)
# 
# set.seed(111)
# folds <- 
#   spatial_block_cv(tb_sp, v = 10)


logreg_spec <- 
  logistic_reg()

# r <- 
#   logreg_spec %>% 
#   fit_resamples(fire ~ vpd + biomass, folds, 
#                 control = control_resamples(save_pred = T))
# 
# collect_metrics(r, summarize = T)
# 
# collect_predictions(r) %>% 
#   conf_mat(truth = fire, estimate = .pred_class)





fit <- 
  logreg_spec %>% 
  fit(fire ~ vpd * biomass, data = tb_f_model_sub)

write_rds(butcher::butcher(fit), 
          "/mnt/pers_disk/data_russia/fit_logreg_inter.rds")


tb_pred <- 
  expand_grid(
    vpd = seq(min(tb_f_model_sub$vpd),
              max(tb_f_model_sub$vpd),
              length.out = 50),
    
    biomass = seq(min(tb_f_model_sub$biomass),
                  max(tb_f_model_sub$biomass),
                  length.out = 50),
  ) %>% 
  
  {augment(fit, .)}
  

ggplot(tb_pred %>% filter(vpd < 2.5), 
       aes(vpd, biomass)) +
  geom_raster(aes(fill = .pred_1)) +
  colorspace::scale_fill_continuous_sequential("inferno", rev = T, end = 0.85) +
  geom_contour(aes(z = .pred_1), breaks = c(0.5, 0.75, 0.9, 0.95), color = "white", linetype = "3333")
  


