
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# remove everything from workspace
rm(list = ls())


library(glmtools)
library(tidyverse)
library(rLakeAnalyzer)
library(GLM3r)
library(lubridate)
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
library(ggplot2)
library(reshape2)

# declare output files
sim_folder= '.'
out_file <- file.path(sim_folder, "output","output.nc")

# read observed data
df_obs <- read.csv('../lerwq_mendota/trainingData//MendotaData_observedGLM_checked.csv')

# run GLM
GLM3r::run_glm('.')
sim_vars(out_file)

x_start = as.POSIXct('1995-01-01 00:00:00')
x_end = as.POSIXct('2004-12-31 23:00:00')

# TEMPERATURE
var_name = 'temp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'Temp. (deg C)'

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# OXYGEN
var_name = 'OXY_oxy'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'DO (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# GLM3r::run_glm('GLM-AED2/')
# NITORGEN
var_name = 'NIT_nit'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'NO3 (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# PHOSPHORUS
var_name = 'PHS_frp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'SRP (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# pH
var_name = 'CAR_pH'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'pH (log10 mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# NIT_amm
var_name = 'NIT_amm'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'Amm (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# Silica
var_name = 'SIL_rsi'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'Silica (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')


# PHYTO
var_name = 'PHY_tchla'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
surface_temp$var = surface_temp[,2]
y_label = 'Chl-a (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 5), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (5m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 13)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 13), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (13m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# DOC
# GLM3r::run_glm('GLM-AED2/')
var_name = 'OGM_doc'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'DOC (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (5m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# ZOOPLANKTON
# GLM3r::run_glm('GLM-AED2/')
var_name = 'ZOO_zoo01'
surface_temp_1 <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 0.5)
surface_temp_1$var = surface_temp_1[,2]
var_name = 'ZOO_zoo02'
surface_temp_2 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_2$var = surface_temp_2[,2]
var_name = 'ZOO_zoo03'
surface_temp_3 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_3$var = surface_temp_3[,2]
y_label = 'ZOO (mmol C/m3)'
# plot_df_obs = df_obs %>% select(datetime, depth, var_name)
# plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot()+
  geom_line(data = surface_temp_1, aes(DateTime, var, col = 'sim_1')) +
  geom_line(data = surface_temp_2, aes(DateTime, var, col = 'sim_2')) +
  geom_line(data = surface_temp_3, aes(DateTime, var, col = 'sim_3')) +
  geom_line() +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CAL') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CAL, col = 'obs_CAL')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CLA') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CLA, col = 'obs_CLA')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_COP') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_COP, col = 'obs_COP')) +
  ggtitle(paste0('Average',var_name)) +
  xlim(x_start, x_end) + ylim(0, 500)+
  xlab(label = '') + ylab(label = y_label) +
  theme_bw()

h <- h1; h
ggsave(filename = paste0('figures/',var_name,'.png'), plot = h, dpi = 300, width = 15, height = 9, units = 'in')

# PHYTOPLANKTON-ZOOPLANKTON
# GLM3r::run_glm('GLM-AED2/')
var_name = 'ZOO_zoo01'
surface_temp_1 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_1$var = surface_temp_1[,2]
var_name = 'ZOO_zoo02'
surface_temp_2 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_2$var = surface_temp_2[,2]
var_name = 'ZOO_zoo03'
surface_temp_3 <- get_var(file = out_file, 
                          var_name = var_name,
                          reference = 'surface',
                          z_out = 0.5)
surface_temp_3$var = surface_temp_3[,2]
var_name = 'PHY_tchla'
surface_temp_4 <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
surface_temp_4$var = surface_temp_4[,2]

plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

y_label = 'ZOO (mmol C/m3)'


h1 <- ggplot()+
  # geom_line(data = surface_temp_1, aes(DateTime, var, col = 'sim_1')) +
  geom_line(data = surface_temp_2, aes(DateTime, var, col = 'sim_2')) +
  # geom_line(data = surface_temp_3, aes(DateTime, var, col = 'sim_3')) +
  geom_line(data = surface_temp_4, aes(DateTime, var, col = 'sim_phy')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CAL') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CAL, col = 'obs_CAL')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_CLA') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_CLA, col = 'obs_CLA')) +
  geom_point(data = df_obs %>% select(datetime, depth, 'ZOO_COP') %>% filter(depth == 0), aes(as.POSIXct(datetime), ZOO_COP, col = 'obs_COP')) +
  geom_point(data = plot_df_obs %>% filter(depth == 5), aes(as.POSIXct(datetime), var, col = 'obs_phy')) +
  ggtitle(paste0('Average',var_name)) +
  xlim(x_start, x_end) + ylim(0, 500)+
  xlab(label = '') + ylab(label = y_label) +
  theme_bw()

h <- h1; h
ggsave(filename = paste0('figures/','phyto-zoop','.png'), plot = h, dpi = 300, width = 15, height = 9, units = 'in')
