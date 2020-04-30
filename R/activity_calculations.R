########
### Activity calculations using Steen and German protocols
########

###### Data being used
### Calculating the activity of:
# single replicate
# 200 uM L-Leucine AMC
# 120 minutes
######

######## GERMAN PROTOCOL
### Quench coefficient, slope of std in homogenate / slope of std in buffer
### Units should not matter here, as long as they are the same for both slopes
homo_slope <- 404784.3 # fsu/uM, slope of std curve in homogenate
buffer_slope <- 235096.9 # fsu/uM, slope of std curve in buffer
quench_coef <- homo_slope / buffer_slope

### Emission coefficient, slope of std in homogenate / standard volume (1 mL)
homo_slope_convert <- homo_slope / 1000 # convert fsu/(umol/L) to fsu/(umol/mL)
emission_coef <- homo_slope_convert / 1 # 1 ml std volume

### Net fsu, (Assay - homogenate control) / quench coefficient - substrate control
assay <- 140409.87 # fsu at timepoint 120 minutes
homo_control <- 1384.83 # fsu of pure homogenate
sub_control <- 58215.49 # fsu of substrate in buffer at timepoint 120 minutes
net_fsu <- (assay - homo_control) / quench_coef - sub_control

### Activity, (net fsu * buffer volume) / (emission coefficient * homogenate volume * time)
buffer_vol <- 0.14 # mL, volume of buffer in assay
homo_vol <- 0.86 # mL, volume of homogenate in assay
activity <- (net_fsu * buffer_vol) /  ### units in umol/min
  (emission_coef * homo_vol * 120)

print(activity)

######## STEEN PROTOCOL
### Concentration of AMC at 120 minutes
### (Assay - kill control - std intercept) / std slope
assay <- assay # same fsu value as German protocol
kill_control <- 80429.44 # fsu at timepoint 120 min
std_intercept <- -12363.58 # fsu, intercept of std curve in buffer-homogenate soln
std_slope <- 268709.4 # fsu/uM, slope of std curve in buffer-homogenate soln

### std_intercept and std_slope /1000 to convert to fsu/(umol/mL) and then / 1 for 1 mL volume
std_intercept_convert <- std_intercept / 1000 / 1 # fsu
std_slope_convert <- std_slope / 1000 / 1 #fsu/umol
conc_120 <- (assay - kill_control - std_intercept_convert) / # units in umol
  std_slope_convert

### Concentration of AMC at 0 minutes
assay <- 47290.40 # fsu at timepoint 0
kill_control <- 24215.23 # fsu at timepoint 0
std_intercept_convert <- std_intercept_convert # same
std_slope_convert <- std_slope_convert # same

conc_0 <- (assay - kill_control - std_intercept_convert) / # units in umol
  std_slope_convert

###Activity
activity <- (conc_120 - conc_0) / (120 - 0) # units umol/min


print(activity)
