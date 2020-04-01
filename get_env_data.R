##################################
# Get environmental data
#
##################################

# Iceland ####
# No logged data but: 
# "cold sampled" had moss surface temp 12C (top 2-4 cm) 
# "warm sampled" moss surface had 27C (top 2-4 cm) 

# Frasne, France ####
# Data stored in excel file
# Package readxl to load data
library(readxl)
dat <- read_excel("clim_Frasne_2019.xlsx", skip = 2)
head(dat)

# Fix column names
colnames(dat) <- c("time", "temp_2mAir_C", "temp_7cmPeat_C", "RH", "rad_Wm-2","par", "wt_s.mag", "wt_wet_part", 
                   "pressure", "precip", "wind_m_s-1", "wind_direction", "wind_std")

# Make time variable
dat$timeD <- as.POSIXct(dat$time)

# Define and save day and night time (eg day time = 6am to 6pm)
dat$day_night <- ifelse(as.numeric(format(dat$timeD,"%H"))>5 & as.numeric(format(dat$timeD,"%H"))< 19,
                        "day", "night")

# Mean temp and WT the last 2 weeks before sampling
with(dat[dat$timeD > "2019-06-06 00:00:00" & dat$timeD < "2019-06-20 00:00:00",], 
     colMeans(cbind(temp_2mAir_C, temp_7cmPeat_C,wt_s.mag, wt_wet_part)))

# Mean day/night temp and WT the last 2 weeks before sampling (6am to 6pm)
day.night <- with(dat[dat$timeD > "2019-06-06 00:00:00" & dat$timeD < "2019-06-20 00:00:00",],
                      aggregate(cbind(temp_2mAir_C, temp_7cmPeat_C,wt_s.mag, wt_wet_part)~factor(day_night), FUN=mean))

# Mean temp and WT the last 2 days before sampling (the day before and the day of sampling)
with(dat[dat$timeD > "2019-06-06 00:00:00" & dat$timeD < "2019-06-20 00:00:00",], 
     colMeans(cbind(temp_2mAir_C, temp_7cmPeat_C,wt_s.mag, wt_wet_part)))

# Mean day/night temp and WT the last 2 days before sampling (6am to 6pm). Day time the day before sampling and the sampling day
# and night time two days before sampling day
day.night.2day <- with(dat[dat$timeD > "2019-06-18 18:00:00" & dat$timeD < "2019-06-20 19:00:00",],
                  aggregate(cbind(temp_2mAir_C, temp_7cmPeat_C,wt_s.mag, wt_wet_part)~factor(day_night), FUN=mean))
