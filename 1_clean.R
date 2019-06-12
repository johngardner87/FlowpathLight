

library(ggplot2)
library(lattice)
library(reshape2)
library(reshape)
library(lubridate)
library(plotly)
library(magrittr)
library(solaR)
library(StreamMetabolism)
library(dplyr)
library(ggpubr)
library(GGally)
library(tidyverse)
library(ggvis)
library(scales)
library(ggthemes)
library(GeoLight)
library(hms)
library(ggpubr)
library(broom)
library(TSA)
library(tibbletime)
library(broom)
library(padr)
library(roll)
library(viridis)
library(fitdistrplus)
library(data.table)

#######################################################
### load in drifter data

hsm.all<-read.csv("data/UMR_drifter_raw.csv")
hsm.all$dt<-as.POSIXct(hsm.all$dt, format="%m/%d/%Y %H:%M" , tz="America/Chicago")

sunrise.m <-sunrise.set(lat=43.734750, lon=-91.231540, date="2016/06/01", timezone="America/Chicago", num.days=125) %>%
  as_tibble() %>%
  mutate(date= as.Date(strftime(sunrise, format="%Y-%m-%d",timezone="America/Chicago")))

# clean up and add flags to drifter data
hsm.all <- hsm.all %>%
  mutate(az = ifelse(az < 0, NA, az)) %>%
  #mutate(az = ifelse(az > 17000, 17000, az))
  mutate(deg_from_vert = abs(90 - (az * 90 / 17000))) %>%
  mutate(deg_from_vert_est = ifelse(is.na(deg_from_vert), 8.68, deg_from_vert)) %>%
  mutate(lux_tilt = lux/cos(deg_from_vert_est * (pi/180))) %>%
  mutate(tilt_flag = ifelse(deg_from_vert_est > 45, "flag", NA)) %>%
  mutate(lux_tilt_rm = ifelse(is.na(tilt_flag), lux_tilt, NA)) %>%
  filter(dt < as.POSIXct("2016-07-14 18:00", tz="America/Chicago" )) %>%
  mutate(error_flag = NA) %>%
  mutate(error_flag = ifelse(date=="6/8/2016", "logging", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/15/2016", "surfaced", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/21/2016" & name=="3A", "unknown", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/21/2016" & name=="3B", "backwater", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/23/2016" & name=="3A" & dt > 
                               as.POSIXct("2016-06-23 12:10", tz="America/Chicago") & dt < 
                               as.POSIXct("2016-06-23 14:50",  tz="America/Chicago"), "surfaced", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/28/2016" & name=="3A" & dt > 
                               as.POSIXct("2016-06-28 14:20", tz="America/Chicago"), "stuck", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/28/2016" & name=="4B", "stuck", error_flag)) %>%
  mutate(error_flag = ifelse(date=="6/29/2016" & name=="3A" & dt > 
                               as.POSIXct("2016-06-29 12:00",  tz="America/Chicago"), "stuck", error_flag)) %>%
  mutate(error_flag = ifelse(date=="7/14/2016" & name=="4B" & dt > 
                               as.POSIXct("2016-07-14 13:00", tz="America/Chicago"), "stuck", error_flag)) %>%
  group_by(doy.name) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins"))))) %>%
  slice(1:(n()-4)) %>% ## remove bottom 4 rows
  slice(-1:-2) %>%     ## remove top 2 rows of each deployment
  mutate(solar.dt = local2Solar(dt, lon=-91.231540)) %>%
  mutate(zenith = GeoLight::zenith(solar(dt), lat=43.734750, lon=-91.231540)) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  mutate(solar.time = as.hms(strftime(solar.dt, format="%H:%M:%S"))) %>%
  left_join(sunrise.m, by="date")


ggplot() +
  geom_line(data=hsm.all, aes(x=dt, y=(lux_tilt_rm), color=name)) +
  facet_wrap(~date, scales="free")

## remove 7/14/16 for kd cause no depth data
## error flag the first 2 measurements and last 3-4
##assume tilt is 8.68 where no data. 
## trim data when bad, deploying/retrieving, flag non lagrangian.

hsn.all<-read.csv("data/NR_drifter_raw.csv")
hsn.all$dt<-as.POSIXct(hsn.all$dt, format="%m/%d/%Y %H:%M:%S", tz="EST")

sunrise.n <-sunrise.set(lat=35.140132, long=-77.059431, date="2014/01/01", timezone="EST", num.days=1000) %>%
  as_tibble() %>%
  mutate(date= as.Date(strftime(sunrise, format="%Y-%m-%d", timezone="EST")))

hsn.all <- hsn.all %>%
  mutate(az = ifelse(az < 0, NA, az)) %>%
  #mutate(az = ifelse(az > 17000, 17000, az))
  mutate(deg_from_vert = abs(90 - (az * 90 / 17000))) %>%
  mutate(deg_from_vert_est = ifelse(is.na(deg_from_vert), 8.68, deg_from_vert)) %>%
  mutate(lux_tilt = lux/cos(deg_from_vert_est * (pi/180))) %>%
  mutate(tilt_flag = ifelse(deg_from_vert_est > 45, "flag", NA)) %>%
  mutate(lux_tilt_rm = ifelse(is.na(tilt_flag), lux_tilt, NA)) %>%
  filter(!date %in% c("3/11/2015","3/12/2015","3/13/2015")) %>%
  mutate(error_flag = NA) %>%
  mutate(error_flag = ifelse(doy.name=="3/10/2015.3B" & dt > 
                               as.POSIXct("2015-03-10 14:23:00",tz="EST"),"stuck", error_flag)) %>%
  mutate(error_flag = ifelse(date == "10/12/2015" & name =="4B", "stuck", error_flag)) %>%
  mutate(error_flag = ifelse(start.dt == "6/16/2015" & name =="3A", "stuck", error_flag)) %>%
  mutate(solar.dt = local2Solar(dt, lon=-77.166742)) %>%
  mutate(zenith = GeoLight::zenith(solar(dt), lat=35.238282, lon=-77.166742)) %>%
  mutate(date = as.Date(date, format="%m/%d/%Y")) %>%
  mutate(solar.time = as.hms(strftime(solar.dt, format="%H:%M:%S"))) %>%
  mutate(time = as.hms(strftime(dt, format="%H:%M:%S", tz="EST"))) %>%
  left_join(sunrise.n, by="date")

#write.csv(hsm.all %>% filter(type=="sub"), "data/UMR_drifter_clean.csv")
#write.csv(hsn.all %>% filter (type=="sub"), "data/NR_drifter_clean.csv")

#####################################################################
# Calculate KD along river from drifter data

#UMR
lm_roll <- rollify(~lm(.x ~ .y, na.action = na.omit), window = 15, unlist = FALSE)
rolling_mean <- rollify(~mean(.x, na.rm = TRUE), window = 15)
roll_time <- rollify(~max(.x, na.rm = TRUE), window = 15)

data_miss <- hsm.all %>%
  filter(type == "sub", !doy.name %in% c("167.3B" ,"196.4B", "173.3A", "173.3B",
                                         "180.4B")) %>%
  filter(is.na(error_flag)) %>%
  mutate(lux_kd = ifelse(lux_tilt_rm < 13, NA, lux_tilt_rm)) %>%
  mutate(depth_kd = ifelse(depth.top < 0.1, NA, depth.top)) %>%
  filter(!is.na(lux_kd) & !is.na(depth_kd)) %>%
  select(doy.name, name, date, dt, lux_kd, depth_kd) %>%
  group_by(doy.name) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins")))))  %>%
  ungroup()

kd_miss_hs <- data_miss  %>%
  dplyr::group_by(doy.name) %>%
  dplyr::mutate(roll_depth = rolling_mean(depth_kd)) %>%
  dplyr::mutate(max_time = roll_time(diff.time)) %>%
  dplyr::mutate(rolling_kd = lm_roll(log(lux_kd), depth_kd)) %>%
  filter(!is.na(rolling_kd)) %>%
  mutate(tidied = purrr::map(rolling_kd, broom::tidy),
         glanced = purrr::map(rolling_kd, broom::glance)) %>%
  unnest(glanced) %>%
  unnest(tidied) %>%
  select(doy.name,date, name, dt, roll_depth, max_time, r.squared, p.value, term, estimate, std.error) %>%
  mutate(term = ifelse(term == ".y", "slope", "yint")) %>%
  gather(variable, value, -(doy.name:term)) %>%
  unite(temp, term, variable) %>%
  spread(temp, value) %>%
  ungroup() %>%
  select(doy.name, dt, roll_depth:yint_std.error)

hsm.all_join <- hsm.all %>%
  ungroup() %>%
  left_join(kd_miss_hs, by=c("doy.name", "dt")) %>%  
  mutate(slope_estimate_plot= ifelse(slope_estimate >= 0, NA, slope_estimate),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) %>%
  mutate(slope_estimate_plot = slope_estimate_plot * -1) %>%
  mutate(slope_estimate_plot= ifelse(slope_estimate_plot < 0.5, NA, slope_estimate_plot),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) %>%
  mutate(slope_estimate_plot= ifelse(p.value > 0.1, NA, slope_estimate_plot),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) 


#NR
lm_roll2 <- rollify(~lm(.x ~ .y, na.action = na.exclude), window =60, unlist = FALSE)
rolling_mean2 <- rollify(~mean(.x, na.rm = TRUE), window = 60)
roll_time2 <- rollify(~max(.x, na.rm = TRUE), window = 60)

data_neu <- hsn.all %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A",
                         "3/10/2015.3B"),
         date != "2015-10-13") %>%
  filter(is.na(error_flag)) %>%
  mutate(lux_kd = ifelse(lux_tilt_rm < 13, NA, lux_tilt_rm)) %>%
  mutate(depth_kd = ifelse(depth.top < 0.1, NA, depth.top)) %>%
  filter(!is.na(lux_kd) & !is.na(depth_kd)) %>%
  select(doy.name, name, date, dt, lux_kd, depth_kd) %>%
  group_by(date) %>%
  mutate(diff.time= (c(0, difftime(tail(dt, -1), head(dt, -1), units=c("mins")))))  %>%
  ungroup()

kd_neu_hs <- data_neu  %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(roll_depth = rolling_mean2(depth_kd)) %>%
  dplyr::mutate(max_time = roll_time2(diff.time)) %>%
  dplyr::mutate(rolling_kd = lm_roll2(log(lux_kd), depth_kd)) %>%
  filter(!is.na(rolling_kd)) %>%
  mutate(tidied = purrr::map(rolling_kd, broom::tidy),
         glanced = purrr::map(rolling_kd, broom::glance)) %>%
  unnest(glanced) %>%
  unnest(tidied) %>%
  select(doy.name,date, name, dt, roll_depth, max_time, r.squared, p.value, term, estimate, std.error) %>%
  mutate(term = ifelse(term == ".y", "slope", "yint")) %>%
  gather(variable, value, -(doy.name:term)) %>%
  unite(temp, term, variable) %>%
  spread(temp, value) %>%
  ungroup() %>%
  select(date, doy.name, dt, roll_depth:yint_std.error)

hsn.all_join <- hsn.all %>%
  left_join(kd_neu_hs, by=c("date", "dt")) %>%
  mutate(slope_estimate_plot= ifelse(slope_estimate >= 0, NA, slope_estimate),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) %>%
  mutate(slope_estimate_plot = slope_estimate_plot * -1) %>%
  mutate(slope_estimate_plot= ifelse(slope_estimate_plot < 0.5, NA, slope_estimate_plot),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) %>%
  mutate(slope_estimate_plot= ifelse(p.value > 0.1, NA, slope_estimate_plot),
         error_plot= ifelse(is.na(slope_estimate_plot), NA, slope_std.error)) %>%
  rename(doy.name = doy.name.x)

##################
# calculate drifter Kd for whole deployment

data_miss_kd <- data_miss %>%
  ungroup() %>%
  filter(depth_kd <5) %>%
  nest(-doy.name) %>%
  mutate(
    fit = purrr::map(data, ~lm(log(.$lux_kd) ~ .$depth_kd, data = .)),
    tidied = purrr::map(fit, tidy),
    glanced = purrr::map(fit, glance)) %>%
  unnest(glanced) %>%
  unnest(tidied) %>%
  dplyr::select(-statistic1, -p.value1) %>%
  mutate(term = ifelse(term == ".$depth_kd", "slope", "yint")) %>%
  gather(variable, value, -(doy.name:term)) %>%
  unite(temp, term, variable) %>%
  spread(temp, value) 

data_neu_kd <- data_neu %>%
  ungroup() %>%
  filter(depth_kd > 0.25, lux_kd >20, lux_kd < 2950) %>%
  nest(-date) %>%
  mutate(
    fit = purrr::map(data, ~lm(log(.$lux_kd) ~ .$depth_kd, data = .)),
    tidied = purrr::map(fit, tidy),
    glanced = purrr::map(fit, glance)) %>%
  unnest(glanced) %>%
  unnest(tidied) %>%
  dplyr::select(-statistic1, -p.value1) %>%
  mutate(term = ifelse(term == ".$depth_kd", "slope", "yint")) %>%
  gather(variable, value, -(date:term)) %>%
  unite(temp, term, variable) %>%
  spread(temp, value) 

#################################################
# join to data, then to full data, goruped_join by dt, doy.name

#NR
hsn_stats_data <- hsn.all_join %>% 
  filter(!is.na(slope_estimate)) %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A")) %>%
  filter(date != as.Date("2015-10-13")) %>%
  filter(p.value < 0.1, r.squared > 0.4) 

hsn_fits1 <- hsn_stats_data %>%
  group_by(date) %>%
  do(tidy(lm((slope_estimate_plot) ~ roll_depth, data=.))) %>%
  filter(term == "roll_depth")

hsn_fits2 <- hsn_stats_data %>%
  group_by(date) %>%
  do(glance(lm((slope_estimate_plot) ~ roll_depth, data=.))) %>%
  left_join(hsn_fits1, by="date")

#UMR
hsm_stats_data <- hsm.all_join %>% 
  filter(type == "sub", !doy.name %in% c("167.3B" ,"196.4B", "173.3A", "173.3B",
                                         "180.4B"), date != as.Date("2016-06-08")) %>%
  filter(slope_estimate_plot < 7) %>%
  filter(p.value < 0.1, r.squared > 0.4) 

hsm_fits1 <- hsm_stats_data %>%
  group_by(doy.name) %>%
  filter(doy.name != "181.3A") %>%
  do(tidy(lm((slope_estimate_plot) ~ roll_depth, data=.))) %>%
  filter(term == "roll_depth")

hsm_fits2 <- hsm_stats_data %>%
  group_by(doy.name) %>%
  filter(doy.name != "181.3A") %>%
  do(glance(lm((slope_estimate_plot) ~ roll_depth, data=.))) %>%
  left_join(hsm_fits1, by="doy.name")

#####################################
# Test CCf of depth time series of hydrospheres in miss river

maxlag <- function(x) {
  cor <- x$acf[,,1]
  lag <- x$lag[,,1]
  res <-  data.frame(cor,lag)
  res_max <- res %>%
    arrange(desc(cor)) %>%
    slice(1:10)
  
  return(res_max)
}

hs_miss_depth_stat <- hsm.all %>%
  ungroup() %>%
  filter(type == "sub", doy.name %in% c("168.4B" ,"175.4B", "175.3A", "180.3B","180.3A",
                                        "181.3A","181.3B","187.3A","187.3B","193.3A","193.3B","196.3A","196.3B" )) %>%
  filter(!date %in% as.Date(c("2016-06-16", "2016-06-29"))) %>%
  mutate(depth.top = ifelse(is.na(error_flag), depth.top, NA)) %>%
  select(dt, date, name, depth.top) %>%
  mutate(name2 = ifelse(name=="4B" |name=="3B", "h2", "h1")) %>%
  select(-name) %>%
  #group_by(date) %>1
  spread(name2, depth.top) %>%
  group_by(date) %>%
  mutate(h1_est =  na.approx(h1, x=dt, na.rm=F, maxgap=4 )) %>%
  mutate(h2_est =  na.approx(h2, x=dt, na.rm=F, maxgap=4 ))

test<- hs_miss_depth_stat %>% filter(date==as.Date("2016-06-23"))
test_ccf <- ccf(test$h1_est, test$h2_est, na.action = na.pass, lag.max=80)
maxlag(test_ccf)

#################################################################  
# Caculate cumulative light along flowpaths

#UMR
hs_miss_cum <- hsm.all_join %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  filter(type == "sub", doy.name %in% c("168.4B" ,"175.4B", "180.3B", "180.3A", "180.4B",
                                        "181.3A","181.3B","187.3A","187.3B","193.3A","193.3B","196.3A","196.3B" )) %>%
  group_by(doy.name) %>%
  mutate(total_distance = max(distance, na.rm=T)) %>%
  do(pad(., by="dt")) %>%   #
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5 )) %>%   #
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins")))))  %>%
  mutate(start_time = first(dt), end_time = last(dt)) %>%
  mutate(total_time = (end_time - start_time) ) %>%
  filter(is.na(error_flag)) %>%   #
  mutate(lux_sum_min = cumsum(replace_na(lux_tilt_rm, 0))) %>%
  mutate(lux_sum = lux_sum_min * diff.time) %>%
  mutate(lux_percent = 1-cume_dist(lux_tilt_rm)) %>%
  #  mutate(total_time_lag = (n() *2)/60) %>%   #
  mutate(total_time_lag = sum(diff.time/60, na.rm=T)) %>%
  mutate(total_lux = max(lux_sum, na.rm=T)) 

#NR
hs_neu_cum <- hsn.all_join %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A",
                         "3/10/2015.3B")) %>%
  group_by(date) %>%
  filter(time > as.hms("6:00:00") & time < as.hms("18:00:00")) %>%
  mutate(start_dist = first(distance, default=0), end_dist=last(distance)) %>%
  mutate(start_dist = ifelse(is.na(start_dist), 0, start_dist)) %>%
  mutate(total_dist = end_dist - start_dist) %>%
  mutate(total_distance = max(distance)) %>%
  mutate(total_dist = ifelse(is.na(total_dist), total_distance, total_dist)) %>%
  do(pad(., by="dt")) %>%   #
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5 )) %>%   #
  mutate(start_time = first(dt), end_time = last(dt)) %>%
  mutate(total_time = (end_time - start_time)/ (60*60) ) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins")))))  %>%
  filter(is.na(error_flag)) %>%  
  mutate(lux_sum_min = cumsum(replace_na(lux_tilt_rm, 0))) %>%
  mutate(lux_sum = lux_sum_min * diff.time) %>%
  mutate(lux_percent = 1-cume_dist(lux_tilt_rm)) %>%
  #mutate(total_time_lag = (n() * )/60) %>%   #
  mutate(total_time_lag = sum(diff.time/60, na.rm=T)) %>%
  mutate(total_lux = max(lux_sum, na.rm=T))


#####################################
# calculuate summary stats, mean, median, variance, RB of LUX_tilt_rm,

cv <- function(x, na.rm=T) {
  cv <- sd(x, na.rm=T)/mean(x, na.rm=T)
  return(cv)
}

#UMR
hs_miss_sum <- hsm.all_join %>%
  ungroup() %>%
  filter(type == "sub", doy.name %in% c("168.4B" ,"175.4B", "175.3A", "180.3B","180.3A",
                                        "180.4B", "181.3A","181.3B","187.3A","187.3B","193.3A","193.3B","196.3A","196.3B" )) %>%
  dplyr::select(error_flag, dt,doy.name, date, time, name, type, channel, temp, DO.p, DO.mg, DO.sat, EC, velocity,
                depth, depth.top, deg_from_vert_est, lux_tilt, lux_tilt_rm, zenith, 
                slope_estimate_plot, error_plot, yint_estimate, yint_std.error, p.value, r.squared) %>%
  mutate_at(.vars=c("temp", "DO.p", "DO.mg", "DO.sat", "EC", "velocity", "depth", "depth.top", 
                    "deg_from_vert_est", "lux_tilt", "lux_tilt_rm", "zenith"), funs(ifelse(is.na(error_flag), . , NA) )) %>%
  mutate(kd_flag = ifelse(p.value < 0.1 & r.squared >0.4, NA , 1)) %>%
  mutate_at(.vars=c("slope_estimate_plot", "error_plot"), funs(ifelse(is.na(kd_flag), . , NA) )) %>%
  group_by(doy.name) %>%
  summarise_at( vars(temp:yint_std.error), funs(mean=mean, med=median, sd=sd, var=var, cv=cv, rb=RBIcalc), na.rm=TRUE)

#NR
hs_neu_sum <- hsn.all_join %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A",
                         "3/10/2015.3B")) %>%
  dplyr::select(error_flag,dt,doy.name, date,time, name, type,solar.time, temp, DO.p, DO.mg, DO.sat, EC, velocity,
                depth, depth.top, deg_from_vert_est, lux_tilt, lux_tilt_rm, zenith, 
                slope_estimate_plot, error_plot, yint_estimate, yint_std.error, p.value, r.squared) %>%
  mutate_at(.vars=c("temp", "DO.p", "DO.mg", "DO.sat", "EC", "velocity", "depth", "depth.top", 
                    "deg_from_vert_est", "lux_tilt", "lux_tilt_rm", "zenith"), funs(ifelse(is.na(error_flag), . , NA) )) %>%
  mutate(kd_flag = ifelse(p.value < 0.1 & r.squared >0.4, NA , 1)) %>%
  mutate_at(.vars=c("slope_estimate_plot", "error_plot"), funs(ifelse(is.na(kd_flag), . , NA) )) %>%
  group_by(date) %>%
  dplyr::filter((time) > hms::as.hms("6:00:00") & (time) < hms::as.hms("18:00:00")) %>%
  summarise_at( vars(temp:yint_std.error), funs(mean=mean, med=median, sd=sd, var=var, cv=cv, rb=RBIcalc), na.rm=TRUE)


#UMR join
hs_miss_sum_join <- hs_miss_cum %>%
  dplyr::select(doy.name, date, doy, name, channel, sunrise, sunset, start_time,
                end_time, total_time,total_time_lag, total_distance, total_lux) %>%
  group_by(doy.name) %>%
  distinct(doy.name, .keep_all = T) %>%
  full_join(hs_miss_sum, by="doy.name")

#NR join
hs_neu_sum_join <- hs_neu_cum %>%
  dplyr::select(doy.name, date, doy, name, sunrise, sunset, start_time,
                end_time, total_time, total_time_lag, total_distance, total_dist, total_lux) %>%
  group_by(date) %>%
  distinct(date, .keep_all = T) %>%
  left_join(hs_neu_sum, by="date")

#######################################################
### Sunfleck analysis for UMR
sunfleck_hsm <- hsm.all %>%
  filter(type == "sub", doy.name %in% c("168.4B" ,"175.4B", "175.3A", "180.3B","180.3A",
                                        "180.4B", "181.3A","181.3B","187.3A","187.3B","193.3A","193.3B","196.3A","196.3B" )) %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  group_by(doy.name) %>%
  do(pad(., by="dt")) %>%
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5)) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins"))))) %>%
  mutate(sunfleck = ifelse(lux_tilt_rm >= 500, 1, NA)) %>%
  mutate(sunfleck = ifelse(is.na(lux_tilt_rm), NA, sunfleck)) %>%
  mutate(darkfleck = ifelse(lux_tilt_rm < 500, 1, NA)) %>%
  mutate(darkfleck = ifelse(is.na(lux_tilt_rm), NA, darkfleck)) %>%
  mutate(ID = rleid(!is.na(sunfleck))) %>%
  mutate(sunfleck_ID = ifelse(!is.na(sunfleck), ID, NA)) %>%
  mutate(darkfleck_ID = ifelse(!is.na(darkfleck), ID, NA)) %>%
  filter(!is.na(sunfleck_ID) | !is.na(darkfleck_ID)) %>%
  ungroup() %>%
  group_by(doy.name, date, sunfleck_ID) %>%
  summarise(sampling_interval = median(diff.time, na.rm=T), sunfleck_duration = n() * sampling_interval, sunfleck_peak = max(lux_tilt_rm,na.rm=T), 
            sunfleck_total_lux = sum(lux_tilt_rm, na.rm=T) * sampling_interval) %>%
  filter(!is.na(sunfleck_ID))

### inter-arrival time
darkfleck_hsm <- hsm.all %>%
  filter(type == "sub", doy.name %in% c("168.4B" ,"175.4B", "175.3A", "180.3B","180.3A",
                                        "180.4B", "181.3A","181.3B","187.3A","187.3B","193.3A","193.3B","196.3A","196.3B" )) %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  group_by(doy.name) %>%
  do(pad(., by="dt")) %>%
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5)) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins"))))) %>%
  mutate(sunfleck = ifelse(lux_tilt_rm >= 500, 1, NA)) %>%
  mutate(sunfleck = ifelse(is.na(lux_tilt_rm), NA, sunfleck)) %>%
  mutate(darkfleck = ifelse(lux_tilt_rm < 500, 1, NA)) %>%
  mutate(darkfleck = ifelse(is.na(lux_tilt_rm), NA, darkfleck)) %>%
  mutate(ID = rleid(!is.na(sunfleck))) %>%
  mutate(sunfleck_ID = ifelse(!is.na(sunfleck), ID, NA)) %>%
  mutate(darkfleck_ID = ifelse(!is.na(darkfleck), ID, NA)) %>%
  filter(!is.na(sunfleck_ID) | !is.na(darkfleck_ID)) %>%
  ungroup() %>%
  group_by(doy.name, date, darkfleck_ID) %>%
  summarise(sampling_interval = median(diff.time, na.rm=T), darkfleck_duration = n() * sampling_interval, darkfleck_trough = min(lux_tilt_rm,na.rm=T), 
            darkfleck_total_lux = sum(lux_tilt_rm, na.rm=T) * sampling_interval) %>%
  filter(!is.na(darkfleck_ID))

### Sunfleck analysis for NR
sunfleck_hsn <- hsn.all %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A",
                         "3/10/2015.3B")) %>%
  filter(time > as.hms("6:00:00") & time < as.hms("18:00:00")) %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  group_by(date) %>%
  do(pad(., by="dt")) %>%
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5)) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins"))))) %>%
  mutate(sunfleck = ifelse(lux_tilt_rm >= 500, 1, NA)) %>%
  mutate(sunfleck = ifelse(is.na(lux_tilt_rm), NA, sunfleck)) %>%
  mutate(darkfleck = ifelse(lux_tilt_rm < 500, 1, NA)) %>%
  mutate(darkfleck = ifelse(is.na(lux_tilt_rm), NA, darkfleck)) %>%
  mutate(ID = rleid(!is.na(sunfleck))) %>%
  mutate(sunfleck_ID = ifelse(!is.na(sunfleck), ID, NA)) %>%
  mutate(darkfleck_ID = ifelse(!is.na(darkfleck), ID, NA)) %>%
  filter(!is.na(sunfleck_ID) | !is.na(darkfleck_ID)) %>%
  ungroup() %>%
  group_by(date, sunfleck_ID) %>%
  summarise(sampling_interval = median(diff.time, na.rm=T), sunfleck_duration = n() * sampling_interval, sunfleck_peak = max(lux_tilt_rm,na.rm=T), 
            sunfleck_total_lux = sum(lux_tilt_rm, na.rm=T) * sampling_interval) %>%
  filter(!is.na(sunfleck_ID))


### inter-arrival time
darkfleck_hsn <- hsn.all %>%
  filter(doy.name %in% c("9/25/2014.3B" ,"10/12/2015.3B", "4/10/2016.4A", "4/20/2016.3A",
                         "3/10/2015.3B")) %>%
  filter(time > as.hms("6:00:00") & time < as.hms("18:00:00")) %>%
  mutate(lux_tilt_rm = ifelse(is.na(error_flag), lux_tilt_rm, NA)) %>%
  group_by(date) %>%
  do(pad(., by="dt")) %>%
  mutate(lux_tilt_rm = na.approx(lux_tilt_rm, x=dt, na.rm=F, maxgap=5)) %>%
  mutate(diff.time= as.numeric(c(0, difftime(tail(dt, -1), head(dt, -1),units=c("mins"))))) %>%
  mutate(sunfleck = ifelse(lux_tilt_rm >= 500, 1, NA)) %>%
  mutate(sunfleck = ifelse(is.na(lux_tilt_rm), NA, sunfleck)) %>%
  mutate(darkfleck = ifelse(lux_tilt_rm < 500, 1, NA)) %>%
  mutate(darkfleck = ifelse(is.na(lux_tilt_rm), NA, darkfleck)) %>%
  mutate(ID = rleid(!is.na(sunfleck))) %>%
  mutate(sunfleck_ID = ifelse(!is.na(sunfleck), ID, NA)) %>%
  mutate(darkfleck_ID = ifelse(!is.na(darkfleck), ID, NA)) %>%
  filter(!is.na(sunfleck_ID) | !is.na(darkfleck_ID)) %>%
  ungroup() %>%
  group_by(date, darkfleck_ID) %>%
  summarise(sampling_interval = median(diff.time, na.rm=T), darkfleck_duration = n() * sampling_interval, darkfleck_trough = min(lux_tilt_rm,na.rm=T), 
            darkfleck_total_lux = sum(lux_tilt_rm, na.rm=T) * sampling_interval) %>%
  filter(!is.na(darkfleck_ID))

hist((darkfleck_hsn$darkfleck_duration), breaks = 40)

############################################
### estimate mean interarrival time and mean duration of sunflecks

fit_inter_miss <- fitdist(darkfleck_hsm$darkfleck_duration, "exp", method="mle") 
plot(fit_inter_miss)
summary(fit_inter_miss)
## interarrival time
1/coef(fit_inter_miss)    ## 12 minutes

fit_dur_miss <- fitdist(sunfleck_hsm$sunfleck_duration, "exp", method="mle") 
plot(fit_dur_miss)
summary(fit_dur_miss)
## interarrival time
1/coef(fit_dur_miss)    # 14 minutes duration of 

###, method="mle"
fit_inter_neu <- fitdist(darkfleck_hsn$darkfleck_duration, "exp", method="mle") 
plot(fit_inter_neu)
summary(fit_inter_neu)
## interarrival time
1/coef(fit_inter_neu)    ## 6.8 minutes inter

fit_dur_neu <- fitdist(sunfleck_hsn$sunfleck_duration, "exp", method="mle") 
plot(fit_dur_neu)
summary(fit_dur_neu)
## interarrival time
1/coef(fit_dur_neu)  # 1.6 minutes duration of 

