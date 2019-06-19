

library(glmtools)
sim_dir = run_example_sim()

Sys.setenv(tz='UTC')

#compare temps
ncfile = file.path(sim_dir, 'output.nc')
tempfield = file.path(sim_dir, 'field_data.csv')
plot_temp_compare(ncfile, tempfield, resample = FALSE)

rmse = validate_sim(ncfile, tempfield, 'water.temperature', fig_path=FALSE, report = TRUE)

################################################################################
#Ice compare
################################################################################
ice_obs = read.table(file.path(sim_dir, 'snow_ice_depth_obs.csv'), sep=',', header=TRUE)
ice_obs$DateTime = as.POSIXct(ice_obs$sampledate, tz="GMT")
ice_obs$avsnow = ice_obs$avsnow/100
ice_obs$whiteice = ice_obs$whiteice/100
ice_obs$blueice  = ice_obs$blueice/100


snow = get_var(ncfile, 'hsnow')
attributes(snow$DateTime)$tzone = 'GMT'
wice = get_var(ncfile, 'hwice')
attributes(wice$DateTime)$tzone = 'GMT'
bice = get_var(ncfile, 'hice')
attributes(bice$DateTime)$tzone = 'GMT'
wtr  = get_temp(ncfile, reference='surface')
attributes(wtr$DateTime)$tzone = 'GMT'

all_ice = merge(snow, ice_obs, all.x=TRUE, by='DateTime')
all_ice = merge(wice, all_ice, all.x=TRUE)
all_ice = merge(bice, all_ice, all.x=TRUE)
all_ice$yday = as.POSIXlt(all_ice$DateTime)$yday
all_ice$year = as.POSIXlt(all_ice$DateTime)$year + 1900

ice = get_ice(ncfile)
attributes(ice$DateTime)$tzone = 'GMT'
ice_on_off = mda.lakes::get_ice_onoff(ice, wtr)

lter_on_off = read.table(file.path(sim_dir,'ice_duration_obs.csv'), sep=',', header=TRUE)
all_ice = merge(all_ice, ice_on_off, all.x=TRUE)
all_ice = merge(all_ice, lter_on_off, all.x=TRUE)

all_ice = all_ice[order(all_ice$DateTime), ]

################################################################################
## Now plots
################################################################################
plot(all_ice$avsnow, all_ice$hsnow)
abline(0,1)

plot(all_ice$whiteice, all_ice$hwice)
abline(0,1)

plot(all_ice$blueice, all_ice$hice)
abline(0,1)

plot(hice~DateTime, subset(all_ice, year < 1990), type='l')
points(all_ice$DateTime, all_ice$blueice)

#png('figures/sp_bice.png', res=300, width=2000, height=2000)
par(mfrow=c(3,1))
plot(hice~DateTime, subset(all_ice, year < 1990), type='l')
title('Blue Ice')
points(all_ice$DateTime, all_ice$blueice)
plot(hice~DateTime, subset(all_ice, year > 1990 & year < 2000), type='l')
points(all_ice$DateTime, all_ice$blueice)
plot(hice~DateTime, subset(all_ice, year > 2000 & year < 2010), type='l')
points(all_ice$DateTime, all_ice$blueice)
#dev.off()

#png('figures/sp_wice.png', res=300, width=2000, height=2000)
par(mfrow=c(3,1))
plot(hwice~DateTime, subset(all_ice, year < 1990), type='l')
title('White Ice')
points(all_ice$DateTime, all_ice$whiteice)
plot(hwice~DateTime, subset(all_ice, year > 1990 & year < 2000), type='l')
points(all_ice$DateTime, all_ice$whiteice)
plot(hwice~DateTime, subset(all_ice, year > 2000 & year < 2010), type='l')
points(all_ice$DateTime, all_ice$whiteice)
#dev.off()

#png('figures/sp_snow.png', res=300, width=2000, height=2000)
par(mfrow=c(3,1))
plot(hsnow~DateTime, subset(all_ice, year < 1990), type='l')
title('Snow')
points(all_ice$DateTime, all_ice$avsnow)
plot(hsnow~DateTime, subset(all_ice, year > 1990 & year < 2000), type='l')
points(all_ice$DateTime, all_ice$avsnow)
plot(hsnow~DateTime, subset(all_ice, year > 2000 & year < 2010), type='l')
points(all_ice$DateTime, all_ice$avsnow)
#dev.off()



####
#ICE

mean(abs(all_ice$firstopen - as.POSIXlt(all_ice$off)$yday+1), na.rm=TRUE)
mean((all_ice$firstopen - as.POSIXlt(all_ice$off)$yday+1), na.rm=TRUE)

plot(all_ice$firstopen, as.POSIXlt(all_ice$off)$yday+1)
abline(0,1)

mean(abs(all_ice$lastopen - as.POSIXlt(all_ice$on)$yday+1), na.rm=TRUE)
mean((all_ice$lastopen - as.POSIXlt(all_ice$on)$yday+1), na.rm=TRUE)

plot(all_ice$lastopen, as.POSIXlt(all_ice$on)$yday+1)
abline(0,1)


##figure for Hipsey paper

#png('figures/sp_bws_plots.png', res=300, width=2000, height=2000)
par(mfrow=c(3,1), oma=c(5,4,0,0), mar=c(0,0,1,1))

plot(hice~DateTime, subset(all_ice, year > 1990 & year < 2001), type='l', ylab='Blue Ice (m)', xaxt='n')
lines(hice~DateTime, all_ice)
points(all_ice$DateTime, all_ice$blueice, col=rgb(0, 0, 0, 0.5), pch=16)
mtext(side=2, text='Blue Ice (m)', line=2.5)
legend('topright', legend = 'A', bty = 'n', inset = c(-0.01,-0.06), cex=2)

plot(hwice~DateTime, subset(all_ice, year > 1990 & year < 2001), type='l', ylab='White Ice (m)', xaxt='n')
lines(hwice~DateTime, all_ice)
points(all_ice$DateTime, all_ice$whiteice, col=rgb(0, 0, 0, 0.5), pch=16)
mtext(side=2, text='White Ice (m)', line=2.5)
legend('topright', legend = 'B', bty = 'n', inset = c(-0.01,-0.06), cex=2)

legend('topleft', legend = c('Modeled', 'Observed'), pch = c(NA, 16), lty=c(1,NA), col=c('black', rgb(0, 0, 0, 0.5)), inset=c(0.01, 0.05))

plot(hsnow~DateTime, subset(all_ice, year > 1990 & year < 2001), type='l', ylab='Snow (m)')
lines(hsnow~DateTime, all_ice)
points(all_ice$DateTime, all_ice$avsnow, col=rgb(0, 0, 0, 0.5), pch=16)
mtext(side=2, text='Snow (m)', line=2.5)
mtext(side=1, text='Year', line=2.5)
legend('topright', legend = 'C', bty = 'n', inset = c(-0.01,-0.06), cex=2)

#dev.off()


### Compare ice on and off for Hipsey paper 

library(mda.lakes)
library(lubridate)
onoff = lter_on_off
mod_onoff = get_ice_onoff(get_ice(ncfile), get_temp(ncfile, reference = 'surface'))

obsmod = merge(onoff, mod_onoff, by='year')


#png('figures/sp_on_off_plots.png', res=300, width=2400, height=1500)
par(mfrow=c(1,2),  mar=c(5,4,1,1))

plot(yday(obsmod$on), yday(as.POSIXct(obsmod$datelastopen)), pch=16, col=rgb(0, 0, 0, 0.5), 
     xlab='Modeled Ice On', ylab='Observed Ice On')
abline(0,1)
plot(yday(obsmod$off[-1]), yday(as.POSIXct(obsmod$datefirstopen[-1])), pch=16, col=rgb(0, 0, 0, 0.5), 
     xlab='Modeled Ice Off', ylab='Observed Ice Off')
abline(0,1)

#dev.off()
