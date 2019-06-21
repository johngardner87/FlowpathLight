# FlowpathLight

This data set and code is associated with Gardner et al. 2019 in press Limnology and Oceanography.

1_clean - this file cleans and analyzes drifter data.

In the data folder there are raw and cleaned drifter datasets from the Upper Mississippi River (UMR) in Wisconsin and Neuse River (NR) in North Carolina.

The cleaned data includes the following columns...

* dt=date time (UMR in CDT, NR in EST);
* temp=temperature in C;
accelerometer data=
ax,
ay,
az,
gx,
gy,
gz,
ax.c,
ay.c,
az.c;
distance=meters along a river interpolated from sparse GPS coordinates;
velocity=m/s calculated from the distance and time traveled in between coordinated;
name=the ID of the drifter;
type=surface or subsurface drifter;
hr=hour of the day;
date=date;
time=time in local time zone;
channel=channel (1-3) of the UMR. This only applies to UMR data;
depth=depth from bottom of drifter to water surface in meters estimated from pressure;
depth.top=corrected depth at the top of the drifter in meters;
lux=light in lux (lumens/m2);
doy=doy of year;
doy.name=doy of year + drifter name for unique identifier;
river=UMR or NR;
deg_from_vert=degress tilted from vertical axes;
deg_from_vert_est=missing data filled in the median deg_from_vert;
lux_tilt=cosine corrected lux for sensor tilt;
tilt_flag=flag data that is tilting too much to reasonably correct;
lux_tilt_rm=new column for lux_tilt setting flaged data to NA;
error_flag=flag bad data due to trapping in wood jams, loss of neautral buoyancy, etc.;
diff.time=time between measurements in minutes;
solar.dt=solar date time;
zenith=solar zenith angle given the date-time and location;
solar.time=time of day in solar time;
sunrise=time of sunrise;
sunset=time of sunset;



