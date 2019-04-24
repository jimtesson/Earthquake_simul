#!/bin/bash

tmin=0
tmax=200000

fig=stress.ps

# No coseismic, no post-seismic
awk '{print $1,$2}' ../results/1_no/stress_c |gmt psxy  -JX15/5 -R$tmin/$tmax/800/1000 -Ba50000f10000:"Time":/a200f100:"Stress":WeSn -W0.1p,red -P -Y24 -K  > $fig 
awk '{print $1,$3}' ../results/1_no/stress_c |gmt psxy  -J -R  -W0.1p,blue  -K -O  >> $fig
echo "no interaction" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig


# only coseismic 
awk '{print $1,$2}' ../results/2_co/stress_c |gmt psxy  -JX15/5 -R$tmin/$tmax/800/1000 -Ba50000f10000:"Time":/a200f100:"Stress":WeSn -W0.1p,red -P -Y-7 -K -O >> $fig 
awk '{print $1,$3}' ../results/2_co/stress_c |gmt psxy  -J -R  -W0.1p,blue  -K -O  >> $fig
echo "co-seismic" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig
#awk '{print $1,$3}' ../results/noc_nor/stress_c |gmt psxy  -J -R  -W0.2p,blue  -K -O  >> $fig
#awk '{print $1,$4}' ../input/stress_c |gmt psxy  -J -R  -W0.2p,green,-  -K -O >> $fig
#awk '{print $1,$2}' ../input/stress_c |gmt psxy  -J -R  -Wthin,yellow  -K -O >> $fig

# coseismic + post-seismic
awk '{print $1,$2}' ../results/3_copo/stress_c |gmt psxy  -JX15/5 -R$tmin/$tmax/800/1000 -Ba50000f10000:"Time":/a200f100:"Stress":WeSn -W0.1p,red -P -Y-7 -K -O >> $fig 
awk '{print $1,$3}' ../results/3_copo/stress_c |gmt psxy  -J -R  -W0.1p,blue  -K -O  >> $fig
echo "co-seismic + post-seismic" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig
#awk '{print $1,$3}' ../results/noc_nor/stress_c |gmt psxy  -J -R  -W0.2p,blue  -K -O  >> $fig
#awk '{print $1,$4}' ../input/stress_c |gmt psxy  -J -R  -W0.2p,green,-  -K -O >> $fig
#awk '{print $1,$2}' ../input/stress_c |gmt psxy  -J -R  -Wthin,yellow  -K -O >> $fig



################
### Histogram
################
# No coseismic, no post-seismic
gmt pshistogram ../results/1_no/recurr -JX4.5/4.5 -R0/5000/0/10000 -W200 -B2500:'T inter':/2000:'Nb earthquakes':neSW  -G255/0/0    -V   -Y-7  -O -K >>  $fig
echo "no interaction" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

# only coseismic 
gmt pshistogram ../results/2_co/recurr -JX4.5/4.5 -R0/5000/0/10000 -W200 -B2500:'T inter':/2000::neSw  -G255/0/0    -V   -X6  -O -K >>  $fig
echo "Only coseismic" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

# coseismic + post-seismic
gmt pshistogram ../results/3_copo/recurr -JX4.5/4.5 -R0/5000/0/10000 -W200 -B2500:'T inter':/2000::neSw  -G255/0/0    -V   -X6  -O -K >>  $fig
echo "Cos + Post" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -O >> $fig


################
### Delta sigma extra
#################

fig='delta-stress.ps'

#awk '{print $1,0.0}' ../results/1_no/stress_l |gmt psxy  -JX15/5 -R$tmin/$tmax/-5/5 -Ba50000f10000:"x":/a1f0.5:"y":WeSn -W0.1p,red -V -P -Y24 -K > $fig
#awk '{print $1,0.0}' ../results/1_no/stress_l |gmt psxy  -J -R  -W0.2p,blue -V -K -O >> $fig

awk '{print $1,$2}' ../results/2_co/stress_l |gmt psxy  -JX15/5 -R$tmin/$tmax/-5/5 -Ba50000f10000:"x":/a1f0.5:"y":WeSn -W0.1p,red -V -P -Y24 -K > $fig
awk '{print $1,$3}' ../results/2_co/stress_l |gmt psxy  -J -R  -W0.2p,blue -V -K -O >> $fig

awk '{print $1,$2}' ../results/3_copo/stress_l |gmt psxy  -J -R -Ba50000f10000:"x":/a1f0.5:"y":WeSn -W0.1p,red  -Y-7 -V -K -O >> $fig
awk '{print $1,$3}' ../results/3_copo/stress_l |gmt psxy  -J -R  -W0.2p,blue -V -O >> $fig

gmt ps2raster $fig -Au -P -Tg -E720

#gs $fig
