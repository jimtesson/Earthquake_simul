#!/bin/bash

#########################################
##### start coupling ####################
#./coupling



#####################################################################
### Displcaments
#################
fig=section_F1_cos.ps
file="../output/s_2Fc"
Rextent=0/150/-30/0

awk '{print $4,-$3,$7}' $file |gmt xyz2grd  -Ggmt/section.grd -I0.3 -R$Rextent

#gmt grd2cpt -L-1/1  -Z gmt/section.grd  > gmt/slip.cpt
#gmt makecpt -T-1/1/0.02 -Z > gmt/slip.cpt

gmt grdimage gmt/section.grd -Jx0.15 -Cgmt/slip.cpt -Ba20f10:"y":/a5f1:"z":WeSn -K   -X4 -Y20  -R  > $fig
gmt grdcontour gmt/section.grd  -J -L-1/1 -Gd3c -A0.1+f5  -W0.1 -R -K -O  >> $fig
#awk '{if(NR%10==0){print 0.001*$4,-$3,$6,$7,.0,.0,.0,.0}}' $file | gmt psvelo  -J -R -Se0.2/.95/0  -K -O  >> $fig
#echo "Slip F1" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

gmt psscale  -Ba0.5f0.1:"Slip (m)": -Cgmt/slip.cpt  -D4/-2/10/0.3h -K -O >> $fig


#####################################################################
### CFS
#################
awk '{print $4,-$3,$8}' $file |gmt xyz2grd  -Ggmt/section.grd -I0.3 -R$Rextent

#gmt grd2cpt -L-60/60 -E100 -Z gmt/section.grd  > gmt/slip.cpt
gmt grdimage gmt/section.grd -Jx0.15 -Cgmt/special.cpt -Ba20f10:"y":/a5f1:"z":WeSn -K -O  -Y-11  -R  >> $fig
gmt grdcontour gmt/section.grd  -J -L-5/5 -Gd4c -A1+f5 -W0.1 -R -K -O -V >> $fig
#awk '{if(NR%5==0){print $2,-$3,$6,$7,.0,.0,.0,.0}}' $file | gmt psvelo  -J -R -Se0.2/.95/0  -K -O  >> $fig
#echo "Slip F1" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig


gmt psscale -Cgmt/special.cpt -L -G-5/5 -O -Cgmt/special.cpt -X5  -D4/-2/10/0.3h >> $fig

gmt ps2raster $fig -Au -P -Tg -E400
