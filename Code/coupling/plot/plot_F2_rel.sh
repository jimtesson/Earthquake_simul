#!/bin/bash

#########################################
##### start coupling ####################
#./coupling

#input file
fileu='../output/u_2F2r'
filec='../output/c_2F2r'

#Figure
fig=F2_rel.ps

#constant
pi=`echo "4*a(1)" | bc -l`
degre=`echo "($pi/180)" | bc -l`

#fault 1
xc=100
yc=100
az=270
L=50
dlxi=`echo "s($degre*(180-$az))*0.5*$L" | bc -l`
dlyi=`echo "-c($degre*(180-$az))*0.5*$L" | bc -l`
xsf1=`echo "$xc-($dlxi)" | bc -l`
ysf1=`echo "$yc-($dlyi)" | bc -l`
xff1=`echo "$xc+($dlxi)" | bc -l`
yff1=`echo "$yc+($dlyi)" | bc -l`

#fault 2
xc=100
yc=150
az=270
L=50
dlxi=`echo "s($degre*(180-$az))*0.5*$L" | bc -l`
dlyi=`echo "-c($degre*(180-$az))*0.5*$L" | bc -l`
xsf2=`echo "$xc-($dlxi)" | bc -l`
ysf2=`echo "$yc-($dlyi)" | bc -l`
xff2=`echo "$xc+($dlxi)" | bc -l`
yff2=`echo "$yc+($dlyi)" | bc -l`




#############################
#		F1  Slip
#############################

source=2

awk -v source=$source '{if($1 == source) {print $2,$3,$7}}' $fileu |gmt xyz2grd  -Ggmt/slip_F1.grd -I2 -R0/300/0/300
#awk '{print $1,$2,$6}' bin/v4F1 |gmt xyz2grd  -Ggmt/slip_F1.grd -I5 -R0/600/0/600

gmt grd2cpt -L-1/1 -Z gmt/slip_F1.grd  > gmt/slip.cpt
gmt grdimage gmt/slip_F1.grd -JX7 -Cgmt/slip.cpt -Ba50f10:"x":/a50f10:"y":WeSn  -K   -X5 -Y3 -R  > $fig
#fault-trace
gmt psxy <<! -J -R -W2,white -K -O  >> $fig # F1
>
$xsf1 $ysf1
$xff1 $yff1
>
!
gmt psxy <<! -J -R -W2,white -K -O  >> $fig # F1
>
$xsf2 $ysf2
$xff2 $yff2
>
!


#Plot vecteur: ux-uy
awk -v source=$source '{if($1 == source && NR%10 == 0){ print $2,$3,$5,$6,.0,.0,.0,.0}}' $fileu | gmt psvelo  -J -R -Se0.4/.95/0  -K  -O    >> $fig
#awk '{print $1,$2,$4,$5,.0,.0,.0,.0}' bin/v4F1 | gmt psvelo  -J -R -Se0.1/.95/0  -K  -O  >> $fig
 
echo "Slip F1" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

#############################
#		F2->F1
#############################


source=2
target=1


awk -v source=$source -v target=$target '{if(($1 == source)&&($2 == target)) { print $3,$4,$8}}' $filec |gmt xyz2grd  -Ggmt/call_coseismic.grd -I2 -R0/300/0/300
gmt grdimage gmt/call_coseismic.grd -JX7 -Cgmt/special.cpt -Ba50f10:"x":/a50f10:"y":WeSn  -K -O  -Y9 -R  >> $fig

#fault-trace
gmt psxy <<! -J -R -W2,red -K -O  >> $fig # F1
>
$xsf1 $ysf1
$xff1 $yff1
>
!
gmt psxy <<! -J -R -W2,white -K -O  >> $fig # F1
>
$xsf2 $ysf2
$xff2 $yff2
>
!


echo "F2->F1" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

gmt psscale   -Ba0.5f0.1:"Slip (m)": -O -Cgmt/slip.cpt -Y-8 -X-1 -D4/-2/7/0.3h >> $fig

#gs -swap $fig 

gmt ps2raster $fig -Au -P -Tg -E400

