#!/bin/bash

#########################################
##### start coupling ####################
#./coupling


#fileu='../output/SLIP/u_2Fc'
#filec='../output/CFS/c_2Fc'
fileconfig='../input/i_2Fr'
filematrix='../output/m_2Fr'

fig=map_rel.ps

pi=`echo "4*a(1)" | bc -l`
degre=`echo "($pi/180)" | bc -l`

nfaults=$(awk '{if(NR == 22){ print $1}}' $fileconfig)
Rextent="320/440/4600/4730"

## paper size
gmt gmtset PS_MEDIA="A4"

# grid resolution
resol=1.5 # in km

for (( i=1; i<$nfaults+1; i++ ));
do
#lecture du nom de la faille
faultname[$i]=$(awk -v i=$i '{if(NR == 24+7*(i-1)){ print $2}}' $fileconfig)
#lecture des geometries
xc[$i]=$(awk -v i=$i '{if(NR == 25+7*(i-1)){ print $1}}' $fileconfig)
xc[$i]=$(awk -v i=$i '{if(NR == 25+7*(i-1)){ print $1}}' $fileconfig)
yc[$i]=$(awk -v i=$i '{if(NR == 25+7*(i-1)){ print $2}}' $fileconfig)
az[$i]=$(awk -v i=$i '{if(NR == 28+7*(i-1)){ print $1}}' $fileconfig)
L[$i]=$(awk -v i=$i '{if(NR == 26+7*(i-1)){ print $1}}' $fileconfig)
CFSMAP[$i]=$(awk -v i=$i '{if(NR == 30+7*(i-1)){ print $1}}' $fileconfig)
echo faille:${faultname[$i]} xc=${xc[$i]} yc=${yc[$i]} az=${az[$i]} L=${L[$i]} CFSmap=${CFSMAP[$i]}
# calcul des points aux extremites des failles
dlxi=`echo "s($degre*(180-${az[$i]}))*0.5*${L[$i]}" | bc -l`
dlyi=`echo "-c($degre*(180-${az[$i]}))*0.5*${L[$i]}" | bc -l`
xsf[$i]=`echo "${xc[$i]}-($dlxi)" | bc -l`
ysf[$i]=`echo "${yc[$i]}-($dlyi)" | bc -l`
xff[$i]=`echo "${xc[$i]}+($dlxi)" | bc -l`
yff[$i]=`echo "${yc[$i]}+($dlyi)" | bc -l`
done


# Palette
#gmt makecpt -T-2/2/0.5  -Cpolar > gmt/CFS_matrix.cpt
#B	42.5/42.5/255
#F	255/42.5/42.5
#N	127.5

k=1
source=$k

fileu="../output/SLIP/u_2Fr_$k"
fig="CFS_rel/map_rel_${faultname[$k]}.ps"


gmt psbasemap -Jx0.13 -Ba20f10:"x":/a20f10:"y":WeSn -R$Rextent -X3 -P -Y6 -K > $fig

#fault-trace
gmt psxy <<! -J -R -W3 -Sf-1/0.35+r -K -O  >> $fig # F1
>
${xsf[$k]} ${ysf[$k]}
${xff[$k]} ${yff[$k]}
>
!

# other faults
for (( j=1; j<$nfaults+1; j++ ));
do
mat=$(awk -v k=$k -v j="$j" '{if(NR <= 2){ cfs=cfs+$j}} END {print cfs}' $filematrix)
#mat=$(awk -v k=$k -v j="$j" '{if(NR <= 2){ cfs=cfs+$j}} END {print cfs}' $filematrix)
#mat=$(awk -v k=$k -v j="$j" '{if(NR == k) { print $j}}' $filematrix)

if [ $j != $source ]
then
echo $k $j $mat
gmt psxy <<! -J -R -W3 -Cgmt/CFS_matrix.cpt -Sf-1/0.35+r -K -O  >> $fig # F1
> -Z$mat
${xsf[$j]} ${ysf[$j]}
${xff[$j]} ${yff[$j]}
>
!
fi

done

echo "Slip fault ${faultname[$source]}" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -O -K >> $fig
gmt psscale   -Ba1f0.5:"CFS (m)":  -Cgmt/CFS_matrix.cpt -Y-1 -X3 -D4/-2/7/0.3h  -O >> $fig
gmt ps2raster $fig -Au -P -Tg -E400


