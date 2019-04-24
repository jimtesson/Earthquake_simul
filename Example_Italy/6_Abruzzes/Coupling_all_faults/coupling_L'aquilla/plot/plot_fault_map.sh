#!/bin/bash

#########################################
##### start coupling ####################
#./coupling


#fileu='../output/SLIP/u_2Fc'
#filec='../output/CFS/c_2Fc'
fileconfig='../input/i_2Fc'
filematrix='../output/m_2Fc'

fig=map_cos.ps

pi=`echo "4*a(1)" | bc -l`
degre=`echo "($pi/180)" | bc -l`

nfaults=$(awk '{if(NR == 22){ print $1}}' $fileconfig)
Rextent="320/440/4600/4730"

## paper size
papersize=`echo "300*($nfaults+1)" | bc -l`
papersize2=$papersize'x'$papersize
echo $papersize2
gmt gmtset PS_MEDIA="A3"

# grid resolution
resol=1.5 # in km

for (( i=1; i<$nfaults+1; i++ ));
do
#lecture du nom de la faille
faultname[$i]=$(awk -v i=$i '{if(NR == 24+7*(i-1)){ print $2}}' $fileconfig)
#lecture des geometries
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

#B	42.5/42.5/255
#F	255/42.5/42.5
#N	127.5

fig="map_all_faults.ps"
gmt psbasemap -Jx0.15 -Ba20f10:"x":/a20f10:"y":WeSn -R$Rextent   -Y9 -X10  -K  > $fig


for (( k=1; k<$nfaults+1; k++ ))
do
source=$k
echo "fault $k"
#fault-trace
color='black'
gmt psxy <<! -J -R -W2,$color -Sf-1/0.3+r -K -O  >> $fig # F1
>
${xsf[$k]} ${ysf[$k]}
${xff[$k]} ${yff[$k]}
>
!
gmt psxy <<! -J -R -Gred -Sc0.3 -K -O  >> $fig # F1
>
${xc[$k]} ${yc[$k]}
>
!

gmt pstext <<! -R -J  -F+f8p,Helvetica -K -O >> $fig
>
${xc[$k]} ${yc[$k]} ${faultname[$k]}
>
!

done
gmt psxy <<! -J -R -W2,$color -Sf-1/0.3+r -O  >> $fig # F1
>
${xsf[$k]} ${ysf[$k]}
${xff[$k]} ${yff[$k]}
>
!
gmt ps2raster $fig -Au -P -Tg -E400