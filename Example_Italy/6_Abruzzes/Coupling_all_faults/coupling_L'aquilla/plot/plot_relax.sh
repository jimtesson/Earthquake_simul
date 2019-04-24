#!/bin/bash

#########################################
##### start coupling ####################
#./coupling


#fileu='../output/SLIP/u_2Fc'
#filec='../output/CFS/c_2Fc'
fileconfig='../input/i_2Fr'

fig=map_rel.ps

pi=`echo "4*a(1)" | bc -l`
degre=`echo "($pi/180)" | bc -l`

nfaults=$(awk '{if(NR == 22){ print $1}}' $fileconfig)
Rextent="330/480/4590/4740"

## paper size
papersize=`echo "300*($nfaults+1)" | bc -l`
papersize2=$papersize'x'$papersize
echo $papersize2
gmt gmtset PS_MEDIA=$papersize2

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
#gmt grd2cpt -L-1/1 -Z gmt/slip_F1.grd  > gmt/slip.cpt
gmt psscale   -Ba0.5f0.1:"Slip (m)":  -Cgmt/slip.cpt -Y5 -X5 -D4/-2/7/0.3h -K > $fig
gmt psscale    -Cgmt/special.cpt  -L  -D4/-2/10/0.3h -X15 -K -O >> $fig

for (( k=1; k<$nfaults+1; k++ ))
do

source=$k

fileu="../output/SLIP/u_2Fr_$k"

if [ $k == 1 ]
then
gmt psbasemap -JX7 -Ba50f10:"x":/a50f10:"y":WeSn -R$Rextent   -Y2 -X-15 -K -O >> $fig
else
locx=`echo "-($nfaults-1)*10" | bc -l`
gmt psbasemap -JX7 -Ba50f10:"x":/a50f10:"y":WeSn -R$Rextent  -Y10 -X$locx -K -O >> $fig
fi

awk -v source=$source '{if($1 == source) {print $2,$3,$7}}' $fileu |gmt xyz2grd  -Ggmt/slip_F1.grd -I$resol -R$Rextent
#awk '{print $1,$2,$6}' bin/v4F1 |gmt xyz2grd  -Ggmt/slip_F1.grd -I5 -R0/600/0/600

#gmt grd2cpt -L-1/1 -Z gmt/slip_F1.grd  > gmt/slip.cpt
gmt grdimage gmt/slip_F1.grd -JX7 -R -Cgmt/slip.cpt   -K -O     >> $fig

#fault-trace
i=$source
color='black'
gmt psxy <<! -J -R -W2,$color -Sf-1/0.3+r -K -O  >> $fig # F1
>
${xsf[$i]} ${ysf[$i]}
${xff[$i]} ${yff[$i]}
>
!

for (( i=1; i<$nfaults+1; i++ ));
do


if [ $i != $source ]
then
color='white'
gmt psxy <<! -J -R -W1,$color -Sf-1/0.3+r -K -O  >> $fig # F1
>
${xsf[$i]} ${ysf[$i]}
${xff[$i]} ${yff[$i]}
>
!
fi
done

#Plot vecteur: ux-uy
#awk -v source=$source '{if($1 == source && NR%10 == 0){ print $2,$3,$5,$6,.0,.0,.0,.0}}' $fileu | gmt psvelo  -J -R -Se0.4/.95/0  -K  -O    >> $fig
#awk '{print $1,$2,$4,$5,.0,.0,.0,.0}' bin/v4F1 | gmt psvelo  -J -R -Se0.1/.95/0  -K  -O  >> $fig
 
echo "Slip fault ${faultname[$source]}" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -O -K >> $fig



###############################
### plot the CFS for each fault
###############################

for (( j=1; j<$nfaults+1; j++ ))
do
    target=$j
    filec="../output/CFS/c_2Fr_"$k"_"$j

    
    if [ $target != $source ]
    then

    gmt psbasemap -JX7 -Ba50f10:"x":/a50f10:"y":WeSn -R$Rextent -X10 -O -K >> $fig
    	if [ ${CFSMAP[$source]} == 1 ]
    	then
    	echo 'source:'$source' -> target:'$target
		echo $filec
		
	awk -v source=$source -v target=$target '{print $3,$4,$8}' $filec |gmt xyz2grd  -Ggmt/call_coseismic.grd -I$resol -R
	gmt grdimage gmt/call_coseismic.grd -JX7 -Cgmt/special.cpt   -K -O  -R  >> $fig
	gmt grdcontour gmt/call_coseismic.grd  -J -Cgmt/cfsrel2.cont  -A0.5+f5 -W0.1 -R -K -O  >> $fig
		fi
# plot fault trace source
i=$source
color='black'
gmt psxy <<! -J -R -W2,$color -Sf-1/0.3+r -K -O  >> $fig # F1
>
${xsf[$i]} ${ysf[$i]}
${xff[$i]} ${yff[$i]}
>
!


# plot fault trace target
i=$target
color='white'
gmt psxy <<! -J -R -W1,$color -Sf-1/0.3+r  -K -O  >> $fig # F1
>
${xsf[$i]} ${ysf[$i]}
${xff[$i]} ${yff[$i]}
>
!



echo "${faultname[$source]}->${faultname[$target]}" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -K -O >> $fig

    fi


done # loop j

done # loop k
# end of file
echo "" | gmt pstext -R -J -Gblack  -F+cTL+fwhite -O >> $fig
#
gmt ps2raster $fig -Au -P -Tg -E400