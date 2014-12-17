#!/bin/bash
#mkdir -p ./bashOutput/
#rm -r ./bashOutput/
#mkdir -p ./bashOutput/


s=$(<timeData_all.txt)
set -- $s
mkdir -p ./timeData/


for var in "$@"
do
#mkdir -p ./data/
echo " ======= bash time: $var"
echo $var >> ./timeData/$var.txt

#mkdir -p ./data/my/$var
#rm -r ./data/my/$var
#mkdir -p ./data/my/data
#rm -r ./data/my/data

#echo " ======= compile pdf2f90.f90 -> pdf "
#gfortran PDF2f90.f90 -o pdf
#echo " ======= run ./pdf > ./bashOutput/pdf$var.txt"
#./pdf < ./timeData/$var.txt >./bashOutput/pdf$var.txt

#echo " ======= copyPDFbeta2f90.f90 -> copyPDF "
#gfortran copyPDFbeta2f90.f90 -o copyPDF
#echo " ======= run ./copyPDF > ./bashOutput/copypdf$var.txt"
#./copyPDF < ./timeData/$var.txt >./bashOutput/copypdf$var.txt

#echo " ======= copyConcentrationf90_m.f90 -> concentr "
#gfortran copyConcentrationf90_m.f90 -o concentr
#echo " ======= run ./concentr > ./bashOutput/concentr$var.txt"
#./concentr < ./timeData/$var.txt >./bashOutput/concentr$var.txt

echo " ======= copyAllconcentrationf -> allConcentr "
gfortran copyAllconcentrationf90.f90 -o allConcentr
echo " ======= run ./concentr > ./bashOutput/allConcentr$var.txt"
./allConcentr < ./timeData/$var.txt >./bashOutput/allConcentr$var.txt

done

rm -r ./timeData/

echo " ======= copyFullAllconcentrationf -> fullAllConcentr "
gfortran copyFullAllconcentrationf90.f90 -o fullAllConcentr
echo " ======= run ./fullAllConcentr > ./bashOutput/fullFllConcentr$var.txt"
./fullAllConcentr >./bashOutput/allConcentr$var.txt

