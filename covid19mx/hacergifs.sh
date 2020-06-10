#!/usr/bin/env bash

cd gifs
cd acum
mkdir "AS" "BC" "BS" "CC" "CH" "CL" "CM" "CS" "DF" \
 "DG" "GR" "GT" "HG" "JC" "MC" "MN" "MS" "NL" "NT" \
 "OC" "PL" "QR" "QT" "SL" "SP" "SR" "TC" "TL" "TS" \
 "VZ" "YN" "ZS"
cd ..
cd diario
mkdir "AS" "BC" "BS" "CC" "CH" "CL" "CM" "CS" "DF" \
 "DG" "GR" "GT" "HG" "JC" "MC" "MN" "MS" "NL" "NT" \
 "OC" "PL" "QR" "QT" "SL" "SP" "SR" "TC" "TL" "TS" \
 "VZ" "YN" "ZS"
cd ..
cd ..

Rscript --vanilla historia_datos.R

cd gifs
for f1 in *;
do cd $f1;
{
for f2 in *;
do cd $f2;
echo "creando $f1$f2.gif"
convert -delay 33 *.png "../$f1$f2.gif";
echo "$f1$f2.gif creado";
cd ..;
rm -r $f2;
done
};
cd ..;
done
