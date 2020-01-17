#!/bin/bash

tmp_file='tmp_input.txt'
filename=$(basename -- "$1")
dirname=$(dirname -- "$1")
extension="${filename##*.}"
ini_rootname="${filename%.*}"
cd $dirname
# if .F file removing preprocessor lines (makes doxygen bug)
if [[ $extension -eq 'F' ]]; then
  rootname="tmp_${ini_rootname}"
  sed -e '/^#/d' $filename > ${rootname}.${extension}
else
  rootname=${ini_root_name}
fi

echo "${rootname} 2 10 T F" > $tmp_file
~/work_in_progress/to_f90/sol_f77_to_f90/f77_to_f90 < $tmp_file > /dev/null

# Modification for some cases
# Bug in api_coupling (end module deleted)
nmod=`grep -E "^[ ]*MODULE [A-Z]+" ${filename}|wc -l`
if [[ $nmod -ne '0' ]]; then
  sed -i -e "s/END SUBROUTINE .*/END MODULE/g" ${rootname}.f90
  sed -i -e "s/END FUNCTION .*/END MODULE/g" ${rootname}.f90
fi
nprog=`grep -E "^[ ]*PROGRAM [A-Z]+" ${filename}|wc -l`
if [[ $nprog -ne '0' ]]; then
  sed -i -e "s/END SUBROUTINE .*/END PROGRAM/g" ${rootname}.f90
fi
sed -i -e "s/MODULE\([A-Z_]\+\)/MODULE \1/g" ${rootname}.f90
sed -i -e "s/^\([ ]*\)USE\([A-Z_]\+\)/\1USE \2/g" ${rootname}.f90
sed -i -e "s/ENDSUBROUTINE\([A-Z_]\+\)/END SUBROUTINE \1/g" ${rootname}.f90
sed -i -e "s/RECURSIVESUBROUTINE/RECURSIVE SUBROUTINE /g" ${rootname}.f90
sed -i -e "s/ENDFUNCTION\([A-Z_]\+\)/END FUNCTION \1/g" ${rootname}.f90
sed -i -e "s/[ ]\+$//g" ${rootname}.f90
sed -i -e "s/= >/=> /g" ${rootname}.f90
sed -i -e "s/DOUBLEPRECISION/DOUBLE PRECISION/g" ${rootname}.f90
sed -i -e "s/ \(([:, ]\+)\)/\1/g" ${rootname}.f90
sed -i -e "s/^\([ ]*\)TYPE /\1TYPE/g" ${rootname}.f90
sed -i -e "s/[ ]*::[ ]*/ :: /g" ${rootname}.f90
sed -i -e "s/ (/(/g" ${rootname}.f90
sed -i -e "s/(LEN = /(LEN=/g" ${rootname}.f90
sed -i -e "s/= =/==/g" ${rootname}.f90
sed -i -e "s/(LU,[ ]*\*[ ]*)/(LU,*)/g" ${rootname}.f90


cat ${rootname}.f90
rm ${rootname}.f90
# Removing temporary source that was created by previous lines
if [[ $extension -eq 'F' ]]; then
  rm tmp_${ini_rootname}.${extension}
fi
rm $tmp_file
cd - > /dev/null
