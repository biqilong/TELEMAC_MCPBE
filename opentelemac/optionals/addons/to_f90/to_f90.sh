#!/bin/bash

tmp_file='tmp_input.txt'
filename=$(basename -- "$1")
dirname=$(dirname -- "$1")
extension="${filename##*.}"
ini_rootname="${filename%.*}"
cd $dirname
# if .F file removing preprocessor lines (makes doxygen bug)
if [[ ${extension} == 'F' ]]; then
  rootname="tmp_${ini_rootname}"
  sed -e '/^#/d' $filename > ${rootname}.${extension}
  new_ext=F90
else
  rootname=${ini_rootname}
  new_ext=f90
fi

echo "${rootname} 2 10 T F" > $tmp_file
$HOMETEL/optionals/addons/to_f90/f77_to_f90 < $tmp_file > /dev/null

new_file=${rootname}.${new_ext}

# Modification for some cases
# Bug in api_coupling (end module deleted)
nmod=`grep -E "^[ ]*MODULE [A-Z]+" ${filename}|wc -l`
if [[ $nmod -ne '0' ]]; then
  sed -i -e "s/END SUBROUTINE .*/END MODULE/g" ${new_file}
  sed -i -e "s/END FUNCTION .*/END MODULE/g" ${new_file}
fi
nprog=`grep -E "^[ ]*PROGRAM [A-Z]+" ${filename}|wc -l`
if [[ $nprog -ne '0' ]]; then
  sed -i -e "s/END SUBROUTINE .*/END PROGRAM/g" ${new_file}
fi
sed -i -e "s/MODULE\([A-Z_]\+\)/MODULE \1/g" ${new_file}
sed -i -e "s/MODULE PROCEDURE\([A-Z_]\+\)/MODULE PROCEDURE \1/g" ${new_file}
sed -i -e "s/^\(\s*\)INTERFACE\([A-Z_]\+\)/\1INTERFACE \2/g" ${new_file}
sed -i -e "s/^\([ ]*\)USE\([A-Z_]\+\)/\1USE \2/g" ${new_file}
sed -i -e "s/^\([ ]*\)TYPE\([A-Z_]\+\)/\1TYPE \2/g" ${new_file}
sed -i -e "s/^\([ ]*\)ENDTYPE\([A-Z_]\+\)/\1END TYPE \2/g" ${new_file}
sed -i -e "s/ENDSUBROUTINE\([A-Z_]\+\)/END SUBROUTINE \1/g" ${new_file}
sed -i -e "s/RECURSIVESUBROUTINE/RECURSIVE SUBROUTINE /g" ${new_file}
sed -i -e "s/ENDFUNCTION\([A-Z_]\+\)/END FUNCTION \1/g" ${new_file}
sed -i -e "s/PUREFUNCTION\([A-Z_]\+\)/PURE FUNCTION \1/g" ${new_file}
sed -i -e "s/ENDSUBROUTINE/END SUBROUTINE/g" ${new_file}
sed -i -e "s/ENDFUNCTION/END FUNCTION/g" ${new_file}
sed -i -e "s/ENDINTERFACE/END INTERFACE/g" ${new_file}
# Removing trailing spaces
sed -i -e "s/[ ]\+$//g" ${new_file}
# Correcting pointer assignment
sed -i -e "s/= >/=> /g" ${new_file}
sed -i -e "s/ %/%/g" ${new_file}
sed -i -e "s/\/ =/\/=/g" ${new_file}
# Corrections for double precision
sed -i -e "s/DOUBLEPRECISION/DOUBLE PRECISION/g" ${new_file}
# Removing spaces beofre :,
sed -i -e "s/ \(([:, ]\+)\)/\1/g" ${new_file}
sed -i -e "s/^\([ ]*\)TYPE /\1TYPE/g" ${new_file}
# :: ->  ::
sed -i -e "s/[ ]*::[ ]*/ :: /g" ${new_file}
# Removing spaces before ( and )
sed -i -e "s/ (/(/g" ${new_file}
sed -i -e "s/ )/)/g" ${new_file}
# Transformer LEN = * into LEN=* and same with KIND
sed -i -e "s/(LEN\s*=\s*\([^ ]\+\)\s*)/(LEN=\1)/g" ${new_file}
sed -i -e "s/(KIND\s*=\s*\([^ ]\+\)\s*)/(KIND=\1)/g" ${new_file}
sed -i -e "s/( \* )/(*)/g" ${new_file}
# Corrections for ==
sed -i -e "s/= =/==/g" ${new_file}
# Corrections for lu, *
sed -i -e "s/(LU,[ ]*\*[ ]*)/(LU,*)/g" ${new_file}
# Correction for array init (/ ... /)
sed -i -e "s/( \//(\//g" ${new_file}
sed -i -e "s/\/ )/\/)/g" ${new_file}
# case default correction
sed -i -e "s/CASEDEFAULT/CASE DEFAULT/g" ${new_file}

# Changing header to doxygen header style
sed -i -e "s/^!\s*|\s*\(\w\+\)\s*|\s*\(.\+\)\s*|\s*\(.\)\(.*\)/!>@param[\2] \1 \3\L\4/g" ${new_file}
sed -i -e "s/^![ ]*|[ ]\+|[ ]\+|/!!  /g" ${new_file}
sed -i -e "s/^\([ ]*\)!BRIEF/\1!>@brief/g" ${new_file}
sed -i -e "s/^\([ ]*\)!brief/\1!>@brief/g" ${new_file}
sed -i -e "s/^\([ ]*\)!history/\1!>@history/g" ${new_file}
sed -i -e "s/^\([ ]*\)!HISTORY/\1!>@history/g" ${new_file}
sed -i -e "s/\[-->\]/[in]/g" ${new_file}
sed -i -e "s/\[<--\]/[out]/g" ${new_file}
sed -i -e "s/\[<->\]/[in, out]/g" ${new_file}
sed -i -e "/\s*!+\s*$/d" ${new_file}
sed -i -e "s/\(\s*\)!+ /\1!! / g" ${new_file}


cat ${new_file}
rm ${new_file}
# Removing temporary source that was created by previous lines
if [[ $extension == 'F' ]]; then
  rm tmp_${ini_rootname}.${extension}
fi
rm $tmp_file
cd - > /dev/null
