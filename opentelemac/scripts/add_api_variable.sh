#!/bin/bash
# Script to automatically add a variable to api_handle_var_*.f

api_handle=$HOMETEL/sources/api/api_handle_var_
api_instance=$HOMETEL/sources/api/api_instance_

###
# Write information into the get function
###
function write_get {

  # specific treatment for bief_obj
  if [[ $var_type == 'bief_double' ]]; then
    real_type="double"
    real_inst_name="$var_inst_name%R"
  elif [[ $var_type == 'bief_integer' ]]; then
    real_type="integer"
    real_inst_name="$var_inst_name%I"
  else
    real_type="$var_type"
    real_inst_name="$var_inst_name"
  fi

  # Defining access to the variable
  if [[ $var_ndim == '0' ]]; then
    inst_name="$real_inst_name"
  elif [[ $var_ndim == '1' ]]; then
    inst_name="$real_inst_name(INDEX1)"
  elif [[ $var_ndim == '2' ]]; then
    # 1 dimensialized array call
    if [[ $var_type == "bief_integer" || $var_type == "bief_double" ]];then
      inst_name="$real_inst_name(\n     &       (INDEX2-1)*INST%$var_inst_name%DIM1 + INDEX1)"
    else
      inst_name="$real_inst_name(INDEX1,INDEX2)"
    fi
  else
    inst_name="$real_inst_name(INDEX1,INDEX2,INDEX3)"
  fi

  if [[ $var_type == "string" ]]; then
    text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          DO J=1,VALUELEN\n            VALEUR(J:J) = INST%$inst_name(J:J)\n          ENDDO"
  elif [[ $var_type == "boolean" ]]; then
    text="\          IF(INST%$inst_name) THEN\n           VALEUR = 0\n         ELSE\n           VALEUR = 1\n         ENDIF"
  else
    text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          VALEUR = INST%$inst_name"
  fi

  tag="! <get_${real_type}>"

  sed -i "/$tag/i $text" $api_handle${module}.f
}

###
# Write information into the set function
###
function write_set {

  # specific treatment for bief_obj
  if [[ $var_type == 'bief_double' ]]; then
    real_type="double"
    real_inst_name="$var_inst_name%R"
  elif [[ $var_type == 'bief_integer' ]]; then
    real_type="integer"
    real_inst_name="$var_inst_name%I"
  else
    real_type="$var_type"
    real_inst_name="$var_inst_name"
  fi

  # Defining access to the variable
  if [[ $var_ndim == '0' ]]; then
    inst_name="$real_inst_name"
  elif [[ $var_ndim == '1' ]]; then
    inst_name="$real_inst_name(INDEX1)"
  elif [[ $var_ndim == '2' ]]; then
    # 1 dimensialized array call
    if [[ $var_type == "bief_integer" || $var_type == "bief_double" ]];then
      inst_name="$real_inst_name(\n     &       (INDEX2-1)*INST%$var_inst_name%DIM1 + INDEX1)"
    else
      inst_name="$real_inst_name(INDEX1,INDEX2)"
    fi
  else
    inst_name="$real_inst_name(INDEX1,INDEX2,INDEX3)"
  fi

  if [[ $var_type == "string" ]]; then
    text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          DO J=1,VALUELEN\n            INST%$inst_name(J:J) = VALEUR(J)\n          ENDDO"
  elif [[ $var_type == "boolean" ]]; then
    text="\          IF(VALEUR.EQ.0) THEN\n           INST%$inst_name = .TRUE.\n         ELSE\n           INST%$inst_name = .FALSE.\n         ENDIF"
  else
    text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          INST%$inst_name = VALEUR"
  fi

  tag="<set_$real_type>"

  sed -i "/$tag/i $text" $api_handle${module}.f
}
###
# Write information into the get_array function
###
function write_get_array {

  # specific treatment for bief_obj
  if [[ $var_type == 'bief_double' ]]; then
    real_type="double"
    real_inst_name="$var_inst_name%R"
  elif [[ $var_type == 'bief_integer' ]]; then
    real_type="integer"
    real_inst_name="$var_inst_name%I"
  else
    real_type="$var_type"
    real_inst_name="$var_inst_name"
  fi

  if [[ $real_type == 'string' || $real_type == 'boolean' ]]; then
    return 0
  fi

  if [[ $var_ndim == '1' ]]; then
    range="1:SIZE(INST%$real_inst_name)"
  else
    return 0
  fi

  text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          VALEUR($range) =\n     &     INST%$real_inst_name($range)"

  tag="! <get_${real_type}_array>"

  sed -i "/$tag/i $text" $api_handle${module}.f
}

###
# Write information into the set function
###
function write_set_array {

  # specific treatment for bief_obj
  if [[ $var_type == 'bief_double' ]]; then
    real_type="double"
    real_inst_name="$var_inst_name%R"
  elif [[ $var_type == 'bief_integer' ]]; then
    real_type="integer"
    real_inst_name="$var_inst_name%I"
  else
    real_type="$var_type"
    real_inst_name="$var_inst_name"
  fi

  if [[ $real_type == 'string' || $real_type == 'boolean' ]]; then
    return 0
  fi

  if [[ $var_ndim == '1' ]]; then
    range="1:SIZE(INST%$real_inst_name)"
  else
    return 0
  fi


  text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          INST%$inst_name($range) = \n     &    VALEUR($range)"

  tag="<set_${real_type_array}_array>"

  sed -i "/$tag/i $text" $api_handle${module}.f
}

###
# Write information into the get_var_size function
###
function write_size {

  if [[ $var_ndim == '0' ]];then
    # do nothing
    return
  elif [[ $var_ndim == '1' ]]; then
    if [[ $var_type == 'bief_integer' || $var_type == 'bief_double' ]]; then
      text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          DIM1 = INST%$var_inst_name%DIM1"
    else
      text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          DIM1 = SIZE(INST%$var_inst_name)"
    fi
  elif [[ $var_ndim == '2' ]]; then
    if [[ $var_type == 'bief_integer' || $var_type == 'bief_double' ]]; then
      text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN\n          DIM1 = INST%$var_inst_name%DIM1\n          DIM2 = INST%$var_inst_name%DIM2"
    else
      echo "WARNING: Not handled"
      exit 1
    fi
  elif [[ $var_ndim == '3' ]]; then
    echo "WARNING: Not handled"
    exit 1
  fi

  tag="<get_var_size>"

  sed -i "/$tag/i $text" $api_handle${module}.f
}

###
# Write information into the get_var_size function
###
function write_type {

  # Define type
  local ftype=""
  if [[ $var_type == "integer" || $var_type == "bief_integer" ]]; then
    ftype="INTEGER"
  elif [[ $var_type == "double" || $var_type == "bief_double" ]]; then
    ftype="DOUBLE"
  elif [[ $var_type == "string" ]]; then
    ftype="STRING"
  elif [[ $var_type == "boolean" ]]; then
    ftype="BOOLEAN"
  fi


  text="\        ELSE IF(TRIM(VARNAME).EQ.'$var_name') THEN \
\n          VARTYPE = '$ftype'\
\n          READONLY = .${var_readonly}.\
\n          NDIM = $var_ndim\
\n          GETPOS = $var_get_pos\
\n          SETPOS = $var_set_pos"


  tag="<get_var_type>"

  sed -i "/$tag/i $text" $api_handle${module}.f

}

###
# Write information into the get_var_list function
###
function write_help {

  text="\          I = I + 1\n          VNAME_T2D(I) = '$var_name'\n          VINFO_T2D(I) = '$var_help'"

  tag="<set_var_list>"

  sed -i "/$tag/i $text" $api_handle${module}.f

}

###
# Increase the value of nvar in the api_handle_var
###
function increase_nvar {

   # Get the actual number of variables
   val=`grep -i "INTEGER, PARAMETER :: NB_VAR" $api_handle${module}.f|grep -io "[0-9]\+\>"`
   # Increment it
   new_val=`expr ${val} + 1`

   sed -i "s/INTEGER, PARAMETER :: NB_VAR_${module^^}=$val/INTEGER, PARAMETER :: NB_VAR_${module^^}=$new_val/g" $api_handle${module}.f

}

###
# Adding the variable into the instance
###
function add_instance {

  local real_type=""
  local real_inst_name=""

  if [[ $var_type == "" ]]; then
    echo
  fi

  if [[ $var_type == "bief_integer" || $var_type == "bief_double" ]];then
    real_type="TYPE(BIEF_OBJ)"
    real_inst_name="${var_inst_name}"
  else
    if [[ $var_type == "integer" ]]; then
      real_type="INTEGER"
    elif [[ $var_type == "double" ]]; then
      real_type="DOUBLE PRECISION"
    elif [[ $var_type == "boolean" ]]; then
      real_type="LOGICAL"
    elif [[ $var_type == "string" ]]; then
      real_type="CHARACTER(LEN=$var_dim1)"
    fi
    real_inst_name=$var_inst_name
    if [[ $var_ndim == "1" ]]; then
      real_inst_name="${real_inst_name}(:)"
    elif [[ $var_ndim == "2" ]]; then
      real_inst_name="${real_inst_name}(:,:)"
    fi
  fi

  # Adding new variable in instance
  tag="<new_var>"

  text="\        $real_type, POINTER :: $real_inst_name"

  sed -i "/$tag/i $text" $api_instance${module}.f

  # Assing the setting of the pointer
  tag="<new_link>"

  text="\        INSTANCE_LIST_${module^^}(ID)%$var_inst_name => $var_inst_name"

  sed -i "/$tag/i $text" $api_instance${module}.f

}

function add_variable {

  echo "Adding $var_name in $module"
  write_get
  echo " -> $? write_get"
  write_set
  echo " -> $? write_set"
  write_size
  echo " -> $? write_size"
  write_type
  echo " -> $? write_type"
  write_help
  echo " -> $? write_help"
  increase_nvar
  echo " -> $? increas_nvar"
  add_instance
  echo " -> $? add_instance"
  write_get_array
  echo " -> $? write_get_array"
  write_set_array
  echo " -> $? write_set_array"

}
#Returns incorrect lines
if [[ $# -lt 1 ]]; then
  echo "incorrect number of argument"
  echo "usage: add_api_variable.sh csv_file"
  echo "To see the help run:"
  echo "usage: add_api_variable.sh -h"
  exit 1
fi
if [[ "$1" == "-h" ]]; then
  echo "Script add variables to the api "
  echo "The input file must be in csv format ; separated and with a comment line for the first line."
  echo "Here is and example:"
  echo "# short_name_of_module api_name_of_variable variable_type variable_fortran_name dim1 ndim readonly get_pos set_pos description"
  echo "t2d;MODEL.TEST1;double;TEST1;0;0;FALSE;NO_POSITION;NO_POSITION;A SIMPLE DOUBLE"
  echo "t2d;MODEL.TEST2;boolean;TEST2;0;0;FALSE;NO_POSITION;NO_POSITION;A SIMPLE BOOLEAN"
  echo "t2d;MODEL.TEST3;integer;TEST3;0;0;FALSE;NO_POSITION;NO_POSITION;A SIMPLE INTEGER"
  exit 1
fi

desc_file=$1
{
read
while IFS=\; read -r module var_name var_type var_inst_name var_dim1 var_ndim var_readonly var_get_pos var_set_pos var_help
do
  add_variable
done
} < $desc_file
