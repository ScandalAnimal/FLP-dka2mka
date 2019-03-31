#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

OUTPUTS='./automates/outputs/'
ERR='./automates/err/'

#Create directory with outputs
if [[ -d ${OUTPUTS} ]]; then
    rm -r ${OUTPUTS}
fi

if [[ -d ${ERR} ]]; then
    rm -r ${ERR}
fi


mkdir ${OUTPUTS}
mkdir ${ERR}

# echo "=======TESTS: -i parameter======="

# # Run tests with -i parameter
# for input_file in ./automatas/inputs/*.in; do
#     ./dka-2-mka -i $input_file >> ./automatas/outputs/${input_file##*/}".i"
# done

# # Compare outputs with expected outputs
# for input_file in ./automatas/inputs/*.in; do
#     diff $input_file ./automatas/outputs/${input_file##*/}".i" >> 
# done


echo "=======TESTS: -t input_file======="

# Run tests with -i parameter
for input_file in ./automates/inputs/*; do

    fn_without_ext=$(echo ${input_file} | cut -d "/" -f 4 | cut -d "." -f 1)

    ./dka-2-mka -t $input_file >> ${OUTPUTS}/${fn_without_ext}".out" 2> /dev/null
done

# Compare outputs with expected outputs
for input_file in ./automates/valid_outputs/*; do
    fn_without_ext=$(echo ${input_file} | cut -d "/" -f 4 | cut -d "." -f 1)
    

    diff $input_file ./automates/outputs/${fn_without_ext}".out" >> ${ERR}${fn_without_ext}".err"
    if [[ ${?} == 0 ]]; then
        echo -e "[${GREEN}SUCCESS${NC}] ${fn_without_ext}"
    else
        echo -e "[${RED}FAIL${NC}] ${fn_without_ext}"
    fi
done