#!/bin/bash

# we'll write all git versions of the file to this folder:
EXPORT_TO=/tmp/all_versions_exported

# take relative path to the file to inspect
GIT_PATH_TO_FILE=$1

# ---------------- don't edit below this line --------------

USAGE="Please cd to the root of your git proj and specify path to file you with to inspect (example: $0 some/path/to/file)"

# check if got argument
if [ "${GIT_PATH_TO_FILE}" == "" ]; then
    echo "error: no arguments given. ${USAGE}" >&2
    exit 1
fi

# check if file exist
if [ ! -f ${GIT_PATH_TO_FILE} ]; then
    echo "error: File '${GIT_PATH_TO_FILE}' does not exist. ${USAGE}" >&2
    exit 1
fi

# extract just a filename from given relative path (will be used in result file names)
GIT_SHORT_FILENAME=$(basename $GIT_PATH_TO_FILE)

# create folder to store all revisions of the file
if [ ! -d ${EXPORT_TO} ]; then
    echo "creating folder: ${EXPORT_TO}"
    mkdir ${EXPORT_TO}
fi

## uncomment next line to clear export folder each time you run script
#rm ${EXPORT_TO}/*

# iterate all revisions
git log --pretty=format:"%h%x09%an%x09%ad%x09%s" --all --full-history -- ${GIT_PATH_TO_FILE} \
    cut -d ' ' -f1 | \
while read h; do \
    COMMIT_HASH=`echo $h | awk '{print $1}' `; \
    COMMIT_DATE=`echo $h | awk '{print $4"-"$5"-"$7}' `; \
    git cat-file -p ${COMMIT_HASH}:${GIT_PATH_TO_FILE} > ${EXPORT_TO}/${COMMIT_DATE}.${GIT_SHORT_FILENAME};\
done    

# return success code
echo "result stored to ${EXPORT_TO}"
exit 0