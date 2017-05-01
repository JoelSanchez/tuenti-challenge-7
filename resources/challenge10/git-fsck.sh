#!/bin/bash

#
# Modified version of:
# http://stackoverflow.com/questions/12850030/git-getting-all-previous-version-of-a-specific-file-folder
# for the purposes of the challenge
# 

# we'll write all git versions of the file to this folder:
EXPORT_TO=/tmp/all_versions_exported_fsck

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
git fsck --full --no-reflogs --unreachable --lost-found | \
while read h; do \
    COMMIT_HASH=`echo $h | awk '{print $3}' `; \
    COMMIT_DATE=`git show $COMMIT_HASH --date=iso | grep 'Date:' | awk '{print $2}'`; \
    echo $COMMIT_HASH; \
    echo 'DATE';
    echo $COMMIT_DATE; \
    git cat-file -p ${COMMIT_HASH}:${GIT_PATH_TO_FILE} > ${EXPORT_TO}/${COMMIT_DATE}.${GIT_SHORT_FILENAME};\
done    

# return success code
echo "result stored to ${EXPORT_TO}"
exit 0