#!/bin/bash

build="$1"
if [ -z "$build" ]
then
    echo "### ERROR ### Build directory required as first argument (e.g., build_gfortran)"
    return 1
fi

export SCIFT_HOME="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/$build"
export PATH=$PATH:$SCIFT_HOME/utils
export PATH=$PATH:$SCIFT_HOME/examples
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SCIFT_HOME/src

return 0
