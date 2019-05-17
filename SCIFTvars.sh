#!/bin/bash

export SCIFT_HOME="`dirname ${BASH_SOURCE[0]}`"
export PATH=$PATH:$SCIFT_HOME/utils
export PATH=$PATH:$SCIFT_HOME/examples
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SCIFT_HOME/src

