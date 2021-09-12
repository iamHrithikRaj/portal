#!/bin/bash
# portal.sh


# $@ takes all arguments of the shell script
# and passes it along to `portal-exe`
# which is our tool
function portal(){
    cd $HOME/.cabal/bin
    OUTPUT=`./portal-exe $@`
    # return code 2 tells the shell
    # script to cd to whatever `teleport` outputs
    if [ $? -eq 2 ]
    then 
        cd $OUTPUT
    else
        echo $OUTPUT
    fi
}
