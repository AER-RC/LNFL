#!/bin/bash

# run LNFL
./lnfl

# move outputs to directory that can be volume mounted
export OUTDIR=/LNFL/LNFL_Out
mkdir $OUTDIR
mv TAPE3 $OUTDIR
mv TAPE6 $OUTDIR
mv TAPE7 $OUTDIR

exit
