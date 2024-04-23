#!/bin/bash

# set iterations
if [ -z "$2" ]; then
  echo '1' > .iterations
else
  echo $2 > .iterations
fi

#SBT_OPTS="-Xms2G -Xmx8G" sbt "testOnly apps.autotuning.$1"
sbt "testOnly apps.autotuning.$1"
#sbt "testOnly apps.autotuning.asumTuning"
#sbt "testOnly apps.autotuning.harrisTuning"
#sbt "testOnly apps.autotuning.kmeansTuning"
#sbt "testOnly apps.autotuning.mmCPU"
#sbt "testOnly apps.autotuning.mmTuning"
#sbt "testOnly apps.autotuning.scalTuning"
#sbt "testOnly apps.autotuning.stencilTuning"
