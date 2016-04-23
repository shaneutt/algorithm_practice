#!/bin/bash
DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )
SCALA="$DIR/scala-2.11.7/bin"
lib="$DIR/lib/"
mkdir -p $lib
${SCALA}/scalac -nowarn -d $lib $DIR/src/*
echo -e "#!/bin/bash\n${SCALA}/scala -cp lib/ ShanesDijkstraTests" > $DIR/runtests.sh
chmod +x $DIR/runtests.sh
echo 'done!'
