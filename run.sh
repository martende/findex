#export JAVA_OPTS="-Xms100m -Xmx1024m"
#scala -cp ./target/scala-2.10/classes:./lib/optional_2.10-0.3.jar:./lib/optional_2.8.0-0.2.jar:./lib/caliper.jar:./lib/sais.jar:$HOME/.sbt/boot/scala-2.10.0/lib/scala-library.jar:$HOME/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.2.1.jar:$HOME/.ivy2/cache/commons-io/commons-io/jars/commons-io-1.3.2.jar:$HOME/.ivy2/cache/com.github.scala-incubator.io/scala-io-core_2.10/jars/scala-io-core_2.10-0.4.2.jar:$HOME/.ivy2/cache/com.jsuereth/scala-arm_2.10/jars/scala-arm_2.10-1.3.jar:$HOME/.ivy2/cache/com.github.scala-incubator.io/scala-io-file_2.10/jars/scala-io-file_2.10-0.4.2.jar -J-Xms1024m -J-Xmx1024m org.fmindex.FMCreatorApp --file /usr/include --file testdata/include.bwt
#CP="/data_nb/aleh/MProg/findex/target/scala-2.10/classes:/data_nb/aleh/MProg/findex/lib/sais.jar:/data_nb/aleh/MProg/findex/lib/caliper.jar:/data_nb/aleh/MProg/findex/lib/optional_2.10-0.3.jar:/home/aleh/.sbt/boot/scala-2.10.0/lib/scala-library.jar:/home/aleh/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.2.1.jar:/home/aleh/.ivy2/cache/commons-io/commons-io/jars/commons-io-1.3.2.jar:/home/aleh/.ivy2/cache/com.github.scala-incubator.io/scala-io-core_2.10/jars/scala-io-core_2.10-0.4.2.jar:/home/aleh/.ivy2/cache/com.jsuereth/scala-arm_2.10/jars/scala-arm_2.10-1.3.jar:/home/aleh/.ivy2/cache/com.github.scala-incubator.io/scala-io-file_2.10/jars/scala-io-file_2.10-0.4.2.jar"
CP="./target/scala-2.10/classes:./lib/optional_2.10-0.3.jar:./lib/optional_2.8.0-0.2.jar:./lib/caliper.jar:./lib/sais.jar:$HOME/.sbt/boot/scala-2.10.0/lib/scala-library.jar:$HOME/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.2.1.jar:$HOME/.ivy2/cache/commons-io/commons-io/jars/commons-io-1.3.2.jar:$HOME/.ivy2/cache/com.github.scala-incubator.io/scala-io-core_2.10/jars/scala-io-core_2.10-0.4.2.jar:$HOME/.ivy2/cache/com.jsuereth/scala-arm_2.10/jars/scala-arm_2.10-1.3.jar:$HOME/.ivy2/cache/com.github.scala-incubator.io/scala-io-file_2.10/jars/scala-io-file_2.10-0.4.2.jar"

scala -cp $CP -J-Xms1024m -J-Xmx1024m \
-J-Dcom.sun.management.jmxremote=true \
-J-Dcom.sun.management.jmxremote.port=20000 \
-J-Dcom.sun.management.jmxremote.ssl=false \
-J-Dcom.sun.management.jmxremote.authenticate=false \
org.fmindex.IndexerApp --dir fmindexV2/a1 -i 1 --merge-debug-level 3

