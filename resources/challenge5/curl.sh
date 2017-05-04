#! /usr/bin/sh

POSITION=0

while [ 1 ]
do
  START=$POSITION
  END=$(($START + 1))
  echo $START $END
  curl --insecure https://52.49.91.111:8443/ghost --http2 -H "Range: bytes=$START-$END" 2> /dev/null >> test4
  POSITION=$(($POSITION + 2))
done

# data:image/png;base64,