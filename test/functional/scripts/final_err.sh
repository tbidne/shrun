echo "output 1"

echo "stderr 1" 1>&2

sleep 1

echo "output 2"

echo "stderr 2" 1>&2

sleep 1

echo "output 3"

echo "stderr 3" 1>&2

sleep 1

exit 1
