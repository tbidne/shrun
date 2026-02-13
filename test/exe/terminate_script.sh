test_dir=$1

log () {
  echo $1
  echo $1 >> "$test_dir/handler.txt"
}

handler () {
  # When run via shrun, the two backgrounded processes will be reparented.
  # Hence the cleanup here needs to run on the pid.
  log "Killing children: $pid1, $pid2"
  kill -15 $pid1
  kill -15 $pid2
  exit 1
}

# Need to use INT/TERM not SIGINT/SIGTERM, because CI does not recognize the
# latter (the shrun logs reveal the error 'trap: SIGINT: bad trap').
trap handler INT TERM

echo "" > "$test_dir/handler.txt"

log "PID: $$"

sleep 55 &

pid1=$!

sleep 66 &

pid2=$!

log "PID 1: $pid1"
log "PID 2: $pid2"

sleep 22
