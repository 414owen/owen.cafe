waitForPort() {
  backoff=0.05
  echo "waiting for socket to be closed"
  while ss -tulwnH -A tcp | grep 0.0.0.0:8000 >/dev/null; do
    sleep $backoff
    backoff=$(bc <<< "$backoff * 1.5")
  done
}
run() {
  cd src
  runhaskell Main.hs -d &
  cd ..
}
waitForPort
run
sep="-------------------"
while inotifywait -r -e modify -e create -e delete src 2> /dev/null; do
  echo $sep
  echo $sep
  echo $sep
  clear
  fuser -k 8000/tcp
  waitForPort
  run
done
