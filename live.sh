#! /usr/bin/env nix-shell
#! nix-shell -i bash -p  inotify-tools bc stdenv psmisc

waitForPort() {
  backoff=0.05
  first=""
  while ss -tulwnH -A tcp | grep 0.0.0.0:8000 >/dev/null; do
    [[ -z "$first" ]] && echo "waiting for socket to be closed"
    first="false"
    sleep $backoff
    backoff=$(bc <<< "$backoff * 1.5")
  done
}
run() {
  cd src
  runhaskell Main.hs -d &
  cd ..
}
if ss -tulwnH -A tcp | grep 0.0.0.0:8000 >/dev/null; then
  read -p "kill process using port 8000 (y/n)? " -n 1 -r
  if [[ $REPLY =~ ^[Yy]$ ]]
  then
    fuser -k 8000/tcp
  fi
fi
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
