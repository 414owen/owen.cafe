#!/usr/bin/env bash

versions="manual switchy table table_equiv direct"
compilers="gcc clang"
opts="s 0 1 2 3"


for opt in $opts; do
  data="O${opt}.dat"
  svg="O${opt}.svg"
  > "$data"

  for compiler in $compilers; do
    >/dev/null make clean
    >/dev/null make CC="$compiler" CFLAGS="-O$opt -s"
  done

  for version in $versions; do
    size=$(stat -c%s ${version}.o)
    echo "$version $size"
  done > "$data"

  gnuplot <<EOF
set terminal svg size 900,500 background rgb "#111827"
set output "$svg"

set style data histograms
set style histogram clustered gap 1
set style fill solid
set boxwidth 0.7

set grid ytics lc rgb "#374151"
set border lc rgb "#9ca3af"
set tics textcolor rgb "#e5e7eb"
set title "-O$opt" textcolor rgb "#f9fafb"

plot "$data" using 2:xtic(1) title "gcc" lc rgb "#60a5fa", \
     "" using 3 title "clang" lc rgb "#f472b6"
EOF

done
