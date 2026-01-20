#!/usr/bin/gnuplot -persist
set view map
set yrange [0:30]
set xrange [0:30]
set dgrid3d 100,100,4
set contour base
splot 'test.csv' u 1:2:3 w pm3d