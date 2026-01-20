#!/usr/bin/gnuplot -persist
# Gnuplot script to generate a contoured heatmap

# Set terminal and output file
set terminal pngcairo enhanced size 1024, 768
set output 'heatmap_from_csv.png'

# Set title, labels, and color palette
set title "Heatmap from 3D Data Points"
set xlabel "X-axis"
set ylabel "Y-axis"
set cblabel "Z-axis (Intensity)"
set palette defined (0 "blue", 1 "green", 2 "yellow", 3 "red")

# Set colorbox
set colorbox
set colorbox border

# Configure heatmap appearance
set view map
unset surface
set pm3d at b

# Set grid and key settings
set grid
unset key

set dgrid3d 5 ,5 splines

# Set datafile separator to a comma
set datafile separator ","

# Plot heatmap
splot 'test.csv' using 1:2:3 with pm3d