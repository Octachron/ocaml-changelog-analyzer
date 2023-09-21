set terminal png
set output 'histogram.png'
set pm3d map

f = "data/contrib_hist"
set xlabel "Number of authored changelog entry"
set ylabel "Number of reviews"
set logscale ycb
set logscale xcb
set logscale zcb
set pm3d interpolate 3,3

set palette rgb 33,13,10
splot f u 1:(0.1+$2):(0.1+$3) with pm3d t "Contribution histogram"
