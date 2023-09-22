set terminal png size 1024,1024
set output 'plot/histogram.png'
set pm3d map

f = "data/contrib_hist"
set xlabel "Number of authored changelog entry"
set ylabel "Number of reviews"
set logscale ycb
set logscale xcb
set logscale zcb
set pm3d interpolate 3,3

set palette rgb 33,13,10
splot f u 1:($2<1?0.5:$2):($3<1?0.5:$3) with pm3d t "Contribution histogram"
