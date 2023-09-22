set term png size 1024,1024
set output "plot/cloud.png"
d = "data/contributions"
set xlabel "number of authored contributions"
set ylabel "number of reviews"
set log x

ymax = 1000
yzero = 0.7
set yrange [0.5:(1+log10(ymax))]
unset ytics
set ytics 1 add ("0" yzero, "1" 1)
set for [i=2:(1+log10(ymax+1))] ytics add (sprintf("%g",10**(i-1)) i) # Add major tics
set for [i=1:(1+log10(ymax+1))] for [j=2:9] ytics add ("" log10(10**i*j) 1) # Add minor tics

set log x
set grid
set grid mxtics mytics
set k o
set k bottom left
set title "Contributions and reviews by contributors"
plot d u 2:($3<1?yzero:1 + log10($3)) t "" \
w points pointtype 7 pointsize 3
