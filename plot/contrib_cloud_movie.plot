set term png size 1024,1024

version=system("echo $OCAML_VERSION")
time=system("echo $OCAML_TIME")

output_name=sprintf("movie_graphics/contributions_%s.png",time)

set output output_name

d = sprintf("movie_data/contributions_at_time_%s.data",time)
set xlabel "number of authored contributions"
set ylabel "number of reviews"

ymax = 100
yzero = 0.7
xmax = 100
xzero = 0.7

set yrange [0.5:(1+log10(ymax))]
set xrange [0.5:(1+log10(xmax))]

unset ytics
set ytics 1 add ("0" yzero, "1" 1)
set for [i=2:(1+log10(ymax+1))] ytics add (sprintf("%g",10**(i-1)) i) # Add major tics
set for [i=1:(1+log10(ymax+1))] for [j=2:9] ytics add ("" log10(10**i*j) 1) # Add minor tics

unset xtics
set xtics 1 add ("0" xzero, "1" 1)
set for [i=2:(1+log10(xmax+1))] xtics add (sprintf("%g",10**(i-1)) i) # Add major tics
set for [i=1:(1+log10(xmax+1))] for [j=2:9] xtics add ("" log10(10**i*j) 1) # Add minor tics

set grid
set grid mxtics mytics
set k o
set k bottom left

f(x) = x

set title sprintf("Contributions and reviews by contributors for %s",version)
plot d u ($1<1?xzero+(1-xzero)*$1:1 + log10($1)):($2<1?yzero+(1-yzero)*$2:1 + log10($2)):3 t "" \
w points linecolor variable pointtype 7 pointsize 3,\
f(x) w lines t ""
