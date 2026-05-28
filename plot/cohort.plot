set title "Contributors cohorts"
#set key invert reverse Left outside
set term svg
set output "graphics/cohorts_by_release.svg"
set key autotitle columnheader
set key Left outside
#set key off
set yrange [0:80]
set auto x
unset xtics
set xtics nomirror rotate by -45 scale 0 font ",8"
set style data histogram
set style histogram rowstacked
set style fill solid border -1
set boxwidth 0.75
plot 'data2026/cohort_hist.data' using 2:xtic(1), for [i=3:42] '' using i