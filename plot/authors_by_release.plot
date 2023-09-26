set term svg
set output "graphics/authors_by_release.svg"
f="data/authors_by_release.data"
set xtics rotate
set yrange [0:80]
set grid xtics ytics mytics mxtics
set title "Number of change authors by OCaml release"
plot f u 0:2:xtic(1) w lp pt 7 t "authors",\
f u 0:3 w lp pt 7 t "new authors"
