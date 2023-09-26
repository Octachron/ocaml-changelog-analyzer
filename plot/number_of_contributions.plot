set term svg
set output "graphics/number_of_contributions_by_release.svg"
f="data/total_contribution_history_filtered.data"
set xtics rotate
set yrange [0:300]
set title "Number of changelog entries by release"
plot f u 0:2:xtic(1) w lp pt 7 t ""
