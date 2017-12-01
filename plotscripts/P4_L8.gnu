
file="res/MC-L008-*.res"


set terminal png

set xrange [0:4]
set output "plots/P4_L8_<e>.png"
set title "Monte Carlo Ising 2D L=8 \n <e>"
plot file using 2:4 title "<e>"


set output "plots/P4_L8_<m>.png"
set title "Monte Carlo Ising 2D L=8 \n <m>"
plot file using 2:8 title "<|m|>",\
     file using 2:11 title "<sqrt(m^2)>"

set yrange [0:]
set output "plots/P4_L8_<c_v>.png"
set title "Monte Carlo Ising 2D L=8 \n <c_v>"
x0=NaN
y0=NaN
plot file using 2:12 title "<c_v>",\
     file using 2:13 title "d<e>/dT" w lines
set yrange [*:*]

set output "plots/P4_L8_X.png"
set title "Monte Carlo Ising 2D L=8 \n {/Symbol c}"
plot file using 2:14 title "{/Symbol c}"
