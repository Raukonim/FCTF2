
file="res/MC-L016-T1500-TPAS050-TINC040.res"


set terminal png

set xrange [0:4]
set output "plots/P4_L16_<e>.png"
set title "Monte Carlo Ising 2D L=16 \n <e>"
plot file using 2:4 title "<e>"


set output "plots/P4_L16_<m>.png"
set title "Monte Carlo Ising 2D L=16 \n <m>"
plot file using 2:8 title "<|m|>",\
     file using 2:11 title "<sqrt(m^2)>"


set output "plots/P4_L16_<c_v>.png"
set title "Monte Carlo Ising 2D L=16 \n <c_v>"
x0=NaN
y0=NaN
plot file using 2:12 title "<c_v>",\
     file using 2:13 title "d<e>/dT"

set output "plots/P4_L16_X.png"
set title "Monte Carlo Ising 2D L=16 \n {/Symbol c}"
plot file using 2:14 title "{/Symbol c}"

