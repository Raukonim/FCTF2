
file="res/MC-L064-T1000-TPAS10-TINC200.res"


set terminal png

set xrange [0:4]
set output "plots/P4_L64_<e>.png"
set title "Monte Carlo Ising 2D L=64 \n <e>"
plot file using 2:4 title "<e>"


set output "plots/P4_L64_<m>.png"
set title "Monte Carlo Ising 2D L=64 \n <m>"
plot file using 2:8 title "<|m|>",\
     file using 2:(sqrt($9)) title "<sqrt(m^2)>"


set output "plots/P4_L64_<c_v>.png"
set title "Monte Carlo Ising 2D L=64 \n <c_v>"
x0=NaN
y0=NaN
plot file using 2:($1*($6)/($2*$2)) title "<c_v>",\
     file using (dx=$2-x0,x0=$2,$2-dx/2):(dy=($4)-y0,y0=($4),dy/dx) title "d<e>/dT"

set output "plots/P4_L64_X.png"
set title "Monte Carlo Ising 2D L=64 \n {/Symbol c}"
plot file using 2:($1*($10)/($2)) title "{/Symbol c}"

