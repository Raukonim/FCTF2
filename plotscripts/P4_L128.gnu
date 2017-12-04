
file="res/MC-L128-T1500-TPAS100-TINC020.res"


set terminal pdfcairo

set xrange [1.5:3]
set xlabel "Temperatura reduida"

set output "plots/P4_L128_e.pdf"
set title "Monte Carlo Ising 2D L=128 \n <e>"
set ylabel "Energia"
plot file using 2:4 title "<e>" pt 7 ps 0.5 lc 7


set output "plots/P4_L128_m.pdf"
set title "Monte Carlo Ising 2D L=128 \n <m>"
set ylabel "Magnetització"
plot file using 2:8 title "<|m|>" pt 7 ps 0.5 lc 7,\
     file using 2:11 title "<sqrt(m^2)>" pt 7 ps 0.5 lc 1


set yrange [0:]
set output "plots/P4_L128_c_v.pdf"
set title "Monte Carlo Ising 2D L=128 \n <c_v>"
set ylabel "Capacitat calorifica"
x0=NaN
y0=NaN
plot file using 2:12 title "<c_v>" pt 7 ps 0.5 lc 7,\
     file using 2:13 title "d<e>/dT" w lines
set yrange [*:*]


set output "plots/P4_L128_X.pdf"
set title "Monte Carlo Ising 2D L=128 \n {/Symbol c}"
set ylabel "Susceptibilitat"
plot file using 2:14 title "{/Symbol c}" pt 7 ps 0.5 lc 7
