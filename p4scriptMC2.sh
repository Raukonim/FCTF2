#!/bin/bash

gfortran -O3 -o MC2.exe MC2.f
./MC2 "data/MC2-L008.dat"
gnuplot plotscripts/P4_L8.gnu
./MC2 "data/MC2-L016.dat"
gnuplot plotscripts/P4_L16.gnu
./MC2 "data/MC2-L032.dat"
gnuplot plotscripts/P4_L32.gnu
./MC2 "data/MC2-L064.dat"
gnuplot plotscripts/P4_L64.gnu
./MC2 "data/MC2-L128.dat"
gnuplot plotscripts/P4_L128.gnu
