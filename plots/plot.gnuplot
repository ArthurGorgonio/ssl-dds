set terminal png size 1362,666;
set output ".ARG5.";
set key outside;
set key right top;
set xlabel "Batch";
set ylabel ARG3
set grid;
if (ARG6 eq "1") {
    set yrange [0:1];
} else {
    if (ARG6 eq "2") {
        set yrange [-1:1];
    } else {
        set autoscale y;
    }
}
set title ARG4

purple = "#8500bb";
yellow = "#ba8b13";
green = "#00bb7d";
blue = "#0A9EC7";
red = "#FA5025";

data = "< paste ".ARG1." ".ARG2."

if (ARG7 eq "1") {
    plot data using 1:7 title 'CPSSDS' w l lt rgb red lw 2, \
    data using 1:($13==1 ? column(7) : NaN) title 'Drift - C' pt 5 ps 1.5 lt rgb red, \
    data using 1:8 title 'Hinkley' w l lt rgb blue lw 2, \
    data using 1:($14==1 ? column(8) : NaN) title 'Drift - H' pt 7 ps 1.5 lt rgb blue, \
    data using 1:10 title 'DyDaSL - FT' w l lt rgb yellow lw 2, \
    data using 1:($16==1 ? column(10) : NaN) title 'Drift - FT' pt 4 ps 1.5 lt rgb yellow, \
    data using 1:9 title 'DyDaSL - N' w l lt rgb purple lw 2, \
    data using 1:($15==1 ? column(9) : NaN) title 'Drift - N' pt 3 ps 1.5 lt rgb purple, \
    data using 1:11 title 'DyDaSL - W' w l lt rgb green lw 2, \
    data using 1:($17==1 ? column(11) : NaN) title 'Drift - W' pt 6 ps 1.5 lt rgb green
} else {
    if (ARG7 eq "2") {
        plot data using 1:2 title 'CPSSDS' w l lt rgb red lw 2, \
        data using 1:3 title 'Hinkley' w l lt rgb blue lw 2, \
        data using 1:5 title 'DyDaSL - FT' w l lt rgb yellow lw 2, \
        data using 1:4 title 'DyDaSL - N' w l lt rgb purple lw 2, \
        data using 1:6 title 'DyDaSL - W' w l lt rgb green lw 2
    } else {
        plot data using 1:7 title 'CPSSDS' w lp lt rgb red lw 2, \
        data using 1:8 title 'Hinkley' w lp lt rgb blue lw 2, \
        data using 1:10 title 'DyDaSL - FT' w lp lt rgb yellow lw 2, \
        data using 1:9 title 'DyDaSL - N' w lp lt rgb purple lw 2, \
        data using 1:11 title 'DyDaSL - W' w lp lt rgb green lw 2
    }
}
