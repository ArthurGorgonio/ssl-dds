set terminal png size 1362,666
set output ARG5;
set key outside;
set key right top;
set xlabel "Batch";
set ylabel ARG3;
set grid;
if (ARG6 eq "1") {
    set yrange [0:1];
} else {
    if (ARG6 eq "2") {
        set yrange [-1:1];
    } else {
        set autoscale y
    }
}
set title ARG4;

purple = "#8500bb";
blue = "#ba8b13";
green = "#00bb7d";

data = "< paste ".ARG1." ".ARG2.";

if (ARG7 eq "1") {
    plot data using 1:5 title 'DyDaSL - N' w l lt rgb purple lw 2, \
    data using 1:($9==1 ? column(5) : NaN) title 'Drift - N' pt 3 ps 1.5 lt rgb purple, \
    data using 1:6 title 'DyDaSL - FT' w l lt rgb blue lw 2, \
    data using 1:($10==1 ? column(6) : NaN) title 'Drift - FT' pt 4 ps 1.5 lt rgb blue, \
    data using 1:7 title 'DyDaSL - W' w l lt rgb green lw 2, \
    data using 1:($11==1 ? column(7) : NaN) title 'Drift - W' pt 6 ps 1.5 lt rgb green
} else {
    if (ARG7 eq "2") {
        plot data using 1:2 title 'DyDaSL - N' w l lt rgb purple lw 2, \
        data using 1:3 title 'DyDaSL - FT' w l lt rgb blue lw 2, \
        data using 1:4 title 'DyDaSL - W' w l lt rgb green lw 2
    } else {
        plot data using 1:5 title 'DyDaSL - N' w lp lt rgb purple lw 2 pt 3, \
        data using 1:6 title 'DyDaSL - FT' w lp lt rgb blue lw 2 pt 4, \
        data using 1:7 title 'DyDaSL - W' w lp lt rgb green lw 2 pt 6
    }
}
