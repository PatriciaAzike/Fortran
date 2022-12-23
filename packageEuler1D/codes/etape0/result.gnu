reset
#set nokey
set style data linesp
set xlabel' abscissa (m)'

set title 'Pressure (Bar)'
plot 'PLOT' u 1:($3*1e-5) pn -20
pause -1

set title 'Velocity in x-direction (m/s)'
plot 'PLOT' u 1:2 pn -20
pause -1

set title 'Temperature (K)'
plot 'PLOT' u 1:4 pn -20
pause -1

set title 'Density (Kg/m^3)'
plot 'PLOT' u 1:5 pn -20
pause -1

set title 'Speed of sound'
plot 'PLOT' u 1:6  pn -20
pause -1


set title 'Mach number (-)'
plot 'PLOT' u 1:($2/$6)
pause -1


