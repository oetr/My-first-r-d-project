bin(x,s) = s*int(x/s) 
binc(x,s) = s*(int(x/s)+0.5)

set boxwidth 1

set datafile separator ","
#plot "ValueSim-2011-1-24-23-11-24.txt" u 25 w l
plot "ValueSim-2011-2-1-21-15-42.txt" u 3:(0.25*rand(0) - 0.35), "" u (0.25*rand(0)-0.35):4 smooth frequency with boxes
#u (bin(4,0.1)):(1./(0.1*1000)) smooth frequency with boxes
#

#3:4:(0.25*rand(0)-0.35)