set view map
set style data linespoints
set xtics border in scale 0,0 mirror norotate  offset character 0, 0, 0
set ytics border in scale 0,0 mirror norotate  offset character 0, 0, 0
set ztics border in scale 0,0 nomirror norotate  offset character 0, 0, 0
#set nocbtics
set title "Temperature Map of a 30x30 Environment"
set xrange [ -0.5 : 29.5 ] noreverse nowriteback
set yrange [ -0.5 : 29.5 ] noreverse nowriteback
set zrange [ * : *  ] noreverse nowriteback
set cblabel "Temperature"
set cbrange [ * : * ] noreverse nowriteback
set palette model RGB
set palette defined ( 0 "black", 1 "white" )
#set palette rgbformulae 34,35,36
splot "temperature-map.dat" with image