logfile="ValueSim-2011-3-28-16-41-54.txt"
data <- read.table(file(logfile), sep=",")
world_size <- 30
number_of_runs <- length(data$V1);

#par(mfrow=c(1,2)) # split the plot into 3x2 rows*cols
#plot(data$V3, data$V4, type="l", pch=21, col="blue", ylab="Y", xlab="X", xlim=c(0,world_size), ylim=c(0,world_size))
#plot( 1:number_of_runs, data$V3, type="l", pch=21, col="cyan", lty=1, ylab="Y", xlab="number of runs", lwd=3)
#plot( 1:number_of_runs, data$V4, type="l", pch=21, col="green", lty=1, ylab="Y", xlab="number of runs", lwd=2)
#plot(1:number_of_runs, data$V6	, type="l", pch=21, col="blue", lty=1, ylab="energy", xlab="number of runs", lwd=1)
plot(1:number_of_runs, data$V48, type="l", pch=20, col="red", lty=1, ylab="Utility", xlab="n", lwd=2, ylim=c(0, 1))
lines(1:number_of_runs, data$V6/100, type="l", pch=21, col="blue", lty=1, ylab="Energy", xlab="n", lwd=1)
lines(1:number_of_runs, (data$V47 + data$V46 + data$V45 + data$V44) / 20, type="l", pch=21, col="orange", lty=1, ylab="Proximity", xlab="X", lwd=1)
lines(1:number_of_runs, (data$V8 + data$V9 + data$V10 + data$V11 + data$V12 + data$V13 + data$V14 + data$V15 + data$V16) / 225, type="l", pch=21, col="cyan", lty=1, ylab="Temperature", xlab="n", lwd=1)
lines(1:number_of_runs, data$V7 / 25, type="l", pch=21, col="yellow", lty=1, ylab="InternalTemperature", xlab="n", lwd=1)
legend(number_of_runs/2, 0.3, c("utility", "energy", "proximity","temperature", "internal temperature"), col=c("red","blue","orange","cyan", "yellow"), text.col = "green4", lty=c(1,1,1,1), lwd=c(2,1,1,1), cex=1)

#plot(1:number_of_runs, data$V3	, type="l", pch=21, col="blue", lty=1, ylab="x and y", xlab="number of runs", lwd=1)
#lines(1:number_of_runs, data$V4	, type="l", pch=21, col="blue", lty=1)
#plot(1:number_of_runs, data$V3, type="l", pch=21, col="blue", ylab="X and Y", xlab="n", ylim=c(0,world_size))
#lines(1:number_of_runs, data$V4, type="l", pch=21, col="red", xlab="n", ylim=c(0,world_size))
#legend(number_of_runs/2, world_size/3, c("x", "y"), col=c("blue","red"), text.col = "green4", lty=c(1,1), lwd=c(1,1), cex=1)