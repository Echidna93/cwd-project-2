data<-read.csv("sim.csv")

par(mfrow = c(2, 2))
plot(data$time, data$S, pch=1, type="l")
plot(data$time, data$I, pch=1, type="l")
plot(data$time, data$E, pch=1, type="l")