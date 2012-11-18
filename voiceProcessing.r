#!/usr/bin/env Rscript

library(tuneR)

"Fourier Transform"
FT <- function(vector)
{
	X <- matrix(nrow = length(vector)/2, ncol = 2)
	colnames(X) <- c("Real", "Imagine")
	for (m in 1:(length(vector)/2))
	{
		Xm <- c()
		cos <- c()
		for (k in 1:length(vector))
		{
			theta = -(2 * pi * k * m)/length(vector)
			Xm_real = vector[k] * cos(theta)
			Xm_img = vector[k] * sin(theta)
			if (is.null(Xm))
				Xm = c(Xm_real, Xm_img)
			else
				Xm = Xm + c(Xm_real, Xm_img)
			cos = c(cos, cos(theta))
		}
		X[m,] = Xm
	}
	return(X)
}

"Magnitude"
Magnitude <- function(matrix)
{
	return(sqrt(matrix[, "Real"]^2 + matrix[, "Imagine"]^2))
}

"Pre-emphasis"
Pre_emphasis <- function(vector) 
{
	ALPHA = 0.935
	s_prime_1 <- 0
	s_prime <- c(s_prime_1)
	for (i in 2:length(vector))
	{
		s_prime_i <- vector[i] - ALPHA * vector[i-1]
		s_prime = c(s_prime, s_prime_i)
	}
	return(s_prime)
}

"Auto-correlation"
Auto_correlation <- function(vector) 
{
	R_SIZE <- 8
	r <- c(sum(vector^2))
	r_mx <- matrix(nrow = R_SIZE + 1, ncol = length(vector))
	r_mx[1,] <- vector
	for (i in 2:(R_SIZE + 1))
	{
		r_mx[i,] <- c(0, r_mx[i-1,])[1:length(vector)]
		r = c(r, sum(r_mx[i,] * r_mx[1,]))
	}
	"A vector of r[r1, r2, ..., rn] retrieved."
	"Suppose A*B = r, where B denotes a vector contains LPC ALPHA constants, and A denotes a matrix constituted by elements in r."
	
	"Generating matrix A"
	A <- matrix(nrow = length(r), ncol = length(r))
	A[1,] <- r
	for (i in 2:length(r))
	{
		A[i,] = c(A[1, i], A[i-1,])[1:length(r)]
	}
	
	"Calculate B"
	"B = r*inverse(A)"
	r_Mx <- matrix(r, ncol = 1, nrow = length(r))
	A_Inv <- solve(A) # You can apply A %*% B to verify
	B <- A_Inv %*% r_Mx
	alpha <- B[,1]
	return(alpha)
}

# play("2273.wav", "vlc") 
wav <-readWave("2273.wav", from = 1, to = Inf, units = "samples", header = FALSE)

"Plot the wave and save it."
jpeg('x.jpg')
plot(wav)
"Very important to use dev.off() command."
dev.off() 
wav <- mono(wav, "left")

"Finding the starting point"
wavTrimed <- prepComb(wav, zero = 1000, where = "start")

"Starting Point of the Voice"
startPt <- 45541
"Offset to locate vowel"
offset <- 5500
freq <- 44100
duration <- 0.020
frameSize <- freq * duration

s1 <- extractWave(wav, from = startPt + offset, to = startPt + offset + frameSize, xunit = "samples")
jpeg('s1.jpg')
plot(s1)
dev.off() 
s1 <- s1@left

s1_FTed = FT(s1)
s1_Mag = Magnitude(s1_FTed)
jpeg('spectual_envelop.jpg')
plot(1:length(s1_Mag), s1_Mag, type = "line", xlab = "Frequency", ylab = "Energy")
dev.off() 

pem_s1 <- Pre_emphasis(s1)
jpeg('pem_s1.jpg')
plot(1:length(pem_s1), pem_s1, type = "line")
dev.off() 

alpha <- Auto_correlation(s1)
print(alpha)
