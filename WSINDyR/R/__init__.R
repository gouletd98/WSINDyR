## __init__ - initializes the values to be used

#INPUTS:
  #Polys - monomial powers included in library
  #trigs - sine / cosine frequencies to include in library
  #scale_theta - normalzied columns of theta (0 means no normal, 2 means 12 normalization)
  #ld - sequential thresholding parameter
  #gamma - Tikhonoff regularization parameter

wsindy <- setClass("wsindy", slots = list(polys = "numeric",
                                          trigs = "matrix", scaled_theta = "numeric",
                                          ld = "numeric", gamma = "numeric",
                                          mult_trajectories = "character",
                                          useGLS = "numeric"),
                   prototype = list(polys = seq(0,5,1), trigs = matrix(1,10,10),
                                    scaled_theta = 0, ld = 0.001,
                                    gamma = 10^(-Inf), mult_trajectories = "False",
                                    useGLS = 10^-12))
#NOTE - we may run into issues with trigs matrix and multiple trajectories??

initialize.wsindy <- function(.Object, polys, trigs, scaled_theta,
                              ld, gamma, mult_trajectories) {
  .Object@polys <- polys
  .Object@trigs <- trigs
  .Object@scaled_theta <- scaled_theta
  .Object@ld <- ld
  .Object@gamma <- gamma
  .Object@mult_trajectories <- mult_trajectories
  .Object@useGLS <- useGLS
  return(.Object)
}

#create the initialized values
wsinit <- new("wsindy") #, polys = seq(0,5,1), trigs = matrix(1,10,10),
                  #scaled_theta = 0, ld = 0.001,
                  #gamma = 10^(-Inf), mult_trajectories = "False",
              #useGLS = 10^-12)

