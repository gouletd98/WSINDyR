# __init__.R
# initializes the values to be used

# INPUTS...
# polys: monomial powers included in library
# trigs: sine / cosine frequencies to include in library
# scale_theta: normalzied columns of theta (0 means no normal, 2 means 12 normalization)
# ld: sequential thresholding parameter
# gamma: Tikhonoff regularization parameter
# mult_trajectories:
# useGLS:

wsindy <- setClass("wsindy", slots = list(polys = "numeric",
                                          trigs = "list", scaled_theta = "numeric",
                                          ld = "numeric", gamma = "numeric",
                                          mult_trajectories = "character",
                                          useGLS = "numeric"),
                   prototype = list(polys = seq(0,5,1), trigs = list(),
                                    scaled_theta = 0, ld = 0.05,
                                    gamma = 10^(-Inf), mult_trajectories = "False",
                                    useGLS = 10^-12))

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
wsinit <- new("wsindy")
