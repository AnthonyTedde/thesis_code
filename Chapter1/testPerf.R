# test perf of function:
n <- 10^5
begin <- Sys.time()
test <- rep(1:4, n)
end <- Sys.time()
totalRep <- end - begin


begin <- Sys.time()
test <- rep.int(1:4, n)
end <- Sys.time()
totalRep.int <- end - begin

# according to documentation rep.int should be faster than rep.