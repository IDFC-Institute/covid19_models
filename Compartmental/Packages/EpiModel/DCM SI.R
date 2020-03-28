#http://statnet.org/tut/BasicDCMs.html

library(EpiModel)

param <- param.dcm(inf.prob = 0.2, act.rate = 0.25)
init <- init.dcm(s.num = 500, i.num = 1)
control <- control.dcm(type = "SI", nsteps = 500)
mod <- dcm(param, init, control)
plot(mod)