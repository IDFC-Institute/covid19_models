library(EpiModel)

SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and FOI from a rearrangement of Beta * c * D
    lambda <- inf.prob * act.rate * i.num/num
    
    
    dS <- -lambda*s.num
    dE <- lambda*s.num - (1/e.dur)*e.num
    dI <- (1/e.dur)*e.num - rec.rate * i.num
    dR <- rec.rate * i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR),
           # se.flow = lambda * s.num,
           # ei.flow = (1/e.dur) * e.num,
           # ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           # d.flow = cfr*(1/i.dur)*i.num),
         num = num)
  })
}

param <- param.dcm(inf.prob = 0.04,
                   act.rate = 20,
                   rec.rate = 1/15,
                   e.dur = 10)
init <- init.dcm(s.num = 30000000,
                 e.num = 10000,
                 i.num = 5000,
                 r.num = 0)
control <- control.dcm(nsteps = 50,
                       dt = 1,
                       new.mod = SEIR)
mod <- dcm(param, init, control)

print(mod)

plot(mod, y="i.num")

# par(mfrow = c(1, 2))
# plot(mod, y = "i.num", run = 2, main = "Prevalence")
# plot(mod, y = "se.flow", run = 2, main = "Incidence")