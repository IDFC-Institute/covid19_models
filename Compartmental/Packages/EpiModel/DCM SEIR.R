library(EpiModel)

SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and FOI from a rearrangement of Beta * c * D
    ce <- R0 / i.dur           #Girish: no need to have pulled these out, I feel.
    lambda <- ce * i.num/num
    
    dS <- -lambda*s.num
    dE <- lambda*s.num - (1/e.dur)*e.num
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows are part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num,
         ei.prev = (e.num + i.num)/num)
  })
}

param <- param.dcm(R0 = 1.9,
                   e.dur = 10,
                   i.dur = 7,
                   cfr = c(0.5, 0.7, 0.9))
init <- init.dcm(s.num = 1e6,
                 e.num = 10,
                 i.num = 0,
                   r.num = 0,
                   se.flow = 0,
                   ei.flow = 0,
                   ir.flow = 0,
                   d.flow = 0)
control <- control.dcm(nsteps = 500,
                       dt = 1,
                       new.mod = SEIR)
mod <- dcm(param, init, control)

mod

par(mfrow = c(1, 2))
plot(mod, y = "i.num", run = 2, main = "Prevalence")
plot(mod, y = "se.flow", run = 2, main = "Incidence")