library(EpiModel)

param <- param.dcm(inf.prob = 0.05,         # beta = probability of transmission
                   act.rate = 20,           # c = rate of contact
                   rec.rate = (1/15)        # R = recovery rate
                   #inf.prob.g2 = 0.06,
                   #rec.rate.g2 = (1/24),
                   #balance = "g1"
                   )

init <- init.dcm(s.num = 30000000,          # Susceptible
                 i.num = 33,                # Infected
                 r.num = 0                  # Removed
                 #s.num.g2 = 120000,
                 #i.num.g2 = 10,
                 #r.num.g2 =1
                 )

control <- control.dcm(type = "SIR",
                       nsteps = 20
                       )

mod <- dcm(param, init, control)

print(mod)

plot(mod)