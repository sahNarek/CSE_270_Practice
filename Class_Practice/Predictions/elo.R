library(elo)
elo.prob(elo.A = 1500, elo.B = 1500)
elo.prob(elo.A = 1900, elo.B = 1500)
odds <- elo.prob(elo.A = 1900, elo.B = 1500) / (1 - elo.prob(elo.A = 1900, elo.B = 1500))

elo.prob(elo.A = 1500, elo.B = 1600)
1500 + 20 * (1 - elo.prob(elo.A = 1500, elo.B = 1600))

elo.prob(elo.A = 1600, elo.B = 1500)
1600 + 20 * (0 - elo.prob(elo.A = 1600, elo.B = 1500))

elo.calc(elo.A = 1500, elo.B = 1600, wins.A = 1, k = 20)
elo.update(elo.A = 1500, elo.B = 1600, wins.A = 1, k = 20)
