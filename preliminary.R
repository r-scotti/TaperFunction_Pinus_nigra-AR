data0 <-read.csv("pn4TapeR.csv")

library(sqldf)
# to verify profile shapes:
# join each section with previous section
ps <- sqldf("select a.*, b.Dx p_Dx, max(b.Hx) p_Hx from data0 a left join data0 b on a.Id=b.Id and b.Hx<a.Hx group by a.Id, a.Hx")
# compute tapering on last section two sections
ps2 <- sqldf("select a.*, b.p_Dx p2_Dx, b.p_Hx p2_Hx from ps a join ps b on a.Id=b.Id and a.p_HX=b.Hx where a.Dx=0")
ps2$lastTap <- with(ps2, (p_Dx-Dx)/(Hx-p_Hx))
ps2$s_lastTap <- with(ps2, (p2_Dx-p_Dx)/(p_Hx-p2_Hx))

OutOfRangeTapering <- 3.5  ## including profiles exceeding this value TapeR does not converge!
library(lattice)
xyplot(Dx ~ Hx | Id, data0[data0$Id %in% ps2$Id[ps2$lastTap<OutOfRangeTapering],])
xyplot(Dx ~ Hx | Id, data0[data0$Id %in% ps2$Id[ps2$lastTap>OutOfRangeTapering],])
