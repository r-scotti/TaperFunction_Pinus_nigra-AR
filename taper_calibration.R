data0 <-read.csv("pn4TapeR.csv")

library(sqldf)
# Remove data redundancy and shape as traditional mesurement tally.
# Extract basic info
##   dbh and h_tot are normally measured independently from profile mesurements
##   and have to be unique values
Trees <- sqldf("select Id, a.Dx dbh, b.Hx h_tot from 
               (select * from data0 where Hx=1.3) a
               join (select * from data0 where Dx=0) b
               using (Id)")
ntUniqeId   <- sqldf("select Id from Trees natural join (select Id, count(*) n from Trees group by Id having n>1)")
if(nrow(ntUniqeId)>0) {print(ntUniqeId); stop("Tree Id should be unique! Stopping.")}

# Separate profiles info
Profiles <- sqldf("select Id, Dx, Hx from data0 where Hx<>1.3 and Dx>0") 
##   when Dx=0 then Hx=h_tot, this is a spurious section!
##   check that all Profiles are connected to a tree
profWOtrees <- sqldf("select distinct Id from Profiles except select Id from Trees")
if(nrow(profWOtrees)>0) {print(profWOtrees); stop("Profile Id without Tree! Stopping.")}

# fit the model (after preliminary.R)
#pset <- sqldf("select a.* from Profiles a")
pset <- sqldf(paste("select a.* from Profiles a join ps2 using(Id) where lastTap <",OutOfRangeTapering))
ttab <- sqldf("select a.*, b.h_tot from pset a join Trees b using(Id)")
library(TapeR)
# define the relative knot positions and order of splines
knt_x = c(0.0, 0.1, 0.75, 1.0); ord_x = 4 # B-Spline knots: fix effects; order (cubic = 4)
knt_z = c(0.0, 0.1, 1.0); ord_z = 4 # B-Spline knots: rnd effects
tpm <- with(ttab, TapeR_FIT_LME.f(Id, Hx/h_tot, Dx, knt_x, ord_x, knt_z, ord_z, IdKOVb = "pdSymm"))
tp.par <- tpm$par.lme
