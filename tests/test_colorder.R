## test column order options of oa.design
require(DoE.base)

P3.3(oa.design(L18, nlevels=c(2,3,3,3),columns="order"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3),columns="min3"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3),columns="min34"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3),columns="minRPFT"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3),columns="minRelProjAberr"), rela=TRUE)


P3.3(oa.design(L18, nlevels=c(2,3,3,3,3),columns="min3"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3),columns="min34"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3),columns="minRPFT"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3),columns="minRelProjAberr"), rela=TRUE)

P3.3(oa.design(L18, nlevels=c(3,3,2,3,3),columns="min3"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(3,3,2,3,3),columns="min34"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(3,3,2,3,3),columns="minRPFT"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(3,3,2,3,3),columns="minRelProjAberr"), rela=TRUE)

P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3),columns="min3"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3),columns="min34"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3),columns="minRPFT"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3),columns="minRelProjAberr"), rela=TRUE)

P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3,3),columns="min3"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3,3),columns="min34"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3,3),columns="minRPFT"), rela=TRUE)
P3.3(oa.design(L18, nlevels=c(2,3,3,3,3,3,3),columns="minRelProjAberr"), rela=TRUE)

## interesting, but take too long
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="min3"))
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="min3.rela"), rela=TRUE)
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="min34"))
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="min34.rela"), rela=TRUE)
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="minRPFT"), rela=TRUE)
#P3.3(oa.design(L36.2.11.3.12, nlevels=c(2,2,2,3,3,3),columns="minRelProjAberr"), rela=TRUE)

## also interesting but also take too long
#P3.3(oa.design(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), columns="order"), rela=TRUE)
#P3.3(oa.design(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), columns="min34"), rela=TRUE)
#P3.3(oa.design(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), columns="min34.rela"), rela=TRUE)
#P3.3(oa.design(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), columns="minRPFT"), rela=TRUE)
#P3.3(oa.design(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), columns="minRelProjAberr"), rela=TRUE)