library(vegan)

# 3 dimensions
all3 = metaMDS(dissim, k=3, 
               distance="euclidean", autotransform=F, pc=T)
plot(all3, type="t", main="ALL")

thinking3 = metaMDS(dissim_thinking, k=3, 
                    distance="euclidean", autotransform=F, pc=T)
plot(thinking3, type="t", main="THINKING")

feelings3 = metaMDS(dissim_feelings, k=3,
                    distance="euclidean", autotransform=F, pc=T)
plot(feelings3, type="t", main="FEELINGS")

hunger3 = metaMDS(dissim_hunger, k=3,
                  distance="euclidean", autotransform=F, pc=T)
plot(hunger3, type="t", main="HUNGER")

# 2 dimensions
all2 = metaMDS(dissim, k=2, 
               distance="euclidean", autotransform=F, pc=T)
plot(all2, type="t", main="ALL")

thinking2 = metaMDS(dissim_thinking, k=2, 
                    distance="euclidean", autotransform=F, pc=T)
plot(thinking2, type="t", main="THINKING")

feelings2 = metaMDS(dissim_feelings, k=2,
                    distance="euclidean", autotransform=F, pc=T)
plot(feelings2, type="t", main="FEELINGS")

hunger2 = metaMDS(dissim_hunger, k=2,
                  distance="euclidean", autotransform=F, pc=T)
plot(hunger2, type="t", main="HUNGER")

# 1 dimensions
all1 = metaMDS(dissim, k=1, 
               distance="euclidean", autotransform=F, pc=T)
plot(all1, type="t", main="ALL")

thinking1 = metaMDS(dissim_thinking, k=1,
                    distance="euclidean", autotransform=F, pc=T)
plot(thinking1, type="t", main="THINKING")

feelings1 = metaMDS(dissim_feelings, k=1,
                    distance="euclidean", autotransform=F, pc=T)
plot(feelings1, type="t", main="FEELINGS")

hunger1 = metaMDS(dissim_hunger, k=1,
                  distance="euclidean", autotransform=F, pc=T)
plot(hunger1, type="t", main="HUNGER")



