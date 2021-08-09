library(ape)
library(phytools)
library(TreePar)
library(PDcalc)

### R code to impute missing species if the genus is alreaddy present in the tree

## R objects
# mammals.t <- list of the 100 phylogenetic trees of class multiPhylo
# iucn <- vector of mode character containing the species names with geographic data

## species imputation (for each of the 100 trees)
mammals.t100 <- mammals.t

for (k in 1:100) {
mammals.t2 <- mammals.t[k][[1]]

m1 <- match(iucn, mammals.t2$tip.label)
n1 <- which(is.na(m1) == TRUE) 
m2 <- unique(iucn[n1]) #250

g1 <- unlist(strsplit(as.character(m2), "_"))[ c(TRUE,FALSE) ] # iucn missing genus
g2 <- unlist(strsplit(mammals.t2$tip.label, "_"))[ c(TRUE,FALSE) ] # tree genus

gg <- match(g1, g2)
genus <- g1[-which(is.na(gg) == TRUE)] # 236 sp
species <- m2[-which(is.na(gg) == TRUE)]

write.table(species , "ImputedSp_mammals.txt")

mammals.tx <- mammals.t2
for (i in 1:length(species)) {
	g <- genus[i]
	cl <- mammals.tx$tip.label[which(g2 == g)] # get clade species name
	if (length(cl) > 1) {
		nd0 <- getMRCA(mammals.tx, cl) # node number
		ndx <- getDescendants(mammals.tx, nd0)
			if (length(which(ndx>length(mammals.tx$tip.label))) > 0) {
				nd <- sample(c(ndx[ndx>length(mammals.tx$tip.label)],nd0),1)
				} else {nd <- nd0}
		edge <- max(getx(mammals.tx)) - node.depth.edgelength(mammals.tx) [nd] # edge length
		}
		
	if (length(cl) == 1) {
		tp <- which(mammals.tx$tip.label == cl)
		nd <- mammals.tx$edge[which(mammals.tx$edge[,2] == tp), 1] # node number
		edge <- mammals.tx$edge.length[which(mammals.tx$edge[,2] == tp)] # edge length
		}
	
	tip <- list(edge=matrix(c(2,1),1,2), tip.label=as.character(species[i]), edge.length=edge, Nnode=1)
	class(tip)<-"phylo"
	mammals.tx <- bind.tree(mammals.tx,tip,where=nd) #6147
	}

mammals.t100[[k]] <- mammals.tx
print(k)}

write.tree(mammals.t100, "mammals_imputed_100trees.nwk")


## polytomy resolution (for each of the 100 trees)

tree.res <- mammals.t100
for (i in 1:100) {
	tree.res[[i]] <- bifurcatr(mammals.t100[[i]], 1)}







