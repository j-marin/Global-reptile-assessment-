library(ape)
library(phytools)
library(TreePar)
library(PDcalc)

### R code to impute missing species if the genus is alreaddy present in the tree

## R objects
# trees.t <- list of the 100 phylogenetic trees of class multiPhylo
# iucn <- vector of mode character containing the species names with geographic data

## species imputation (for each of the 100 trees)
trees.t100 <- trees.t

for (k in 1:100) {
tree.t2 <- trees.t[k][[1]]

m1 <- match(iucn, tree.t2$tip.label)
n1 <- which(is.na(m1) == TRUE) 
m2 <- unique(iucn[n1]) #250

g1 <- unlist(strsplit(as.character(m2), "_"))[ c(TRUE,FALSE) ] # iucn missing genus
g2 <- unlist(strsplit(tree.t2$tip.label, "_"))[ c(TRUE,FALSE) ] # tree genus

gg <- match(g1, g2)
genus <- g1[-which(is.na(gg) == TRUE)] # 236 sp
species <- m2[-which(is.na(gg) == TRUE)]

write.table(species , "ImputedSp.txt")

tree.tx <- tree.t2
for (i in 1:length(species)) {
	g <- genus[i]
	cl <- tree.tx$tip.label[which(g2 == g)] # get clade species name
	if (length(cl) > 1) {
		nd0 <- getMRCA(tree.tx, cl) # node number
		ndx <- getDescendants(tree.tx, nd0)
			if (length(which(ndx>length(tree.tx$tip.label))) > 0) {
				nd <- sample(c(ndx[ndx>length(tree.tx$tip.label)],nd0),1)
				} else {nd <- nd0}
		edge <- max(getx(tree.tx)) - node.depth.edgelength(tree.tx) [nd] # edge length
		}
		
	if (length(cl) == 1) {
		tp <- which(tree.tx$tip.label == cl)
		nd <- tree.tx$edge[which(tree.tx$edge[,2] == tp), 1] # node number
		edge <- tree.tx$edge.length[which(tree.tx$edge[,2] == tp)] # edge length
		}
	
	tip <- list(edge=matrix(c(2,1),1,2), tip.label=as.character(species[i]), edge.length=edge, Nnode=1)
	class(tip)<-"phylo"
	tree.tx <- bind.tree(tree.tx,tip,where=nd) #6147
	}

trees.t100[[k]] <- tree.tx
print(k)}


## polytomy resolution (for each of the 100 trees)

trees.res <- trees.t100
for (i in 1:100) {
	trees.res[[i]] <- bifurcatr(trees.t100[[i]], 1)}


