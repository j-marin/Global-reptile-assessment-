library(raster)
library(picante)
library(ape)
library(caper)

### R code to compute the phylogenetic diversity (PD)

## sub-sampling (here it corresponds to the 50x50 resolution analysis, it should be change accordingly to the raster resolution used)
m <- matrix(rep(0,328624), nrow = 368, ncol = 893)
rg <- seq(1, 328624, 10000)
rg <- c(rg, 328625)

## R objects
# str_names2 : vector of mode character containing the species names with both genetic and geographic data
# tree : list of the 100 phylogenetic trees of class multiPhylo
# imputed : vector of mode character containing the imputed species names (we imputed species for which the genus was already present in the tree)

## set directory to the directory containing the raster files of each species

for (j in 1:length(rg)) {
	mat.x <- integer(0)
	for (i in 1:length(str_names2)) {
		str_name <- paste(str_names2[i], ".tif", sep="")
		imported_raster = raster(str_name)
		rx <- values(imported_raster)
		rx [is.na(rx)] <- 0
		mat.x <- cbind(mat.x, rx[rg[j]:(rg[j+1]-1)])
		print(i)
		}

	colnames(mat.x) <- sp_names2

	## all species
	sr.k <- pd(mat.x, tree[1][[1]], include.root=TRUE)[,2]
	
	# compute median pd for cells with more than 0 species
	pd.cells <- which(sr.k > 0)
	
	if (length(pd.cells) > 0) {
	pd.k <- matrix(0, ncol=100, nrow = dim(mat.x)[1]) 
		for(k in 1:100) {
		for(p in pd.cells){
			sp.cell <- names(which(mat.x[p,]>0))
			t.cell <- keep.tip(tree[k][[1]], sp.cell) 
			genus.cell <- unlist(strsplit(sp.cell, "_"))[ c(TRUE,FALSE) ]
			imputed.sp.nb <- match(sp.cell, imputed)
			imputed.sp.nb <- which(!is.na(imputed.sp.nb) == "TRUE")
			imputed.sp <- sp.cell[imputed.sp.nb]
			imputed.genus <- unique(unlist(strsplit(imputed.sp, "_"))[ c(TRUE,FALSE) ])
			
			ed.x <- ed.calc(t.cell)$spp
			ed.x["ED2"] <- ed.x[,2]

			# sensitivity analysis
			if (length(imputed.genus) > 0) {
				for (mm in 1:length(imputed.genus)) {
					sp.x <- sp.cell [which(genus.cell == imputed.genus[mm])]
					sp.genet <- sp.x[is.na(match(sp.x, imputed.sp))]
					sp.imputed <- sp.x[!is.na(match(sp.x, imputed.sp))]
					if (length(sp.genet) > 1) {
						ed.x[!is.na(match(ed.x$species, sp.imputed)),2] <- mean(ed.x[!is.na(match(ed.x$species, sp.genet)),2])
					
						ed.x[!is.na(match(ed.x$species, sp.x)),3] <- ed.x[!is.na(match(ed.x$species, sp.x)),2]
						ed.x[!is.na(match(ed.x$species, sp.x)),3] <- mean(ed.x[!is.na(match(ed.x$species, sp.x)),3])
						}
					}}

						
			pd.k[p,k] <- mean(colSums(ed.x[,2:3]))
			}
			print(k)
			}

		pd.k <- apply(pd.k,1,median,na.rm = TRUE)
		pd.all <- cbind(pd.k,sr.k)}
	
	else {pd.all <- cbind(rep(0, length(sr.k)), sr.k)}

	write.table (pd.all, file = "pd.all_median.txt", quote=FALSE, append = TRUE, col.names = FALSE, row.name = FALSE)

	## non threatened species
	mat.x2 <- mat.x[,-d]
	sr.k <- pd(mat.x2, tree[1][[1]], include.root=TRUE)[,2]
	
	# compute median pd for cells with more than 0 species
	pd.cells <- which(sr.k > 0)
	
	if (length(pd.cells) > 0) {
	pd.k <- matrix(0, ncol=100, nrow = dim(mat.x2)[1]) 
		for(k in 1:100) {
		for(p in pd.cells){
			sp.cell <- names(which(mat.x2[p,]>0))
			t.cell <- keep.tip(tree[k][[1]], sp.cell) 
			genus.cell <- unlist(strsplit(sp.cell, "_"))[ c(TRUE,FALSE) ]
			imputed.sp.nb <- match(sp.cell, imputed)
			imputed.sp.nb <- which(!is.na(imputed.sp.nb) == "TRUE")
			imputed.sp <- sp.cell[imputed.sp.nb]
			imputed.genus <- unique(unlist(strsplit(imputed.sp, "_"))[ c(TRUE,FALSE) ])
			
			ed.x <- ed.calc(t.cell)$spp
			ed.x["ED2"] <- ed.x[,2]
			# sensitivity analysis
			if (length(imputed.genus) > 0) {
				for (mm in 1:length(imputed.genus)) {
					sp.x <- sp.cell [which(genus.cell == imputed.genus[mm])]
					sp.genet <- sp.x[is.na(match(sp.x, imputed.sp))]
					sp.imputed <- sp.x[!is.na(match(sp.x, imputed.sp))]
					if (length(sp.genet) > 1) {
						ed.x[!is.na(match(ed.x$species, sp.imputed)),2] <- mean(ed.x[!is.na(match(ed.x$species, sp.genet)),2])
						
						ed.x[!is.na(match(ed.x$species, sp.x)),3] <- ed.x[!is.na(match(ed.x$species, sp.x)),2]
						ed.x[!is.na(match(ed.x$species, sp.x)),3] <- mean(ed.x[!is.na(match(ed.x$species, sp.x)),3])
						}
					}}
						
			pd.k[p,k] <- mean(colSums(ed.x[,2:3]))
			}
			print(k)
			}

		pd.k <- apply(pd.k,1,median,na.rm = TRUE)
		pd.all <- cbind(pd.k,sr.k)}
	
	else {pd.all <- cbind(rep(0, length(sr.k)), sr.k)}

	write.table (pd.all, file = "pd.nt_mammals_median.txt", quote=FALSE, append = TRUE, col.names = FALSE, row.name = FALSE)

	print(j)}



