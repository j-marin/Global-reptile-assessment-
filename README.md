# Global-reptile-assessment-
Title: Global reptile assessment shows commonality of tetrapod conservation needs

Authors: Neil Cox, Bruce E. Young, Philip Bowles, Miguel Fernandez, Julie Marin, Giovanni Rapacciuolo, Monika Böhm, Thomas M. Brooks, S. Blair Hedges, Craig Hilton-Taylor, Michael Hoffmann, Richard K. B. Jenkins, Marcelo F. Tognelli, Graham J. Alexander, Allen Allison, Natalia B. Ananjeva, Mark Auliya, Luciano Javier Avila, David G. Chapple, Diego F. Cisneros-Heredia, Harold G. Cogger, Guarino R. Colli, Anslem de Silva, Carla C. Eisemberg, Johannes Els, Ansel Fong G., Tandora D. Grant, Rodney A. Hitchmough, Djoko T. Iskandar, Noriko Kidera, Marcio Martins, Shai Meiri, Nicola J. Mitchell, Sanjay Molur, Cristiano de C. Nogueira, Juan Carlos Ortiz, Johannes Penner, Anders G. J. Rhodin, Gilson Rivas, Mark-Oliver Rödel, Uri Roll, Kate L Sanders, Georgina Santos-Barrera, Glenn M. Shea, Stephen Spawls, Bryan L. Stuart, Krystal A. Tolley, Jean-François Trape, Marcela A. Vidal, Philipp Wagner, Bryan P. Wallace, and Yan Xie


1) Tetrapod trees used in computing PD
amphibians_imputed_resolved_100trees.trees.zip
birds_imputed_resolved_100trees.trees.zip
mammals_imputed_resolved_100trees.trees.zip
reptiles_imputed_resolved_100trees.trees.zip

Methods

To calculate the phylogenetic diversity (PD), we used published timetrees of mammals (Upham et al. 2019 PLoS biology), birds (Hedges et al. 2015 Molecular Biology and Evolution), and amphibians (Jetz & Pyron 2018 Nature ecology and evolution). For reptiles, we combined two timetrees: a comprehensive squamate timetree containing 9,755 squamate species, including the species Sphenodon punctatus (Tonini et al. 2016 Biological Conservation) and a turtle and crocodilian tree containing 384 species (Colston et al. 2020 BMC Evolutionary Biology). The timetrees contain some species lacking genetic data, added by taxonomic interpolation to maximize taxonomic coverage. In total, we analyzed 33,168 tetrapod species including 10,139 reptiles, 5911 mammals, 9879 birds, and 7239 amphibians. For squamates, and for turtles and crocodiles, 10,000 fully resolved trees were available. For each group, we randomly sampled 100 trees and combined them to obtain 100 fully resolved reptile timetrees, to accommodate for uncertainty. Similarly, we randomly sampled 100 amphibian and 100 mammal timetrees over the 10,000 available. 

We thoroughly compared the species name mismatches between geographical and phylogenetic data in order to match synonyms and correct misspelled names. We also imputed species for which the genus was already present in the tree (262 amphibians, 1694 bird, 236 mammal, and 777 reptile species). Imputed species were randomly attached to a node within the genus subtree. Because polytomies can result in an overestimation of the phylogenetic diversity, we randomly resolved all polytomies using the Rangel et al. (Evolution, 2015) method implemented in R code. This procedure was performed 100 times for birds, and one time for each of the 100 amphibian, 100 mammal and 100 reptile timetrees.


2) Code used to compute PD and to perform the sensitivity analysis.
PD_computation.r
