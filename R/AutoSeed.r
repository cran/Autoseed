if (getRversion()>="2.15.1") utils::globalVariables(c("mala","edgar","drugbank"))

#' Get disease-related genes from eDGAR
#'
#'
#' @param disease  Name of the disease, character
#'
#' @return a vector containing genesymbol related to the disease
#' @export
#'
#' @examples
#' result = edgar_disease_gene("diabetes")

	edgar_disease_gene = function(disease){



	  grep(toupper(disease),edgar$Disease,value = FALSE) -> grep_temp

	  gene = as.vector(edgar$Gene[grep_temp])

	  return(gene)

	 }

#' Get disease-related genes from DrugBank.
#'
#' @param search Name of the disease, character
#'
#' @return The genes related to the disease, list
#' @export
#'
#' @examples
#' result = drugbank_disease_gene("diabetes")

	drugbank_disease_gene = function(search){


	  grep(toupper(search),drugbank$disease,value = FALSE) -> grep_temp

	  gene = as.vector(drugbank$gene[grep_temp])

	  return(gene)

}


#' Get disease-related genes from MalaCards
#'
#' @param disease Name of the disease
#'
#' @return The genes related to the disease, character vector
#' @export
#'
#' @examples
#' result = malacards_disease_gene("diabetes")

	malacards_disease_gene = function(disease){


	 grep(toupper(disease),mala$disease,value = FALSE) -> grep_temp

	 gene = as.vector(mala$gene[grep_temp])

	 return(gene)

	}


#' Get disease-related genes from eDGAR, DrugBank and MalaCards
#'
#' @param search Name of the disease
#'
#' @return
#' result$edgar: Containing Disease Name, OMIM ID and Genesymbol (Data comes from the eDGAR)
#' result$malacards: Containing genes related to the disease (Data comes from the MalaCards)
#' result$drugbank: Containing genes related to the disease (Data comes from the DrugBank)
#' @export
#'
#' @examples
#' result = AutoSeed("diabetes")
	AutoSeed = function(search){
		output = c()

		edgar_disease_gene(search)->output$edgar
		malacards_disease_gene(search)->output$malacards

		drugbank_disease_gene(search)->output$drugbank

	return(output)
	}




