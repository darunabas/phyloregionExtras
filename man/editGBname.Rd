% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleanGBnames.R
\name{editGBname}
\alias{editGBname}
\title{Clean taxon names of DNA sequences from NCBI's GenBank}
\usage{
editGBname(x, sep = "\\\\ ", ...)
}
\arguments{
\item{x}{A matrix of DNA sequences (often in fasta format) with character strings representing the names of the taxa.}

\item{sep}{Punctuation marks or string of characters to clean.}

\item{\dots}{Further arguments passed to or from other methods.}
}
\value{
A fasta file with all the names of DNA sequences renamed in the format: "Genus_species_Accession#".
}
\description{
\code{editGBname} Removes unwanted white spaces, punctuation marks, and reorders names of raw sequences downloaded 
from GenBank for each taxon.
}
\examples{
require(ape)
cat(">KJ557927.1 Aloe vogtsii voucher Grace57 internal transcribed spacer 1, 
partial sequence; 5.8S ribosomal RNA gene, 
complete sequence; and internal transcribed spacer 2, partial sequence", 
"GTCGAGACCCGAAAGGACAACCGCGAATCATCGATCTCTTTACAATGAGCGCCCGAGCATCGCTTCGGCG",
">KJ557926.1 Aloe viguieri voucher Grace193 internal transcribed spacer 1, 
partial sequence; 5.8S ribosomal RNA gene, 
complete sequence; and internal transcribed spacer 2, partial sequence", 
"GTCGAGACCCGAAAGGACGACCGCGAACCATTGATCTCTTTACAATGAGCGCCCGAGCATCGCTTCGGCG",
">KJ557925.1 Aloe vanrooyenii voucher Grace70 internal transcribed spacer 1, 
partial sequence; 5.8S ribosomal RNA gene, 
complete sequence; and internal transcribed spacer 2, partial sequence", 
"GTCGAGACCCGAAAGGACAACCGCGAACCATCGATCTCTTTACAATGAGCGCCCGAGCATCGCTTCGGCG", 
file = "its.aloe.fasta", sep = "\n")

tmp <- read.FASTA("its.aloe.fasta", type = "DNA")

editGBname(tmp)

}
\author{
Barnabas H. Daru \email{darunabas@gmail.com}
}
\keyword{bioregion}
