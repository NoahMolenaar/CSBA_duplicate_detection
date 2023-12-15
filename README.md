# CSBA_duplicate_detection

What is this project about? See the abstract and introduction of my paper corresponding to this code: 

"In the realm of online shopping, the accessibility of products
through comparison-platforms has simplified consumer decision-making.
This study enhances the Multi-component Similarity Method with Pre-
selection (MSMP) for duplicate detection that can be used by such plat-
forms, building on the pre-selection technique using Locality-Sensitive
Hashing (LSH). This paper introduces television brand-names as addi-
tional model words to find a better performing, scalable solution for
product duplicate detection than the one found for the standard MSMP.

Shopping and comparing products has become much easier for consumers since
the introduction of Web shops. Consumers in the Netherlands for example, can
nowadays rely on websites such as www.vergelijk.nl and www.kieskeurig.nl
to find their desired products. These websites compare the same products, offered
by different online shops, and present an overview of the prices for these shops
and the product specifications. This way, consumers can save up a lot of time
by not comparing products among different websites.
A way of implementing duplicate detection demanded by those websites is
the Multi-component Similarity Method (MSM), such as used in [11]. As this
method can have an enormous running time for vast sets of data, [12] first use
Locality-Sensitive Hashing (LSH) as pre-selection to obtain duplicate candidate
pairs. As MSM will then only be performed on the found candidate pairs, the
computation time gets reduced. The application of LSH from [12] is expanded in
[7], by having different model words as input and the addition of data cleaning.
This paper, extends the way LSH is used by [12], by adding television brands to
the set of model words. This brings me to the research question: “How does the
inclusion of brand-name model words contribute to the performance of the Multi-
component Similarity Method with pre-selection? ” Furthermore, data cleaning is
used by implementing inconsistencies such as found in [7].

7. Hartveld, A., van Keulen, M., Mathol, D., van Noort, T., Plaatsman, T., Fras-
incar, F., Schouten, K.: An lsh-based model-words-driven product duplicate de-
tection method. In: International Conference on Advanced Information Systems
Engineering. pp. 409–423. Springer (2018)
11. Van Bezu, R., Borst, S., Rijkse, R., Verhagen, J., Vandic, D., Frasincar, F.: Multi-
component similarity method for web product duplicate detection. In: Proceedings
of the 30th annual ACM symposium on applied computing. pp. 761–768 (2015)
12. Van Dam, I., van Ginkel, G., Kuipers, W., Nijenhuis, N., Vandic, D., Frasincar, F.:
Duplicate detection in web shops using lsh to reduce the number of computations.
In: Proceedings of the 31st Annual ACM Symposium on Applied Computing. pp.
772–779 (2016)
"

In this paper, I have worked with Jens Peek (542716jp) as programming buddy. We worked in R and created different R scripts for in-sample evaluation and out-of-sample evaluation.
The in-sample script needs to be run first, thereafter the out-of-sample script can be run. The latter script is quite similar to the former, but uses the resulting parameters from the former as input. As of this, only the in-sample code will be described below. The out-of-sample simply uses one parameter value per parameter and the test data, obtained by using the same seed as in the training case. 
We are particularly interested in the results of the out-of-sample script. 

The data used is that of TVs-all-merged.json and television brands.xlsx. 

The code essentially consists of four parts, namely the data collection and cleaning part, a part consisting of all functions used, the LSH part and the MSM part. 

The data cleaning and addition of brand-based model words is explained in my paper, television brands.xlsx needs to be downloaded for this. 

Most functions are inspired by and named after the functions in Algorithm 1 in [11]. The MSM part is created after following this algorithm. 

The LSH part is inspired by [12].

Final results as reported in my paper can be found after running the outofsample.R code and accessing the F1_msm_matrix matrix for the F1* measure and the PeRe_msm_matrix matrix for the F1 measure.




