# otolith-classifier
Binary classifiers test for differences in West and South coast pelagic populations

1) Input data is a list consisting of two elements
- data: (N x 3) data frame with variables (id, length, coast) and N = n of otoliths
- coords: list of N data frames, each containing (x, y) co-ordinates of otolith outlines (from e.g. ImageJ)

2) Run "run_makedata.R" which creates the shape features that go into the classifier. This function does the following:
- creates normalized elliptical Fourier coefficients, and PCs of these
- standardizes these for fish length (regressions with y = NEFC and x = (coast, length))
- removes any features with significant (coast * length) interaction 
The main functions that this function calls are "make_otdata.R", "create_nefc_data.R", and "stdize_by_length.R". 

3) Run "run_classifiers" which creates bootstrap samples of the data and for each of these runs linear discriminant analysis, random forests, and support vector machines, extracting accuracy measures from these.