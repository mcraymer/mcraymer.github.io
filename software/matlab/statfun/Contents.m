% MatLab Stats Toolbox
% Version 2.3a (2011-03-12)
%
% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Miscellaneous statistics utility functions for linear regression
% and probabilities.
%
% corr     - Correlation matrix from covariance matrix
% invchi2  - Inverse of Chi-square dist'n for 2 deg. freedom
% invf2    - Inverse of Fisher dist'n for 2 deg. of freedom
% invnprob - Inverse of Normal dist'n
% lreg     - Linear regression with optional data weighting
% lreg2    - Linear regression with optional data weighting and
%            optional multiple offsets
% mrand    - Multivariate random numbers
% ndist    - Computes values of the normal distribution curve
% nprob    - Probability for Normal dist'n
% rms      - Room mean square
% trend    - Estimation of multple offsets, linear trend and
%            periodic components
% wmean    - Weighted least squares mean

% Version History
%
% 1.0  Original version.
% 1.2  Added mrand.
% 1.3  Added corr (from Utils_Toolbox 1.2).
% 1.4  Added invchi2, invf2.
% 1.5  Added rms.
% 1.6  Added computation of variance factor to lreg.
%      Optimized lreg for unweighted option.
%      Changed lreg to output scaled cov matrix.
% 1.7  Added wmean.
% 1.8  Added ndist.
% 1.9  Changed lreg, wmean not to scale cov matrix by variance factor.
% 2.0  Added lreg2.
%      Added name of function to help message for all functions.
% 2.1  Corrected lreg2 usage comment and nargin check.
%      Added trend for estimation of multiple offsets, linear trend
%      and periodic components.
% 2.2  Added lreg3.
% 2.3  Removed lreg3 and modified lreg2 for no intercept.
% 2.3a Added general toolbox description in Contents.
%      Revised function description and added copyright notice in all
%      functions; no changes to funcationality.
