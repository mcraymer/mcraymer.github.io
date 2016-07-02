% MatLab Least Squares Transform Toolbox
% Version 1.0 (1998-01-09)
%
% Copyright (c) 1998, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% A set of functions and scripts for least squares transforms and
% least squares spectral analysis. Used to produce exmaples in:
%
% Craymer, M.R. The Least Squares Spectrum, Its Inverse Transform and
% Autocorrelation Function: Theory and Some Applications in Geodesy.
% Ph.D. Dissertation, Department of Civil Engineering, University of
% Toronto, 1998.
%
% acf - Autocovariance function of evenly spaced series
% acfbin - Autocovariance function of unevenly spaced series using equally spaced lag bins
% acfunb - Transforms biased ACF/ACvF to unbiased ACF/ACvF
% acfw - Weighted autocovariance function of evenly spaced series
% covmat - Forms covariance/correlation matrix from given ACF or ACvF
% covmate - Forms covariance function for specified times given ACF or ACvF
% dfs - Discrete Fourier spectrum (one-sided) without correlations between frequencies
% dft - Discrete Fourier transform
% ffs - Fast Fouier spectrum (one-sided) up to Nyquist frequency
% ffsall - Fast Fourier spectrum (two-sided) for all Fourier frequencies
% fmax - Maximum frequency estimable from a given series
% fmin - Minimum frequency estimable from a given series
% fnyquist - Nyquist frequency for a given series using various methods
% freq - Natural Fourier frequencies (up to Nyquist) for a given series
% freqall - All natural Fourier frequencies (incl. Nyquist) for a given series
% gendat - Generates equally or unequally spaced test data
% hornedat - Generates test data used by Horne & Baliunas (1986)
% idft - Inverse discrete Fourier transform
% ilsft - Inverse least squares Fourier transform without correlations between frequencies
% ilsftc - Inverse least squares Fourier transform with correlations between frequencies
% ilsftce - Inverse least squarews Fourier transform of an even function with correlations between frequencies
% ilsftce - Inverse least squarews Fourier transform of an even function fwithout correlations between frequencies
% lags - Finds all possible lags for a series
% lomb - Normalized periodogram for an unevenly spaced series as defined by Lomb (1976)
% lsft - Least squares Fourier transform without correlations between frequencies
% lsfte - Least squares Fourier transform of an even function without correlation between frequencies
% lss - Least squares spectrum (one-sided) without correlations between frequencies
% lssa - Least squares (one-sided) spectral analysis without correlations between frequencies
% lssaz - Least squares (one-sided) spectral analysis with zero-padding
% lssc - Least squares spectrum (one-sided) with correlations between frequencies
% lssconf - Confidence interval for one-sided LS spectral value
% range - Range of a series (max-min)
% scargle - Modified periodogram as defined by Scargle (1982)
% trend - Least squares trend estimation
% zeropad - Pads a n-length data series with n zeros

% Version History
%
% 0.1  Created
% 1.0  First public distribution
