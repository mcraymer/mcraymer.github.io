% MatLab Utils Toolbox
% Version 2.4a (2011-03-12)
% 
% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Miscellaneous general utility functions 
% 
% blanka  - Creates array of blanks (based on MATLAB blanks)
% barold  - Modified MATLAB bar to handle round off error
% bdiag   - Forms a block diagonal matrix with B on diagonal
% caldate - Finds calendar date given Julian date
% gridder - Assigns irregular points to nearest grid nodes
% histold - Modified MATLAB hist for new bar2
% juldate - Finds Julian date given calendar date
% liststr - Lists a vector of strings with index numbers 
% lt2full - Converts lower triangular matrix to full form
% ncols   - Number of columns in a matrix
% nrows   - Number of rows in a matrix
% primes  - Finds prime numbers
% release - Returns MATLAB release number
% rms     - Computes rms of vector or array
% strind  - Finds index for a string in an array of strings
% trimtail - Trims (removes) trailing blanks from tail of string
% ver2    - MATLAB VER replacement to also report release number

% Version History
%
% 1.0  Original version
% 1.1  Added atan2.m
% 1.2  Added corr.m
% 1.3  Moved corr.m to Stats_Toolbox 1.3
%      Added plotellip.m
% 1.4  Added lt2full.m
% 1.5  Added bar2.m, hist2.m
% 1.6  Removed atan2 (already in MATLAB 4.x)
%      Added blanka.m, pltvec.m, ptlabels.m, rms.m
%      Modified pltellip for user-defined line type
% 1.7  Added strind.m
%      Changed bar2.m, hist2.m to barold.m, histold.m
% 1.8  Added dates.m
%      Added trim.m, triml.m
%      Corrected pltvec.m
% 1.9  Removed ptlabels.m, pltellip.m, pltvec.m to plotfun/
%      Added ncols.m, nrows.m
% 1.9a Modified dates.m to convert to all other formats
% 1.9b Modified dates.m to use isempty function
% 2.0  Moved dates.m to geodetic toolbox
%      Added/modified juldate and caldate from geodetic toolbox (replaced
%      by new date routines)
% 2.1  Added primes.m
% 2.2  Added release.m to return MATLAB release number
%      Added ver2.m to replace MATLAB function VER to report release number
% 2.3  Removed triml.m, use built-in functions STRJUST or STRTRIM instead
% 2.4  Removed trim.m, use built-in function STRTRIM instead
%      Removed findstr, use built-in function STRMATCH instead
%      Added trimtail.m
% 2.4a Added general toolbox description in Contents.
%      Revised function description and added copyright notice in all
%      functions; no changes to funcationality.
