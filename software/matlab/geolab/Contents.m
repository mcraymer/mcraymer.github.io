% MatLab GeoLab Utilities
% Version 1.6 (2012-01-15)
%
% Copyright (c) 2012, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Utilities to read/write/transform data files in GeoLab v1 and v2
% format. GeoLab is a popular commerical software for the adjustment
% of geodetic networks. Information about GeoLab can be found at
% <http://www.bitwiseideas.com/Products/GeoLab/>.
%
% mat2geo - Converts MATLAB .mat file to GeoLab 3DC set
% rdgeo1  - Reads GeoLab v1 ADJ file containing code 95 set
% rdgeo2  - Reads GeoLab v2 ADJ file containing 3DC set
% snx2geo - Converts SINEX data file to GeoLab 3DC set
% trngeo  - GUI program to transform GeoLab IOB file to different
%           reference systems
% trnhlp  - Displays help window for trngeo
% trniob  - Transforms IOB records (called by trngeo)
% trniob1 - Old non-GUI version of TRNGEO (no ITRF91)

% Version History
%
% 1.0  1996-06-08  Original version.
% 1.1  1996-06-12  Modified trngeo, trniob.
% 1.2  1996-06-25  Added ITRF91 conversions to trngeo.
%      1996-06-26  Added ITRF92-ITRF93 conversions to trngeo.
% 1.3  1996-09-08  Added rdgeo2.
% 1.4  2001-03-28  Added mat2geo and snx2geo for working with SINEX
%                  files.
% 1.5  2003-04-08  Added rdgeo1.
%                  Modified rdgeo2 (change variable "name" to "id").
% 1.5a 2011-03-12  Modified descriptions of all functions; no changes
%                  functionality of functions.
% 1.6  2012-01-15  Modified snx2geo for function use with optional interactive
%                  input; added check for solution type S on header record;
%                  removed option to rescale covariance matrix.
