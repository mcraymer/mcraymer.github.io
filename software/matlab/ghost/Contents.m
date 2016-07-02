% MatLab GHOST Utilities Toolbox
% Version 3.9 (2012-01-15)
%
% Copyright (c) 2012, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Utilties to read/write/modify/plot GHOST data files. GHOST is a
% suite of programs for the adjustment of geodetic networks developed
% by the Geodetic Survey Division of Natural Resources Canada.
% Infomration on GHOST can be found at (request login info from
% craymer@nrcan.gc.ca) <http://www.geod.nrcan.gc.ca/~craymer/ghost/>.
%
% chnum    - Changes station number in GHOST files
% mat2gho  - Converts MATLAB .mat file to GHOST 3DC set
% pltdhz   - Plots horizontal discrepancy vectors for a network
% pltdiff  - Plots 3D coordinate discrepancies for a network
% pltlabels- Plots point labels on a plot of networks points
% rdcompos - Reads coordinate diffs from GHOST COMPOS.LIS file
% rdcompos2 -Reads coordinate diffs from GHOST COMPOS.LIS file (reads
%            number of stations from header)
% rdconfel - Reads conf. region data from GHOST CONFEL.LIS file
% rddxyz   - Reads GHOST code 21 (DXYZ) coordinate difference sets
% rdplh    - Reads GHOST file of code 4 or 96 (PLH) records
% rdres3dc - Reads 3D coordinate residuals from GHOST LISERS.LIS file
% rdwsa    - Reads GHOST code 93 (WSA) or 95 (3DC) coordinate sets
% rdxyz    - Reads GHOST code 24 or 92 (XYZ) coordinate records
% snx2gho  - Converts SINEX data file to GHOST 3DC set
% wrdxyz   - Writes GHOST file of code 41 (DXYZ) record sets
% wrplh    - Writes GHOST file of code 4 or 96 (PLH) records
% wrwsa    - Writes GHOST file of code 93 (WSA) or 95 (3DC) record sets
% wrxyz    - Writes GHOST file of code 24 or 92 (XYZ) records

% Version History
%
% 1.0  1996-06-09  Created.
% 1.1  1996-11-27  Added pltcompos.
%      1996-11-30  Corrected rdgho4.
% 1.2  1996-12-16  Added pltdhz, rdcompos.
% 1.3  1998-03-25  Corrected rdgho4 for default lon sign.
% 1.4  1998-04-03  Updated rdcompos to read id numbers & output status.
%                  Replaced pltcomp with pltdiff and made minor changes.
%                  Minor changes to pltdhz.
%      1998-04-19  Added rdconfel.
% 1.5  1998-06-10  Added ghost3dc, snx2gho, snx2mat.
% 1.6  1999-01-20  Added initialization of variables to rdgho4 and made
%                  compatible with MATLAB 5.2.
% 1.7  1999-05-05  Enhanced comments for snx2gho, snx2mat.
%                  Added mat2gho (replaces ghost3dc).
%      1999-05-07  Updated rdcompos to output height.
% 1.8  1999-12-07  Modified snx2gho & snx2mat to use %d instead of %i format;
%                  assume 6 parameters/station (incl velocity); convert scode to
%                  char string.
% 1.9  2000-04-11  Corrected rdcompos to get number stations from 11th line of
%                  header and added reading of both station numbers and names.
% 2.0  2000-09-25  Minor correction to snx2gho to avoid warning messages when
%                  initializing indx, scode, XYZ to zeros.
% 2.1  2000-10-23  Modified snx2gho & snx2mat to trap error when opening i/o files
%                  and added automatic detection of velocity estimates.
% 2.2  2000-12-14  Corrected rdconfel to output lat,lon in radians.
% 2.3  2001-03-28  Removed ghost3dc (replaced by mat2gho).
%                  Revised desriptive comments and banner in snx2gho.
% 2.4  2001-04-24  Modified rdcomps for GHOST COMPOS version LINUX2000oct and
%                  added option to specific number of stations.
% 2.5  2002-05-06  Added rdxyz, wrplh & wrxyz.
%                  Changed name of rdgho4 to rdplh and expanded station name to
%                  include GHOST station number and name (24 chars).
%      2002-05-13  Modified rdcompos to read number common stations from
%                  "Correction elevation" line in header (commented out other).
% 2.6  2002-08-23  Modified rdcompos to ignore number of stations in header
%                  records and to read all possible difference records; removed
%                  optional input to specify number of stations to read.
% 2.7  2003-04-01  Modified rdwsa to also read 95 record sets, 92 (XYZ) coordinate
%                  records and both covariance (POV) and weight (POW,RNE) matrices,
%                  and expanded station id to include station number and name.
%                  Removed trimming of leading blanks from rdwsa.
%                  Added wrwsa to write 93 and 95 record sets with either 92 or 96
%                  record sets.
%                  Added rdres3dc to read coordinate observation (3DC) residuals
%                  from LISRES.LIS file for a specified station.
% 2.8  2004-04-26  Replaced use of obsolete function triml with strjust in rdplh
%                  and rdxyz.
% 2.9  2006-10-22  Modified snx2mat to add error traps for reading beyond end of
%                  file; added warning if no covariance matrix found.
% 3.0  2006-12-19  Modified snx2gho to output point code and solution number,
%                  increased number of significant figures in output cov matrix,
%                  output reference epoch from SINEX coordinate records & removed
%                  option to scale cov matrix.
% 3.1  2007-06-16  Modified rdplh & rdxyz not to left-justify station number.
% 3.2  2008-03-19  Modified rdcompos and rdres3dc for 2006Mar version of GHOST.
%                  Added snxvel2mat to read both coordinates and velocities from
%                  a SINEX file.
% 3.3  2009-01-05  Added rdcompos2 (reads number of stations from header instead
%                  of incrementally increasing array dimensions as records read).
% 3.4  2009-01-14  Corrected rdres2dc for sessions without specified stations.
% 3.5  2010-11-10  Added rddxyz & wrdxyz to read & write code 41 records.
%                  Added chnum to change a station number in a GHOST file.
%                  Modified rdwsa to read optional epoch on 93 & 95 records, and
%                  suppressed inadvertent output of h.
%                  Cleaned up snx2mat & snxvel2mat and moved to SINEX Utilities
%                  toolbox.
% 3.5a 2011-03-12  Modified format of descriptions in contents.m and all
%                  functions, including chaning dates to consitent format and
%                  adding copyright notice to all functions; no changes to
%                  funcationality of functions.
% 3.6  2011-04-20  Corrected comment symbol in header of snx2gho.
% 3.7  2011-11-13  Modified snx2gho for function use with optional interactive
%                  input.
% 3.8  2011-11-27  Modified for GHOST version 2005Mar.
%                  Computed NEU residual from XYZ residual (NEU residuals not
%                  always computed correctly by GHOST).
%                  Commented out status/debug output.
% 3.9  2012-01-15  Added pltlabels to plot point labels on a network plot.
%                  Corrected snx2gho to not report incorrect number of input
%                  parameters for interactive input, corrected check for valid
%                  parameter types on SINEX header record, and added error trap
%                  for no covariance matrix.
