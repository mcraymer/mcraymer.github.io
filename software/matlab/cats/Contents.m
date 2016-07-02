% MatLab CATS Utilities Toolbox
% Version 1.3 (2013-04-20)
%
% Copyright (c) 2013, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Utilties for creating/reading/plotting time series data files in the format
% used by the GPS Time Series Analysis Software (CATS) from Simon Williams
% at the Proudman Oceanographic Laboratory, UK. The CATS software is available
% from <http://www.pol.ac.uk/home/staff/?user=WillSimoCats>.
%
% gho2cats  - Reads GHOST WSA files and computes NEU coordinates and st.dev.
%             of a site wrt to initial position and writes to time series
%             file in CATS format
% gho2cats  -
% pltcats   - Reads and plots NEU time series from CATS file
% rdcats    - Reads NEU time series from CATS file
% snx2cats  - Reads SINEX files and computes NEU coordinates and st.dev. of a
%             site wrt initial position and writes to CATS time series file
% snx2cats2 - Same as snx2cats but converts to decimal year using CATS formula

% Version History
%
% 1.0  2010-11-18  Initial version (moved most routines from SINEX toolbox).
%                  Modified rdcats to read arbitrary number of header/comment
%                  records.
%                  Modified snx2cats & snx2cats2 to write reference frame
%                  record and "Reference Position" comment in header.
%                  Removed pltcatsline and added option of plotting lines
%                  between points in pltcats.
% 1.1  2010-12-11  Corrected usage info in header in snx2cats & snx2cats2.
%                  Correct plotting offset for north time series in pltcats,
%                  Enabled computing and plotting linear trends with multiple
%                  offsets using lreg2, and output rms of residual time series
%                  in pltcats.
% 1.2  2011-03-29  Revised snx2cats & snx2cats2 so that 0.00005 is assigned to
%                  standard deviation of sn, se, su if less than 0.00005 to
%                  avoid round off to 0.0000 in output neu file.
% 1.3  2013-04-20  Revised gho2cats, snx2cats & snx2cats2 to output file even if
%                  only one epoch, changed order of input arguments to require
%				   rframe input & added/modified user messages.
%                  Corrected pltcats not to compute & plot velocities if 1 epoch.
%				   Modified pltcats to test/quit if only 1 epoch.
%				   Modified gho2cats to use new rdwsa (reads epoch from columns
%				   46-59 on header record).
%				   Renamed old gho2cats to gho2cats2 (uses old rdwsa to read
%				   epoch from columns 8:15 on header record).
