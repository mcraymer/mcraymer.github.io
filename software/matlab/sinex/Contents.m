% MatLab SINEX Utilities Toolbox
% Version 1.0a (2011-0-12)
%
% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% rdsnx     - Reads XYZ coordinates and cov matrix from SINEX file
% rdsnx2    - Same as rdsnx but converts to decimal year using CATS formula
% rdsnxvel  - Reads XYZ cordinates/velocities and cov matrix from SINEX file
% rdsnxvel2 - Same as rdsnxvel but converts to decimal year using CATS formula
% snx2mat   - Reads XYZ coordinates and cov matrix from SINEX file and writes
%             to MATLAB .mat file
% snx2neu   - Reads SINEX files and computes NEU coordinates and st. dev. wrt
%             initial position
% snxvel2mat - Reads XYZ coordinates/velocites and cov matrix from SINEX file
%             and writes to MATLAB .mat file

% Version History
%
% 0.1  2010-03-20  Initial version
% 1.0  2010-04-07  Added rdsnxvel & rdsnxvel2;
%                  Modified snxvel2mat.
% 1.0a 2011-03-12  Added general toolbox description in Contents.
%                  Revised function description and added copyright
%                  notice in all functions; no changes to
%                  funcationality.
