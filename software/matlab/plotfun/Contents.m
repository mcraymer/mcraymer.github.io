% MatLab Plot Toolbox
% Version 1.7a (2011-03-12)
%
% Copyright (c) 2011, Michael R. Craymer
% All rights reserved.
% Email: mike@craymer.com
%
% Miscellaneous functions for plotting of points, vectors, error
% ellipses and point labels.
%
% Plot Controls
% figdef   - Resets default figure/axes to startup values
% figsmall - Sets default figure/axes sizes to fit 6.5" margins
% getaxep  - Gets current axes position
% getfigp  - Gets current default figure position
% labelon  - Turns point text labels on (visible on)
% labeloff - Turns point text labels off (visible off)
% mclegend - My version of MATLAB legend.m (2 extra input args)
% ptlabels - Creates an array of point labels (pt. no. & value)
% pltmenu  - Adds figure menu for general plot controls
% pltell   - Plots an ellipse
% pltell3  - Plots an ellipsoid as projection ellipses on axes planes
% pltvec   - Plots a set of vectors in a network
% scalebar - Adds scale bar to current plot
% setaxep  - Sets default axes position (left,bottom,width,height)
% setaxes  - Sets default axes size (width,height)
% setfigp  - Sets default figure position
% setfigs  - Sets default figure size (width,height)
% setfont  - Sets default figure text and axes font

% Version History
%
% 1.0  Created.
% 1.1  Added fignew, figold.
% 1.2  Modified plotmenu to omit label items if no label handle input.
%      Added ptlabels, pltellip, pltvec from Utils toolbox.
% 1.3  Renamed deffigpos to deffigp for PC.
% 1.4  Added getfigp, setfigp, setfigs.
%      Added getaxep, setaxep, setaxes.
%      Added mclegend, scalebar.
%      Modified setfigp.
% 1.5  Added figsmall, setfont.
%      Modified setfigp (reset font type/size).
%      Modified setaxep (set default, not current).
%      Renamed deffigp to figdef.
%      Modified getaxep (get default, not current).
% 1.6  Changed name of plotmenu to pltmenu.
% 1.6a Corrected name of pltell & changed file name
% 1.7  Added pltell3
% 1.7a Added general toolbox description in contents.m and copyright
%      notice to all functions; no changes to funcationality of
%      functions.
