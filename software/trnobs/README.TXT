
TRNOBS SOFTWARE ARCHIVE
------------------------------
Last revised 20 September 2007

This directory contains files for the TRNOBS software for transforming
GHOST INTCOBS and GeoLab (versions 1 and 2) IOB observation files
between ITRF and NAD83(CSRS98) reference frames.

NOTE 1
TRNOBS was revised to v2.8 (06.10.14) to include ITRF2005 (denoted as
ITRF05). The label NAD83(CSRS98) was also replaced with NAD83(CSRS) as
the two are now the same (CSRS98 was used to distinguish it from a
repliminary version called CSRS96 which saw very little use).

NOTE 2
TRNOBS was revised to v2.8a (07.09.20) to correct the optional output
of transformation rates for the transformation from NAD83(CSRS) to
ITRF2005 (incorrect signs for plate rotation). This error does not
affect the transformed coordinates, and rates are not output unless
the printrates parameter is changed from .false. to .true. in the
source code.


DIRECTORY CONTENTS
------------------
README.TXT     This file.

trn28a_lnx.zip Compressed ZIP file of TRNOBS v2.8a for Linux. Contains
               Fortran source code, sample input/output files and
               binary executable for RedHat Linux.

trn28a_osx.zip Compressed ZIP file of TRNOBS v2.8a for Mac OSX. Contains
               Fortran source code, sample input/output files and
               binary executable for RedHat Linux.

trn28a_dos.zip Compressed ZIP file of TRNOBS v2.8a for DOS. Contains
               Fortran source code, sample input/output files and
               binary executable for DOS.

trnobs28a_lnx  TRNOBS v2.8a binary executable for Linux.

trnobs28a_osx  TRNOBS v2.8a binary executable for Mac OSX.

trnobs28a.exe  TRNOBS v2.8a binary executable for DOS.

trnobs28a.f    Fortran source code for TRNOBS v2.8a.

trnobs28a.txt  Documentation for TRNOBS v2.8a.

ghost.n83      Example GHOST data file in NAD83(CSRS98) reference
               system. Used as input to test TRNOBS.

ghost.i97      Example TRNOBS output file transforming ghost.n83 file
               from NAD83(CSRS) to ITRF97 at epoch 2000-01-01.

ghost.i00      Example TRNOBS output file transforming ghost.n83 file
               from NAD83(CSRS) to ITRF2000 at epoch 2002-01-01.

ghost.i05      Example TRNOBS output file transforming ghost.n83 file
               from NAD83(CSRS) to ITRF2005 at epoch 2006-01-01.

ghost_i97.n83  Example TRNOBS output file transforming ghost.i97 file
               from ITRF97 at epoch 2000-01-01 to NAD83(CSRS).

ghost_i00.n83  Example TRNOBS output file transforming ghost.i00 file
               from ITRF2000 at epoch 2002-01-01 to NAD83(CSRS).

ghost_i05.n83  Example TRNOBS output file transforming ghost.i05 file
               from ITRF2005 at epoch 2006-01-01 to NAD83(CSRS).

geolab.n83     Example GeoLab IOB data file in NAD83(CSRS98) reference
               system. Used as input to test TRNOBS.

geolab.i00     Example TRNOBS output file transforming geolab.n83 file
               from NAD83(CSRS) to ITRF2000 at epoch 2002-01-01.

geolab.i05     Example TRNOBS output file transforming geolab.n83 file
               from NAD83(CSRS) to ITRF2005 at epoch 2006-01-01.

geolab_i00.n83 Example TRNOBS output file transforming geolab.i00 file
               from ITRF2000 at epoch 2002-01-01 to NAD83(CSRS).

geolab_i05.n83 Example TRNOBS output file transforming geolab.i05 file
               from ITRF2005 at epoch 2006-01-01 to NAD83(CSRS).

archive        Archive of old versions of TRNOBS


DISTRIBUTION
------------
This software is available only to members of the GHOST User's Group
(GUG).  No further distribution of this software, in any form, either
in whole or in part, is permitted. Membership in GUG is presently
restricted to government organizations. Approval of membership is at
the sole discretion of the Geodetic Survey Division (GSD) of
Geomatics Canada, Natural Resources Canada.


DISCLAIMER
----------
This software is provided on an "as is" basis. The members of GUG and
Her Majesty the Queen in Right of Canada (Canada), represented by the
Minister of Natural Resources and acting through GSD, make no
guarantees, representations or warranties respecting this software,
either expressed or implied, arising by law or otherwise, including
but not limited to, effectiveness, completeness, accuracy or fitness
for a particular purpose.

Canada and the members of GUG shall not be liable in respect of any
claim, demand or action, irrespective of the nature of the cause of
the claim, demand or action alleging any loss, injury or damages,
direct or indirect, which result from the use or possession of this
software.  The members of GUG shall indemnify and save harmless
Canada against any such claims, demands or actions, as shall Canada
indemnify and save harmless the members of GUG.


FEEDBACK
--------
Please post any comments or questions about the GHOST Software Archive
to the GHOST Software User's Group at

  http://www.geod.nrcan.gc.ca/~craymer/ghost/
  mailto:gug@geod.nrcan.gc.ca

--
The GHOST User's Group Forum
Postings <mailto:gug@geod.nrcan.gc.ca>
Subscribe <mailto:gug-request@geod.nrcan.gc.ca?body=subscribe>
Unsubscribe <mailto:gug-request@geod.nrcan.gc.ca?body=unsubscribe>
SW archives <ftp://ghostftp@gauss.geod.nrcan.gc.ca/>, Password req'd
Web page (help) <http://www.geod.nrcan.gc.ca/~craymer/ghost/>
Comments (List Manager) <mailto:craymer@nrcan.gc.ca>
