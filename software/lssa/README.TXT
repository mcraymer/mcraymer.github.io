---------------------------------------------------------------------------

 LSSA:  Least Squares Spectral Analysis            Version 3.2 (14 Jan 99)

 Copyright (c) 1989-99 University of New Brunswick

---------------------------------------------------------------------------

This Least Squares Spectral Analysis program is based on the the software
described in "Least Squares Spectral Analysis Revisited" by D. Wells, P.
Vanicek and S. Pagiatakas (Dept. of Surveying Engineering Technical Report
No. 84, 1985).  A copy of the report may be obtained for $5.00 from:

        Dept. of Geodesy and Geomatics Engineering
        University of New Brunswick
        P.O. Box 4400
        Fredericton, New Brunswick, E3B 5A3
        Canada

Version 3 of the software uses two input files (a data file and command
file) and creates two output files (a listing of results and a data file
for plotting the time series and spectrum with other program.

Running the LSSA Program
------------------------
On the IBM PC, run the program by entering the following command at the
DOS prompt:

	LSSA lssa.cmd

where "lssa.cmd" is the input command file (see below).  If the command
file is not specified on the command line, the program will prompt you
for it.

On the Macintosh, simply double-click the LSSA application.  The program
will prompt you for the input command file to use.

Input Command File
------------------
The input command file supplies the program with the desired parameters
and options to be used for each run.  The format of the file is:

        Name of input data file (character*20)
        Name for output listing file (character*20)
        Name for output plot data file (character*20)
        Mode of analysis (BATCH=batch, SQNTL=sequential)
        Equally or unequally spaced time series (EQ=equal, UN=unequal)
        Flag for plotting input time series (Y=yes, N=no)
        # of datum biases and times of biases
        Linear trend switch (0=off, 1=on)
        # of forced periods and their values
        User defined function switch (0=off, 1=on)
        Cut-off levels for listing large std. dev. and correlations
        Spectral band number, longest period, shortest period, number of
                periods to be computed (one record per window)

The data must be separated by spaces, commas or tabs.  Each new record
must start on a new line (items may continue on the next line if there
are too many -- e.g., for forced periods).  See the file LSSA3.CMD for
an example.

The program allows several spectral windows to be calculated.  To use
this feature repeat the last record of the command file for each spectral
window.  Note that you must increment the "spectral band number" by one
for each new window. An example is given in the file LSSA3.CMD

Input Data File
---------------
The input data file supplies the program with the time series times and
values.  The format of this file is:

        Data file identification string (less than 80 chars)
        Time,   Associated Value (single record for each time)
          :             :
          :             :
          :             :

The data must be separated by spaces, commas or tabs.  See the file
TEST.DAT for an example.

Output Listing File
-------------------
The program creates a listing containing the following results:

        A summary of the input parameters
        A listing and plot of the input time series
        Estimates of the forced consituents
        Outstanding standard deviations and correlations among the
                forced constituents
        A listing of the residual time series
        A listing and plot of the computed spectrum for each window
                together with the mean spectral value for white noise
                and the critical spectral value for detecting spectral
                peaks at the 95% confidence level

Output Plotting Data File
-------------------------
The program also creates a data file containing the input time series as
well as the computed spectrum for use in various plotting program.  The
format for this file is:

        TIME    SERIES VALUE
        Time    Associated Value (single record for each time)
          :       :
          :       :
          :       :
        FREQUENCY                       SPECTRAL VALUE
        Natural Frequency (unitless)    Spectral Value (% of variance)
          :                               :
          :                               :
          :                               :

Note the two headers that are used to separate the time series and spectrum
listings.  The data in each row are separated by tabs for easy input to
many different types of plotting programs.  The input time series is also
listed in case the input data file does not use tabs to separate the data.
To plot the data simply copy the appropriate part to a separate data file
using your text editor and read this into a plotting program.

Error Messages
--------------
The program generates the following warning and error messages:

      Code      Description

        1       WARNING.  The number of datum biases is less than 0.  It
                is set to 0.
        2       WARNING.  The input linear trend switch is not 0 (don't
                use it) or 1 (use it).  It is set to 0 (not used).
        3       WARNING.  The number of forced periods is less than 0. It
                is set ot 0.
        4       WARNING.  The user-defined function switch is not 0 (don't
                use it) or 1 (use it).  It is set to 0 (not used).
        5       WARNING.  The total number of forced constituents is not
                equal to the sum of the individual forced constituents
                (base functions, linear trend, user-defined function and
                forced periods).  It is set to the sum of these.
      104       FATAL.  Less than 3 time series values input.
      105       FATAL.  Time series times not monotonic increasing.
      106       FATAL.  The total number of constituents exceeds the
                dimensions of arrays A,B,U,V.
      107       FATAL.  The first datum bias must be set equal to the first
                time series time when one or more datum biases are forced.
      108       FATAL.  The residual time series consists of only computer
                roundoff error.  The residuals are essentially zero.
      109       FATAL.  The total number of forced periods exceeds the
                dimensions of arrays CLP,CNP,CP,SLP,SNP,SP,SPMQ,SPPQ.
      110       FATAL.  The number of datum biases and gaps (not specified
                as datum biases) exceeds the dimensions of CLP,CNP,IVL,SLP,
                SNP,XL,XN.
      111       FATAL.  A forced period is less than 2 times the time
                series increment (i.e., Nyquist frequency).
      112       FATAL.  A spectral period is less than 2 times the time
                series increment (i.e., Nyquist frequency).

Help and Problems
-----------------
Please refer any questions or problems to:

        Petr Vanicek
        Geodetic Research Laboratory
        Dept. of Geodesy and Geomatics Engineering
        University of New Brunswick
        P.O. Box 4400
        Fredericton, N.B.  E3B 5A3
        Tel:  (506) 453-5144
        Fax:  (506) 453-4943
        Email: vanicek@unb.ca        
or
        Michael Craymer
        Geodetic Survey Division
        Natural Resources Canada
        615 Booth Street
        Ottawa, Ontario  K1A 0E9
        Tel: (613) 947-1829
        Fax: (613) 992-6628
        Email: craymer@nrcan.gc.ca

Distribution
------------
This software and related documentation are copyrighted by the University
of New Brunswick and protected by Canadian copyright law and international
trade provisions.  Only unmodified copies of this software may be
distributed.  Please include ALL files when distributing this software.
If you find any problems, please report these to the Geodetic Research
Laboratory so that others may benifit from the corrections and enhancements.

Limitation of Liability
-----------------------
THE UNIVERSITY OF NEW BRUNSWICK SHALL HAVE NO LIABILITY WITH RESPECT TO
THIS SOFTWARE OR OTHERWISE FOR SPECIAL, INCIDENTAL, CONSEQUENTIAL, PUNITIVE
OR EXEMPLARY DAMAGES EVEN IF THE UNIVERSITY OF NEW BRUNSWICK HAS BEEN
ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 
