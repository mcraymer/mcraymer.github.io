c***********************************************************************
c
      program dates
c
c     Converts different date formats to all other date formats.
c     Supported date formats are:
c       Year, Month, Day'
c       Year, Day of Year'
c       Year (including decimal)'
c       Julian Date'
c       GPS Week, Seconds of Week'
c
c     Created:
c       26 Apr 99  Michael R. Craymer
c
c     Modified:
c       09 Jul 99  MRC  Version 1.1
c                       Corrected GPS week number to begin at 0 in
c                       GPS2JD, JD2GPS
c                       Output both total GPS weeks since 1980.1.6
c                       and official GPS weeks since 1024 rollover
c                       Added reminder to include century in input year
c                       in main program
c       14 Dec 00  MRC  Version 1.2
c                       Corrected output of seconds of week as integer
c                       Output only total GPS week
c       24 Jul 03  MRC  Version 1.3
c                       Corrected typo in call to gps2jd (replaced dow
c                       with sow)
c                       Removed input/output of GPS week rollover
c       11 Jun 04  MRC  Version 1.4
c                       Corrected error in v1.3 when removed GPS week
c                       rollover in GPS2JD and JD2GPS; removed
c                       rollover computation altogether
c
c     Input:
c       Console
c
c     Output:
c       Console
c
c     Externals:
c       cal2jd, doy2jd, gps2jd, jd2cal, jd2dow, jd2doy, jd2gps, jd2yr,
c       yr2jd
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit  none
c
c  Variable specifications
c
      logical   err
      character buf*80, day(7)*3, ver*15
      integer   ios, fmt, iyr, mn, dow, gpsweek
      real*8    jd, yr, dy, doy, sow
      data      day/'Sun','Mon','Tue','Wed','Thu','Fri','Sat'/,
     +          ver/'1.4 (04-06-11)'/
c
c
c
 100  format(1x,a)
 110  format(1x,a,$)
c
c  Get type of conversion
c
      do while (.true.)
      
      err = .false.
      write(*,*)
      write(*,100) '------------------------------------------------'
      write(*,100) ' DATES: Converts between different date formats'
      write(*,100) '        M.Craymer, v'//ver
      write(*,100) '------------------------------------------------'
      write(*,100)
      write(*,100) 'Available input date formats:'
      write(*,100) ' 1) Year, Month, Day'
      write(*,100) ' 2) Year, Day of Year'
      write(*,100) ' 3) Year with decimal'
      write(*,100) ' 4) GPS Week, Sec of Week'
      write(*,100) ' 5) Julian Date'
      write(*,100)
      write(*,110) 'Select input date format [quit] > '
      read(*,'(a)',iostat=ios) buf
      if (ios.ne.0) then
         write(*,*) '***** ERROR reading input string'//char(7)
         write(*,*) '      IOSTAT =',ios
         stop
      end if
c
c  Exit program
c
      if (buf.eq.' ') then
         fmt = 0
c
c  Get date format
c
      else
         read(buf,*,iostat=ios) fmt
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date format'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
      end if
c
c  0 - Quit program
c
      if (fmt.eq.0) then
         stop
c
c  1 - Year, month, day
c
      else if (fmt.eq.1) then
         write(*,110) 'Enter year (incl century), month, day > '
         read(*,'(a)',iostat=ios) buf
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input string'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
         read(buf,*,iostat=ios) iyr, mn, dy
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date'//char(7)
            write(*,*) '      IOSTAT =',ios
            err = .true.
         end if
         if (.not.err .and. (mn.lt.1 .or. mn.gt.12)) then
            write(*,*) '***** ERROR: Invalid month'//char(7)
            write(*,*) '      Must be in the range [1...12]'
            err = .true.
         end if
         if (.not.err .and. (dy.lt.1 .or. dy.ge.32)) then
            write(*,*) '***** ERROR: Invalid day'//char(7)
            write(*,*) '      Must be in the range [1...31]'
            err = .true.
         end if

         if (.not.err) call cal2jd (iyr, mn, dy, jd, err)
         if (.not.err) call jd2doy (jd, iyr, doy, err)
         if (.not.err) call jd2yr (jd, yr, err)
         if (.not.err) call jd2dow (jd, dow, err)
         if (.not.err) call jd2gps (jd, gpsweek, sow, err)
c
c  2 - Year, day of year
c
      else if (fmt.eq.2) then
         write(*,110) 'Enter year (incl century), day of year > '
         read(*,'(a)',iostat=ios) buf
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input string'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
         read(buf,*,iostat=ios) iyr, doy
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date'//char(7)
            write(*,*) '      IOSTAT =',ios
            err = .true.
         end if
         if (.not.err .and. (doy.lt.1 .or. doy.ge.367)) then
            write(*,*) '***** ERROR: Invalid day of year'//char(7)
            write(*,*) '      Must be in the range [1...366]'
            err = .true.
         end if

         if (.not.err) call doy2jd (iyr, doy, jd, err)
         if (.not.err) call jd2cal (jd, iyr, mn, dy, err)
         if (.not.err) call jd2yr (jd, yr, err)
         if (.not.err) call jd2dow (jd, dow, err)
         if (.not.err) call jd2gps (jd, gpsweek, sow, err)
c
c  3 - Year & decimal of year
c
      else if (fmt.eq.3) then
         write(*,110) 'Enter year with decimal (incl century) > '
         read(*,'(a)',iostat=ios) buf
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input string'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
         read(buf,*,iostat=ios) yr
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date'//char(7)
            write(*,*) '      IOSTAT =',ios
            err = .true.
         end if

         if (.not.err) call yr2jd (yr, jd, err)
         if (.not.err) call jd2cal (jd, iyr, mn, dy, err)
         if (.not.err) call jd2doy (jd, iyr, doy, err)
         if (.not.err) call jd2dow (jd, dow, err)
         if (.not.err) call jd2gps (jd, gpsweek, sow, err)
c
c  4 - GPS week, sec of week
c
      else if (fmt.eq.4) then
         write(*,110) 'Enter GPS week, sec of week > '
         read(*,'(a)',iostat=ios) buf
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input string'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
         read(buf,*,iostat=ios) gpsweek, sow
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date'//char(7)
            write(*,*) '      IOSTAT =',ios
            err = .true.
         end if
         if (.not.err .and. gpsweek.lt.0) then
            write(*,*) '***** ERROR: Invalid GPS week'//char(7)
            write(*,*) '      Must be greater or equal to 0'
            err = .true.
         end if
         if (.not.err .and. (sow.lt.0 .or. sow.ge.604800)) then
            write(*,*) '***** ERROR: Invalid sec of week'//char(7)
            write(*,*) '      Must be between 0 and 604800'
            err = .true.
         end if

         if (.not.err) call gps2jd (gpsweek, sow, jd, err)
         if (.not.err) call jd2cal (jd, iyr, mn, dy, err)
         if (.not.err) call jd2doy (jd, iyr, doy, err)
         if (.not.err) call jd2yr (jd, yr, err)
         if (.not.err) call jd2dow (jd, dow, err)
c
c  5 - Julian date
c
      else if (fmt.eq.5) then
         write(*,110) 'Enter input Julian date > '
         read(*,'(a)',iostat=ios) buf
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input string'//char(7)
            write(*,*) '      IOSTAT =',ios
            stop
         end if
         read(buf,*,iostat=ios) jd
         if (ios.ne.0) then
            write(*,*) '***** ERROR reading input date'//char(7)
            write(*,*) '      IOSTAT =',ios
            err = .true.
         end if
         if (.not.err .and. jd.lt.1) then
            write(*,*) '***** ERROR: Invalid Julian date'//char(7)
            write(*,*) '      Must be greater than 0'
            err = .true.
         end if

         if (.not.err) call jd2cal (jd, iyr, mn, dy, err)
         if (.not.err) call jd2doy (jd, iyr, doy, err)
         if (.not.err) call jd2yr (jd, yr, err)
         if (.not.err) call jd2dow (jd, dow, err)
         if (.not.err) call jd2gps (jd, gpsweek, sow, err)
c
c  Invalid date format
c
      else
         write(*,*) '***** ERROR: Undefined date format'//char(7)
         write(*,*) '      FMT =',fmt
         err = .true.
      end if
c
c  List results
c
      if (.not.err) then
         write(*,*)
         write(*,'(1x,a,i5,a,i2,a,f4.1)') 'Date   ',iyr,'-',mn,'-',dy
         write(*,'(1x,a,f10.4)') 'Year      ', yr
         write(*,'(1x,a,f6.1)')  'Day of Year   ', doy
         write(*,'(1x,a,3x,a)')  'Day of Week   ', day(dow)
         write(*,'(1x,a,i6)')    'GPS Week      ', gpsweek
         write(*,'(1x,a,i6)')    'Sec of Week   ', int(sow)
         write(*,'(1x,a,f9.1)')  'Julian Date   ', jd
      end if
      write(*,*)
c
c  Get another date
c
      end do ! while
      end

c*********************************************************************** cal2jd
c
      subroutine cal2jd (yr, mn, dy, jd, err)
c
c     Converts calendar date to Julian date using algorithm from
c     "Practical Ephemeris Calculations" by Oliver Montenbruck
c     (Springer-Verlag, 1989). Uses astronomical year for B.C. dates
c     (2 BC = -1 yr). Non-vectorized version. See also DOY2JD, GPS2JD,
c     JD2CAL, JD2DOW, JD2DOY, JD2GPS, JD2YR, YR2JD.
c
c     Input
c       yr - year of calendar date
c       mn - month of calendar date
c       dy - day of calendar date
c
c     Output
c       jd  - julian date
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer yr, mn
      real*8  dy, jd
c
c  Local specifications
c
      integer y, m, date, date1, date2, b
c-----------------------------------------------------------------------
c  Compute Julian date
c
      if (mn.gt.2) then
        y = yr
        m = mn
      else
        y = yr - 1
        m = mn + 12
      end if

      date  = int(dy)+31*(mn+12*yr)
      date1 = 4+31*(10+12*1582)   ! Last day of Julian calendar (1582.10.04)
      date2 = 15+31*(10+12*1582)  ! First day of Gregorian calendar (1582.10.15)

      err = .false.
      if (date.le.date1) then
        b = -2
      else if (date.ge.date2) then
        b = int(y/400.d0) - int(y/100.d0)
      else
        write(*,*) '***** ERROR: Invalid date in CAL2JD'//char(7)
        write(*,*) '      Dates between October 5 & 15, 1582 do not '//
     +             'exist'
        write(*,*) '      YR =',yr
        write(*,*) '      MN =',mn
        write(*,*) '      DY =',dy
        err = .true.
        return
      end if

      if (y.gt.0) then
        jd = int(365.25d0*y) + int(30.6001d0*(m+1)) + b + 1720996.5d0 +
     +       dy
      else
        jd = int(365.25d0*y-0.75d0) + int(30.6001d0*(m+1)) + b +
     +       1720996.5d0 + dy
      end if
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** doy2jd
c
      subroutine doy2jd (yr, doy, jd, err)
c
c     Converts year and day of year to Julian date. See also CAL2JD,
c     GPS2JD, JD2CAL, JD2DOW, JD2DOY, JD2GPS, JD2YR, YR2JD.
c
c     Input
c       yr  - year of calendar date
c       doy - day of year
c
c     Output
c       jd  - julian date
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer yr
      real*8  doy, jd
c-----------------------------------------------------------------------
c  Compute Julian date
c
      call cal2jd (yr, 1, 0.d0, jd, err)
      if (err) return
      jd = jd + doy
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** gps2jd
c
      subroutine gps2jd (gpsweek, sow, jd, err)
c
c     Converts GPS week (since 1980.01.06) and seconds of week to Julian
c     date. See also CAL2JD, DOY2JD, JD2CAL, JD2DOW, JD2DOY, JD2GPS,
c     JD2YR, YR2JD.
c
c     Input
c       gpsweek  - GPS week number beginning with 0 on 1980.1.6
c       sow      - seconds of week since 0 hr, Sun
c
c     Output
c       jd  - julian date
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c       09 Jul 99  MRC  Corrected GPS week number to begin at 0
c       11 Jun 04  MRC  Removed computation of GPS week rollover
c
c     Copyright (c) 1999-2004 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer gpsweek
      real*8  sow, jd
c
c  Local specifications
c
      real*8  jdgps
c-----------------------------------------------------------------------
c  Check for invalid GPS week
c
      err = .false.
      if (gpsweek.lt.0) then
        write(*,*) '***** ERROR: Invalid GPS week in GPS2JD'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      GPSWEEK =',gpsweek
        err = .true.
        return
      end if
c
c  Compute Julian date
c
      call cal2jd (1980,1, 6.d0, jdgps, err) ! beginning of GPS week numbering
      if (err) return
      jd = jdgps + gpsweek*7.d0 + sow/3600.d0/24.d0
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** jd2cal
c
      subroutine jd2cal (jd, yr, mn, dy, err)
c
c     Converts Julian date to calendar date using algorithm from
c     "Practical Ephemeris Calculations" by Oliver Montenbruck
c     (Springer-Verlag, 1989). Must use astronomical year for B.C.
c     dates (2 BC = -1 yr). See also CAL2JD, DOY2JD, GPS2JD, JD2DOW,
c     JD2DOY, JD2GPS, JD2YR, YR2JD.
c
c     Input
c       jd - julian date
c
c     Output
c       yr  - year of calendar date
c       mn  - month of calendar date
c       dy  - day of calendar date (including decimal)
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer yr, mn
      real*8  dy, jd
c
c  Local specifications
c
      integer a, b, c, d, e, f
c-----------------------------------------------------------------------
c  Check for valid Julian date
c
      err = .false.
      if (jd.lt.0) then
        write(*,*) '***** ERROR: Invalid Julian date in JD2CAL'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      JD =',jd
        err = .true.
        return
      end if
c
c  Compute calendar date
c
      a = int(jd+0.5d0)
      if (a.lt.2299161) then
        c = a + 1524
      else
        b = int( (a-1867216.25d0) / 36524.25d0 )
        c = a + b - int(b/4d0) + 1525
      end if
      d = int( (c-122.1d0)/365.25d0 )
      e = int(365.25d0*d)
      f = int( (c-e) / 30.6001d0 )
      dy = c - e - int(30.6001*f) + mod((jd+0.5d0),a)
      mn = f - 1 - 12*int(f/14d0)
      yr = d - 4715 - int( (7+mn)/10.d0 )
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** jd2dow
c
      subroutine jd2dow (jd, dow, err)
c
c     Converts Julian date to day of week (Sun=1,...,Sat=7). Adapted
c     from the algorithm from "Practical Ephemeris Calculations" by
c     Oliver Montenbruck (Springer-Verlag, 1989). Must use astronomical
c     year for B.C. dates (2 BC = -1 yr). See also CAL2JD, DOY2JD,
c     GPS2JD, JD2CAL, JD2DOY, JD2GPS, JD2YR, YR2JD.
c
c     Input
c       jd - julian date
c
c     Output
c       dow - day of week (1=Sun,...,7=Sat)
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer dow
      real*8  jd
c-----------------------------------------------------------------------
c  Check for valid Julian date
c
      err = .false.
      if (jd.lt.0) then
        write(*,*) '***** ERROR: Invalid Julian date in JD2CAL'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      JD =',jd
        err = .true.
        return
      end if
c
c  Compute day of week
c
      dow = int(mod(jd+1.5d0,7)) + 1
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** jd2doy
c
      subroutine jd2doy (jd, yr, doy, err)
c
c     Converts Julian date to year and day of year. See also CAL2JD,
c     DOY2JD, GPS2JD, JD2CAL, JD2DOW, JD2GPS, JD2YR, YR2JD.
c
c     Input
c       jd - julian date
c
c     Output
c       yr  - year
c       doy - day of year (including decimal)
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd, jd2cal
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer yr
      real*8  doy, jd
c
c  Local specifications
c
      integer mn
      real*8  dy, jd0
c-----------------------------------------------------------------------
c  Check for valid Julian date
c
      err = .false.
      if (jd.lt.0) then
        write(*,*) '***** ERROR: Invalid Julian date in JD2CAL'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      JD =',jd
        err = .true.
        return
      end if
c
c  Compute year & day of year
c
      call jd2cal (jd, yr, mn, dy, err)
      if (err) stop
      call cal2jd (yr, 1, 0.d0, jd0, err)
      if (err) stop
      doy = jd - jd0
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** jd2gps
c
      subroutine jd2gps (jd, gpsweek, sow, err)
c
c     Converts Julian date to GPS week, seconds of week. See also
c     CAL2JD, DOY2JD, GPS2JD, JD2CAL, JD2DOW, JD2DOY, JD2GPS, JD2YR,
c     YR2JD.
c
c     Input
c       jd - julian date
c
c     Output
c       gpsweek  - GPS week beginning with 0 on 1980.1.6
c       sow      - seconds of week since 0 hr, Sun
c       err      - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd, jd2cal
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c       09 Jul 99  MRC  Corrected GPS week number to begin at 0
c       11 Jun 04  MRC  Removed computation of GPS week rollover
c
c     Copyright (c) 1999-2004 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      integer gpsweek
      real*8  jd, sow
c
c  Local specifications
c
      real*8  jdgps
c-----------------------------------------------------------------------
c  Check for valid Julian date
c
      err = .false.
      if (jd.lt.0) then
        write(*,*) '***** ERROR: Invalid Julian date in JD2CAL'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      JD =',jd
        err = .true.
        return
      end if
c
c  Compute GPS week and seconds of week
c
      call cal2jd (1980, 1, 6.d0, jdgps, err) ! beginning of GPS week numbering
      if (err) return
      gpsweek = int((jd-jdgps)/7.d0)
      sow = (jd - (jdgps+gpsweek*7.d0)) * 3600.d0*24.d0
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** jd2yr
c
      subroutine jd2yr (jd, yr, err)
c
c     Converts Julian date to year and decimal of year. See also CAL2JD,
c     DOY2JD, GPS2JD, JD2CAL, JD2DOW, JD2DOY, JD2GPS, YR2JD.
c
c     Input
c       jd - julian date
c
c     Output
c       yr  - year and decimal of year
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd, jd2cal
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      real*8  jd, yr
c
c  Local specifications
c
      integer iyr, mn
      real*8  dy, jd0, jd1
c-----------------------------------------------------------------------
c  Check for valid Julian date
c
      err = .false.
      if (jd.lt.0) then
        write(*,*) '***** ERROR: Invalid Julian date in JD2CAL'//char(7)
        write(*,*) '      Must be greater than or equal to zero'
        write(*,*) '      JD =',jd
        err = .true.
        return
      end if
c
c  Compute year and decimal year
c

      call jd2cal (jd, iyr, mn, dy, err)
      if (err) return
      call cal2jd (iyr, 1, 1.d0, jd0, err)
      if (err) return
      call cal2jd (iyr+1, 1, 1.d0, jd1, err)
      if (err) return
      yr = iyr + (jd-jd0)/(jd1-jd0)
c
c  Return to calling routine
c
      return
      end

c*********************************************************************** yr2jd
c
      subroutine yr2jd (yr, jd, err)
c
c     Converts year and decimal of year to Julian date. See also CAL2JD,
c     DOY2JD, GPS2JD, JD2CAL, JD2DOW, JD2DOY, JD2GPS, JD2YR.
c
c     Input
c       yr - year and decimal of year
c
c     Output
c       jd  - julian date
c       err - logical error flag (.true.=error, .false.=no error)
c
c     Externals
c       cal2jd, day2jd
c
c     Created
c       25 Apr 99  Michael R. Craymer
c                  Converted from MATLAB function of same name.
c
c     Modified
c
c     Copyright (c) 1999 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical err
      real*8  yr, jd
c
c  Local specifications
c
      integer iyr
      real*8  jd0, jd1, days, doy
c-----------------------------------------------------------------------
c  Compute Julian date
c
      iyr = int(yr)
      call cal2jd (iyr, 1, 1.d0, jd0, err)
      if (err) return
      call cal2jd (iyr+1, 1, 1.d0, jd1, err)
      if (err) return
      days = jd1 - jd0
      doy = (yr-iyr)*days + 1.d0
      call doy2jd (iyr, doy, jd, err)
      if (err) return
c
c  Return to calling routine
c
      return
      end
