c***********************************************************************
c
      program HLTables
c
c     Creates a file of house league score sheets/tables for printing.
c     Uses a sorted (ranked) list of players for input.
c
c     Input:
c       User - Max. number of players per house (default = 5)
c            - Starting house number (default = 1)
c       File - Sorted (ranked) list of players, each record
c              corresponding to a single player in a house and each
c              house separated by a blank or comment record (* in
c              column one).  Each record contains players last name,
c              initial, home phone, work phone, each separated by
c              a comma, tab or space(s).  Player names and phones must
c              contain no blanks or commas!  Last names longer than
c              10 characters will be truncated.
c
c     Output:
c       Scrn - Players in each house
c       File - List of house league score sheets
c
c     Created:
c       27 Jan 93  Michael R. Craymer
c
c     Modified:
c       1993-08-22  MRC  Version 1.0
c                        Changed month and year variables to one date
c                        variable for table header in sub. dialog and
c                        wrplayers
c                        Changed to list lastname first in tables in sub
c                        wrplayers
c                        Added rankings to output tables in sub.
c                        wrplayers
c                        Added new sub. wrrankings to create output file
c                        of player rankings
c       1994-01-03  MRC  Changed output file of player rankings to a
c                        house league player directory in sub.
c                        wrdirectory (renamed from wrrankings)
c                        Removed nh1 from sub. wrplayers
c                        Changed prompt for output players directory
c                        file (from output rankings) in sub. dialog
c       2005-09-16  MRC  Version 1.1
c                        Increased ver variable from 9 to 20 chars.
c                        Added comma after last name in sub rdplayers
c                        and modified sub wrdirectory accordingly.
c                        Corrected output of blank row in table in sub
c                        wrplayers.
c                        Explicitly added space in front of last name in
c                        table header in sub wrplayers.
c       2006-11-03  MRC  Version 1.2
c                        Increased telh & telw from 10 to 14 for ten-
c                        ten-digit tel numbers.
c                        Increased width of Name column in output table
c                        file to handle longer names and ten-digit tel
c                        numbers.
c
c     Externals:
c
c     Copyright (c) 1993-2006 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit  none
c
c  Parameter specifications
c
      integer   maxnp
      parameter (maxnp = 5)
c
c  Variable specifications
c
      logical   eof
      character ifile*32, ofile1*32, ofile2*32, ver*20,
     +          firstname(maxnp)*15, lastname(maxnp)*15,
     +          telh(maxnp)*14, telw(maxnp)*14,
     +          date*20
      integer   lui, luo1, luo2, nh1, nh, np, nr
      data      lui/11/, luo1/12/, luo2/13/, ver/'1.2 (2006-11-03)'/
c
c  Get user input
c
      call dialog (ver, lui, luo1, luo2, ifile, ofile1, ofile2,
     +             nh1, date)
c
c  For each house, read and list players
c
      eof = .false.
      nh  = nh1
      nr  = 1
 10   continue
         call rdplayers (lui, maxnp, nh, nr, np, firstname, lastname,
     +                   telh, telw, eof)
         if (eof) then
            write(*,*)
c            write(*,*) 'End of file'
c            write(*,*) 'Press CR to exit...'
c            read(*,*)
            stop 'End of file...Finished'
         end if
         call wrplayers (luo1, nh, nr, np, date, firstname, lastname,
     +                   telh, telw)
         call wrdirectory (luo2, nh, maxnp, nr, date, firstname,
     +                     lastname, telh, telw)
         nr = nr + np
         nh = nh + 1
      goto 10
c
c  End program
c
      end

c***********************************************************************
c
      subroutine dialog (ver, lui, luo1, luo2, ifile, ofile1, ofile2,
     +                   nh1, date)
c
c     Reads user input from keyboard.
c
c     Input:
c       ver
c       lui
c       luo1
c       luo2
c
c     Output:
c       ifile
c       ofile1
c       ofile2
c       nh1
c       date
c
c     Created:
c       1993-01-27  Michael R. Craymer
c
c     Modified:
c       1993-08-22  MRC  Changed month and year variables to a one
c                        date variable.
c                        Added output file for rankings list
c       1994-01-03  MRC  Changed prompt for output players directory
c                        file (from output rankings).
c
c     Externals:
c
c     Copyright (c) 1993-1994 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit  none
c
c  Dummy specifications
c
      character ver*(*), ifile*(*), ofile1*(*), ofile2*(*), date*(*)
      integer   lui, luo1, luo2, nh1
c
c  Local specifications
c
      logical   ex
      character reply, buf*80, tab
      integer   ios, ibeg, iend

      tab = char(9)

c-----------------------------------------------------------------------
c
c  Program banner
c
      write(*,*)
      write(*,*) '---------------------------------------------------',
     +           '----------------------------'
      write(*,*) ' HLTables: Creates house league score sheets ',
     +           '(tables)'
      write(*,*) '           Version ',ver
      write(*,*) '---------------------------------------------------',
     +           '----------------------------'
      write(*,*)
c
c  Prompt format
c
c100  format(a,$)
 100  format(1x,a,$)
c100  format(1x,a)
c
c  Get input file name of players and phone numbers
c
 10   continue
      write(*,100) 'Enter file name for input list of players [quit] > '
      read(*,'(a)') ifile

      if (ifile.eq.' ') then
         stop ' '
      end if
      inquire(file=ifile,exist=ex)
      if (ex) then
         open(lui,file=ifile,status='old',iostat=ios)
         if (ios.ne.0) then
            write(*,*) '***** Error opening file ',ifile,char(7)
            write(*,*) '      IOSTAT = ',ios
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
      else
         write(*,*) '***** Warning: File does not exist',char(7)
         goto 10
      end if
c
c  Get output file name for house score sheets
c
 20   continue
      write(*,100) 
     +  'Enter file name for output house score sheets/tables [quit] > '
      read(*,'(a)') ofile1

      if (ofile1.eq.' ') then
         stop ' '
      else
         inquire(file=ofile1,exist=ex)
         if (ex) then
            write(*,100)
     +    '***** Warning: File already exists -- overwrite it? (y,N) > '
            read(*,'(a)') reply
            if (reply.eq.'y' .or. reply.eq.'Y') then
               open(luo1,file=ofile1,status='old',iostat=ios)
               if (ios.ne.0) then
                  write(*,*) '***** Error opening file ',ofile1,char(7)
                  write(*,*) '      IOSTAT = ',ios
                  write(*,*) 'Press CR to exit...'
                  read(*,*)
                  stop ' '
               end if
               close(luo1,status='delete')
            else
               goto 20
            end if
         end if        

         open(luo1,file=ofile1,status='new',iostat=ios)
         if (ios.ne.0) then
            write(*,*) '***** Error opening file ',ofile1,char(7)
            write(*,*) '      IOSTAT = ',ios
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
      end if
c
c  Get output file name for player directory list
c
 25   continue
      write(*,100) 
     +  'Enter file name for output players directory [quit] > '
c    +  'Enter file name for output rankings list [quit] > '
      read(*,'(a)') ofile2

      if (ofile2.eq.' ') then
         stop ' '
      else
         inquire(file=ofile2,exist=ex)
         if (ex) then
            write(*,100)
     +    '***** Warning: File already exists -- overwrite it? (y,N) > '
            read(*,'(a)') reply
            if (reply.eq.'y' .or. reply.eq.'Y') then
               open(luo2,file=ofile2,status='old',iostat=ios)
               if (ios.ne.0) then
                  write(*,*) '***** Error opening file ',ofile2,char(7)
                  write(*,*) '      IOSTAT = ',ios
                  write(*,*) 'Press CR to exit...'
                  read(*,*)
                  stop ' '
               end if
               close(luo2,status='delete')
            else
               goto 25
            end if
         end if        

         open(luo2,file=ofile2,status='new',iostat=ios)
         if (ios.ne.0) then
            write(*,*) '***** Error opening file ',ofile2,char(7)
            write(*,*) '      IOSTAT = ',ios
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
      end if
c
c  Get date of league
c
 30   continue
      write(*,100) 'Enter date of play (month days, year) [quit] > '
      read(*,'(a)') buf

      if (buf.eq.' ') then
         stop ' '
      end if

      ibeg = 1
      do while (buf(ibeg:ibeg).eq.' ')
         ibeg = ibeg + 1
      end do

      iend = len(buf)
      do while (buf(iend:iend).ne.' ')
         iend = iend - 1
      end do

      date = buf(ibeg:iend)
      call upcase (date)
c
c  Get starting house number
c
      nh1 = 1
c     write(*,100) 'Enter starting house number [1] > '
c     read(*,'(a)') buf
c
c     if (buf.eq.' ') then
c        nh1 = 1
c     else
c        read(buf,*) nh1
c     end if
c
c  Return to calling routine
c
      return
      end

c***********************************************************************
c
      subroutine rdplayers (lui, maxnp, nh, nr, np, firstname, lastname,
     +                      telh, telw, eof)
c
c     Read players in a house from input file.
c
c     Input:
c       lui
c       maxnp
c       nh
c       nr
c
c     Output:
c       np
c       firstname
c       lastname
c       telh
c       telw
c       eof
c
c     Created:
c       1993-01-27  Michael R. Craymer
c
c     Modified:
c       2005-09-16  MRC  Added comma at end of last name.
c       2006-11-03  MRC  Modified to handle ten-digit tel numbers
c
c     Externals:
c
c     Copyright (c) 1993-2006 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      logical   eof
      character lastname(*)*(*), firstname(*)*(*), telh(*)*(*),
     +          telw(*)*(*)
      integer   lui, maxnp, nh, nr, np
c
c  Local specifications
c
      character buf*79, tab
      integer   ios, ibeg, iend, i

      tab = char(9)

c-----------------------------------------------------------------------
c
c  Initialize arrays
c
      do i = 1, maxnp
         firstname(i) = ' '
         lastname(i) = ' '
         telh(i) = ' '
         telw(i) = ' '
      end do
      buf = ' '
c
c  Find first player in house
c
      do while (buf.eq.' ' .or. buf(1:1).eq.'*')
         read(lui,'(a)',iostat=ios) buf
         if (ios.lt.0) then
            write(*,*)
            write(*,*) 'End of file'
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         else if (ios.gt.0) then
            write(*,*) '***** Error reading input file',char(7)
            write(*,*) '      IOSTAT = ',ios
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
      end do
c
c  Echo house number
c
      write(*,*)
      write(*,'(1x,a,i3,a)') 'Creating table for house ', nh, '...'
c
c  Read player data from input record
c
      np = 0
      do while (buf.ne.' ' .and. buf(1:1).ne.'*')
         np = np + 1
c
c  Get last name
c
         ibeg = 1
         do while (buf(ibeg:ibeg).eq.' ' .or.
     +             buf(ibeg:ibeg).eq.',' .or.
     +             buf(ibeg:ibeg).eq.tab)
            ibeg = ibeg + 1
         end do
         iend = ibeg
         do while (buf(iend+1:iend+1).ne.' ' .and.
     +             buf(iend+1:iend+1).ne.',' .and.
     +             buf(iend+1:iend+1).ne.tab)
            iend = iend + 1
         end do
         lastname(np) = buf(ibeg:iend)//','
c
c  Get first name
c
         ibeg = iend + 1
         do while (buf(ibeg:ibeg).eq.' ' .or.
     +             buf(ibeg:ibeg).eq.',' .or.
     +             buf(ibeg:ibeg).eq.tab)
            ibeg = ibeg + 1
         end do
         iend = ibeg
         do while (buf(iend+1:iend+1).ne.' ' .and.
     +             buf(iend+1:iend+1).ne.',' .and.
     +             buf(iend+1:iend+1).ne.tab)
            iend = iend + 1
         end do
         firstname(np) = buf(ibeg:iend)
c
c  Get phone number (home)
c
         ibeg = iend + 1
         do while (buf(ibeg:ibeg).eq.' ' .or.
     +             buf(ibeg:ibeg).eq.',' .or.
     +             buf(ibeg:ibeg).eq.tab)
            ibeg = ibeg + 1
         end do
         iend = ibeg
         do while (buf(iend+1:iend+1).ne.' ' .and.
     +             buf(iend+1:iend+1).ne.',' .and.
     +             buf(iend+1:iend+1).ne.tab)
            iend = iend + 1
         end do
         telh(np) = buf(ibeg:iend)
         telh(np) = telh(np)(1:iend-ibeg+1)//' h'
c
c  Get phone number (work)
c
         ibeg = iend + 1
         do while ((buf(ibeg:ibeg).eq.' ' .or.
     +              buf(ibeg:ibeg).eq.',' .or.
     +              buf(ibeg:ibeg).eq.tab) .and.
     +             ibeg.lt.79)
            ibeg = ibeg + 1
         end do
         iend = ibeg
         do while (buf(iend+1:iend+1).ne.' ' .and.
     +             buf(iend+1:iend+1).ne.',' .and.
     +             buf(iend+1:iend+1).ne.tab .and.
     +             iend.ne.79)
            iend = iend + 1
         end do
         telw(np) = buf(ibeg:iend)
         telw(np) = telw(np)(1:iend-ibeg+1)//' w'
c
c  Echo input to screen
c
         write(*,100) nr+np-1, lastname(np), firstname(np),
     +                telh(np), telw(np)
 100     format(1x,i2,4(2x,a))
c
c  Check if too many players
c
         if (np.gt.maxnp) then
            write(*,*) '***** Error: Too many players in this house',
     +                char(7)
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
c
c  Get next input record
c
         read(lui,'(a)',iostat=ios) buf

         if (ios.lt.0) then
            eof = .true.
            return
         else if (ios.gt.0) then
            write(*,*) '***** Error reading input file',char(7)
            write(*,*) '      IOSTAT = ',ios
            write(*,*) 'Press CR to exit...'
            read(*,*)
            stop ' '
         end if
      end do          ! end do while player record
c
c  Return to calling routine
c
      return
      end

c***********************************************************************
c
      subroutine wrplayers (luo, nh, nr, np, date, firstname, lastname,
     +                      telh, telw)
c
c     Write house score sheet.
c
c     Input:
c       luo
c       nh
c       nr
c       np
c       date
c       firstname
c       lastname
c       telh
c       telw
c
c     Output:
c       File - house score sheet
c
c     Created:
c       27 Jan 93  Michael R. Craymer
c
c     Modified:
c       1993-08-22  MRC  Changed format for new date variable replacing
c                        month and year variable in table header.
c                        Changed to list lastname first in tables in sub
c                        wrplayers.
c                        Added ranking number to table.
c       1994-01-03  MRC  Removed variable nh1.
c       2005-09-16  MRC  Corrected output of blank row in table (extra
c                        blank line needed for player ranking numer).
c       2006-11-03  MRC  Increased width of first column of output table
c                        by 4 chars to handle longer names and ten-digit
c                        tel numbers.
c
c     Externals:
c
c     Copyright (c) 1993-2006 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      character lastname(*)*(*), firstname(*)*(*), telh(*)*(*),
     +          telw(*)*(*), date*(*)
      integer   luo, nh, nr, np
c
c  Local specifications
c
      integer   i

c-----------------------------------------------------------------------
c  Create house score sheet
c
      if (nh.eq.1) write(luo,*)
      if (mod(nh,2).eq.1) then
         if (nh.ne.1) write(luo,'(a)') char(12)
      else
         write(luo,*)
      end if
c
c  Header
c
      write(luo,100)
      write(luo,101)
      write(luo,102) nh, date
      write(luo,101)
      write(luo,103)
      write(luo,104) (lastname(i),i=1,5)
      write(luo,105) (firstname(i),i=1,5)
      write(luo,103)
c
c  Player 1
c
      write(luo,112) nr
      write(luo,110) lastname(1)
      write(luo,110) firstname(1)
      write(luo,111) telh(1)
      write(luo,111) telw(1)
      write(luo,106)
c
c  Player 2
c
      if (np.ge.2) then
         write(luo,122) nr + 1
      else
         write(luo,123)
      end if
      write(luo,120) lastname(2)
      write(luo,120) firstname(2)
      write(luo,121) telh(2)
      write(luo,121) telw(2)
      write(luo,106)
c
c  Player 3
c
      if (np.ge.3) then
         write(luo,132) nr + 2
      else
         write(luo,133)
      end if
      write(luo,130) lastname(3)
      write(luo,130) firstname(3)
      write(luo,131) telh(3)
      write(luo,131) telw(3)
      write(luo,106)
c
c  Player 4
c
      if (np.ge.4) then
         write(luo,142) nr + 3
      else
         write(luo,143)
      end if
      write(luo,140) lastname(4)
      write(luo,140) firstname(4)
      write(luo,141) telh(4)
      write(luo,141) telw(4)
      write(luo,106)
c
c  Player 5
c
      if (np.eq.5) then
         write(luo,152) nr + 4
      else
         write(luo,153)
      end if
      write(luo,150) lastname(5)
      write(luo,150) firstname(5)
      write(luo,151) telh(5)
      write(luo,151) telw(5)
      write(luo,100)

c-----------------------------------------------------------------------
c
c  Format statements
c  Header
c
 100  format('-------------------------------------------------------',
     +       '----------------------------')
 101  format('|                                                      ',
     +       '                           |')
c102  format('|                               H O U S E  ',i2,'      ',
c    +       '                               |')
 102  format('| H O U S E   ',i2,'                                   ',
     +       '          ',a20,' |')
 103  format('|------------------------------------------------------',
     +       '---------------------------|')
 104  format('| NAME           | ',a11,'| ',a11,'| ',a11,'| ',a11,'| ',
     +       a11,'|')
 105  format('|                | ',a11,'| ',a11,'| ',a11,'| ',a11,'| ',
     +       a11,'|')
 106  format('|----------------|------------+------------+------------',
     +       '+------------+------------|')
c
c  Player 1
c
 110  format('| ',a15, '|x x x x x x |            |            |  ',
     +       '          |            |')
 111  format('| ',a14,' |x x x x x x |            |            |  ',
     +       '          |            |')
 112  format('| ',i14,' |x x x x x x |            |            |  ',
     +       '          |            |')
c
c  Player 2
c
 120  format('| ',a15, '|            |x x x x x x |            |  ',
     +       '          |            |')
 121  format('| ',a14,' |            |x x x x x x |            |  ',
     +       '          |            |')
 122  format('| ',i14,' |            |x x x x x x |            |  ',
     +       '          |            |')
 123  format('|                |            |x x x x x x |            ',
     +       '|            |            |')
c
c  Player 3
c
 130  format('| ',a15, '|            |            |x x x x x x |  ',
     +       '          |            |')
 131  format('| ',a14,' |            |            |x x x x x x |  ',
     +       '          |            |')
 132  format('| ',i14,' |            |            |x x x x x x |  ',
     +       '          |            |')
 133  format('|                |            |            |x x x x x x ',
     +       '|            |            |')
c
c  Player 4
c
 140  format('| ',a15, '|            |            |            |x ',
     +       'x x x x x |            |')
 141  format('| ',a14,' |            |            |            |x ',
     +       'x x x x x |            |')
 142  format('| ',i14,' |            |            |            |x ',
     +       'x x x x x |            |')
 143  format('|                |            |            |            ',
     +       '|x x x x x x |            |')
c
c  Player 5
c
 150  format('| ',a15, '|            |            |            |  ',
     +       '          |x x x x x x |')
 151  format('| ',a14,' |            |            |            |  ',
     +       '          |x x x x x x |')
 152  format('| ',i14,' |            |            |            |  ',
     +       '          |x x x x x x |')
 153  format('|                |            |            |            ',
     +       '|            |x x x x x x |')

c-----------------------------------------------------------------------------------
c|                                                                                 |
c| H O U S E   nn                                                             DATE |
c|                                                                                 |
c|---------------------------------------------------------------------------------|
c| NAME           | First      | First      | First      | First      | First      |
c|                | Lastname   | Lastnameeee| Lastname   | Lastname   | Lastname   |
c|---------------------------------------------------------------------------------|
c| First          |x x x x x x |            |            |            |            |
c| Lastname       |x x x x x x |            |            |            |            |
c| 123-456-7890 h |x x x x x x |            |            |            |            |
c| 123-456-7890 w |x x x x x x |            |            |            |            |
c|----------------|------------+------------+------------+------------+------------|
c| First          |            |x x x x x x |            |            |            |
c| Lastname       |            |x x x x x x |            |            |            |
c| 123-456-7890 h |            |x x x x x x |            |            |            |
c| 123-456-7890 w |            |x x x x x x |            |            |            |
c|----------------|------------+------------+------------+------------+------------|
c| First          |            |            |x x x x x x |            |            |
c| Lastname       |            |            |x x x x x x |            |            |
c| 123-456-7890 h |            |            |x x x x x x |            |            |
c| 123-456-7890 w |            |            |x x x x x x |            |            |
c|----------------|------------+------------+------------+------------+------------|
c| First          |            |            |            |x x x x x x |            |
c| Lastname       |            |            |            |x x x x x x |            |
c| 123-456-7890 h |            |            |            |x x x x x x |            |
c| 123-456-7890 w |            |            |            |x x x x x x |            |
c|----------------|------------+------------+------------+------------+------------|
c| First          |            |            |            |            |x x x x x x |
c| Lastname       |            |            |            |            |x x x x x x |
c| 123-456-7890 h |            |            |            |            |x x x x x x |
c| 123-456-7890 w |            |            |            |            |x x x x x x |
c-----------------------------------------------------------------------------------

c
c  Return to calling routine
c
      return
      end

c***********************************************************************
c
      subroutine wrdirectory (luo, nh, maxnp, nr, date, firstname,
     +                        lastname, telh, telw)
c
c     Write rankings list.
c
c     Input:
c       luo
c       nh
c       maxnp
c       nr
c       date
c       firstname
c       lastname
c       telh
c       telw
c
c     Output:
c       File - rankings list
c
c     Created:
c       1993-08-22  Michael R. Craymer
c
c     Modified:
c       1994-01-03  MRC  Changed output file to a house league player
c                        diretory and renamed routine to wrdirectory
c                        Added tabs to output listing for easier
c                        in Word
c       2005-09-16  MRC  Modified for comma included within lastname.
c       2006-11-03  MRC  Modified format statements to handle longer
c                        names and ten-digit tel numbers.
c                        *** Only outputs 7-digit tel numbers ***
c                            (ignores first 5 chars of number)
c
c     Externals:
c
c     Copyright (c) 1993-2006 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      character lastname(*)*(*), firstname(*)*(*), telh(*)*(*),
     +          telw(*)*(*), date*(*)
      integer   luo, nh, maxnp, nr
c
c  Local specifications
c
      character name*25, tab
      integer   i, iend

      tab = char(9)
c-----------------------------------------------------------------------
c  Write header for rankings list
c
      if (nr.eq.1) then
c        write(luo,100)
c        write(luo,101)
         write(luo,102) date
c        write(luo,101)
         write(luo,*)
         write(luo,*) 'To print in Word use: 0.75 l/r margins, ',
     +                '2 columns in section 2'
         write(luo,*) 'Times 14 pt title, 10 pt body.'
         write(luo,100)
      end if
c
c  For each player, write ranking, names and numbers
c
      write(luo,*)
      write(luo,104) nh
      write(luo,*)
      do i = 1, maxnp
         if (lastname(i).ne.' ') then
            iend = len(lastname(i))
            do while (lastname(i)(iend:iend).eq.' ')
               iend = iend - 1
            end do
c           name = lastname(i)(1:iend)//',  '//firstname(i)
c           write(luo,103) nr+i-1, name, telh(i), telw(i)
c           write(luo,103) nr+i-1, name, tab, telh(i), tab, telw(i)
            name = lastname(i)(1:iend)//' '//firstname(i)
            if (telh(i)(1:1).eq.'?') then
               write(luo,103) nr+i-1, name, tab, ' ', tab, telw(i)
            else if (telw(i)(1:1).eq.'?') then
               write(luo,103) nr+i-1, name, tab, telh(i), tab, ' '
            else
               write(luo,103) nr+i-1, name, tab, telh(i), tab, telw(i)
            end if
         end if
      end do

c-----------------------------------------------------------------------
c
c  Format statements
c  Header
c
c100  format('-------------------------------------------------------',
c    +       '----------------------------')
c101  format('|                                                      ',
c    +       '                           |')
c102  format('| H O U S E   L E A G U E   R A N K I N G S            ',
c            '      ',a20,' |')

 100  format('___________________________________________________',
     +       '_________________________')
 102  format('SQUASH HOUSE LEAGUE DIRECTORY                 ',
     +       '         ',a20)
c103  format(i4,'. ',a20,5x,a14,5x,a14)
 103  format(i4,'. ',a20,a,a14,a,a14)
 104  format('HOUSE ',i2)

c
c  Return to calling routine
c
      return
      end

c***********************************************************************
c
      subroutine upcase (string)
c
c     Converts string to upcase characters.
c
c     Input:
c       string
c
c     Output:
c       string
c
c     Created:
c       29 Jan 93  Michael R. Craymer
c
c     Modified:
c
c     Externals:
c
c     Copyright (c) 1993 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c
      character string*(*)
c
c  Local specifications
c
      integer   i
c
c  Convert each character in string to upper case
c
      do i = 1, len(string)
         if (string(i:i).ge.'a' .and. string(i:i).le.'z') then
            string(i:i) = char( ichar(string(i:i)) - 32 )
         end if
      end do
c
c  Return to calling routine
c
      return
      end

c***********************************************************************
c
      subroutine dummy
c
c     
c
c     Input:
c
c     Output:
c
c     Created:
c       ?? ??? ??  Michael R. Craymer
c
c     Modified:
c
c     Externals:
c
c     Copyright (c) 1993 Michael R. Craymer
c
c---+*---1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
c
c  Dummy specifications
c

c
c  Local specifications
c

c
c  Return to calling routine
c
      return
      end
