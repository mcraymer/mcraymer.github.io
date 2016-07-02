c***********************************************************************
c
      program cdf
c
c     CDF (Cummulative Density Functions) computes CDF for various
c     probability distributions.  Based on the public domain stats
c     library DCDFLIB v1.1 (Nov 97).
c
c     Created:
c       19 Oct 96  Michael R. Craymer
c
c     Modified:
c       21 Oct 96  MRC  Commented out inverse T distribution (doesn't
c                       work)
c       03 Jul 00  MRC  Updated with new v1.1 of DCDFLIB libarary
c                       Added ability to quit from data entry to menu
c
c     Externals:
c       DCDFLIB Library v1.1
c
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-*
      implicit none
      character ver*15, libver*15, opt*2, buf*10
      integer status
      real*8 x, p, q, df, dfn, dfd, bound
      data ver/'1.1 (00.07.03)'/, libver/'1.1 (Nov 97)'/
 100  format(1x,a,$)

      write(*,*) '---------------------------------------------------'
      write(*,*) ' CDF: Cumulative Distribution Functions & Inverses'
      write(*,*) '      Based on DCDFLIB Version '//libver
      write(*,*) '      M.Craymer, Version '//ver
      write(*,*) '---------------------------------------------------'

 10   continue
      write(*,*)
      write(*,*) 'Distribution Solutions:'
      write(*,*) 'ND) Normal Direct        Nl) Normal Inverse'
      write(*,*) 'TD) Student t Direct     TI) Student t Inverse'
      write(*,*) 'CD) Chi-Square Direct    CI) Chi-Square Inverse'
      write(*,*) 'FD) Fisher Direct        FI) Fisher Inverse'
      write(*,*)
      write(*,100) 'Select solution code (Q=quit) > '
      read(*,'(a)') opt

      if (opt.eq.'Q' .or. opt.eq.'q' .or. opt.eq.' ') then
         write(*,*)
         goto 999
*        stop 'CDF finished'
c
c  Normal dist'n -- direct
c
      else if (opt.eq.'ND' .or. opt.eq.'nd') then
         write(*,100) 'Enter critical limit > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) x
         p = 0.d0
         q = 1.d0
         call cdfnor (1, p, q, x, 0.0d0, 1.0d0, status, bound)
         if (status.eq.0) then
            write(*,*) 'Probability (%) = ', p*100.d0
         else
            write(*,*) '*** Error in subroutine CDFNOR'//char(7)
            write(*,*) '    STATUS = ',status
         end if
c
c  Normal dist'n -- inverse
c
      else if (opt.eq.'NI' .or. opt.eq.'ni') then
         write(*,100) 'Enter probability (%) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) p
         p = p/100.d0
         q = 1.d0-p
         x = 0.d0
         call cdfnor (2, p, q, x, 0.0d0, 1.0d0, status, bound)
         if (status.eq.0) then
            write(*,*) 'Critical limit = ', x
         else
            write(*,*) '*** Error in subroutine CDFNOR'//char(7)
            write(*,*) '    STATUS = ',status
         end if
c
c  Student t dist'n -- direct
c
      else if (opt.eq.'TD' .or. opt.eq.'td') then
         write(*,100) 'Enter critical limit > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) x
         write(*,100) 'Enter degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) df
         p = 0.d0
         q = 1.d0
         call cdft (1, p, q, x, df, status, bound)
         if (status.eq.0) then
            write(*,*) 'Probability (%) = ', p*100.d0
         else
            write(*,*) '*** Error in subroutine CDFT'//char(7)
            write(*,*) '    STATUS = ', status
         end if
c
c  Student t dist'n -- inverse
c
      else if (opt.eq.'TI' .or. opt.eq.'ti') then
*        write(*,*) '*** Not available (bug in CDFLIB library)'
*        write(*,*) '    Use Inverse Chi-square and take square root'
*        goto 10
         
         write(*,100) 'Enter probability (%) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) p
         p = p/100.d0
         p = 1-2*(1-p)  ! modify for F dist'n
         q = 1.d0-p
         write(*,100) 'Enter degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) df
         x = 0.d0

*  Subroutine cdft does not work for some reason -- error in library?
*         write(*,*) 'p=',p
*         write(*,*) 'q=',q
*         write(*,*) 'x=',x
*         write(*,*) 'df=',df
*         call cdft (2, p, q, x, df, status, bound)
*  Use subroutine cdff instead
          call cdff (2, p, q, x, 1d0, df, status, bound)
          x = sqrt(x)

         if (status.eq.0) then
            write(*,*) 'Critical limit = ', x
         else
*           write(*,*) '*** Error in subroutine CDFT'//char(7)
            write(*,*) '*** Error in subroutine CDFF'//char(7)
            write(*,*) '    STATUS = ', status
         end if
c
c  Chi-square dist'n -- direct
c
      else if (opt.eq.'CD' .or. opt.eq.'cd') then
         write(*,100) 'Enter critical limit > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) x
         write(*,100) 'Enter degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) df
         p = 0.d0
         q = 1.d0
         call cdfchi (1, p, q, x, df, status, bound)
         if (status.eq.0) then
            write(*,*) 'Probability (%) = ', p*100.d0
         else
            write(*,*) '*** Error in subroutine CDFCHI'//char(7)
            write(*,*) '    STATUS = ', status
         end if
c
c  Chi-square dist'n -- inverse
c
      else if (opt.eq.'CI' .or. opt.eq.'ci') then
         write(*,100) 'Enter probability (%) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) p
         p = p/100.d0
         q = 1.d0-p
         write(*,100) 'Enter degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) df
         x = 0.d0
         call cdfchi (2, p, q, x, df, status, bound)
         if (status.eq.0) then
            write(*,*) 'Critical limit = ', x
         else
            write(*,*) '*** Error in subroutine CDFCHI'//char(7)
            write(*,*) '    STATUS = ', status
         end if
c
c  Fisher dist'n -- direct
c
      else if (opt.eq.'FD' .or. opt.eq.'fd') then
         write(*,100) 'Enter critical limit > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) x
         write(*,100) 'Enter 1st degrees of freedom > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) dfn
         write(*,100) 'Enter 2nd degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) dfd
         p = 0.d0
         q = 1.d0
         call cdff (1, p, q, x, dfn, dfd, status, bound)
         if (status.eq.0) then
            write(*,*) 'Probability (%) = ', p*100.d0
         else
            write(*,*) '*** Error in subroutine CDFF'//char(7)
            write(*,*) '    STATUS = ', status
         end if
c
c  Fisher dist'n -- inverse
c
      else if (opt.eq.'FI' .or. opt.eq.'fi') then
         write(*,100) 'Enter probability (%) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) p
         p = p/100.d0
         q = 1.d0-p
         write(*,100) 'Enter 1st degrees of freedom > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) dfn
         write(*,100) 'Enter 2nd degrees of freedom (1e100=inf) > '
         read(*,'(a)') buf
         if (buf.eq.' ') goto 10
         read(buf,*) dfd
         x = 0.d0
         call cdff (2, p, q, x, dfn, dfd, status, bound)
         if (status.eq.0) then
            write(*,*) 'Critical limit = ', x
         else
            write(*,*) '*** Error in subroutine CDFF'
            write(*,*) '    STATUS = ', status
         end if
c
c  Invalid solution
c
      else
         write(*,*) '*** Error: Invalid solution code'//char(7)
      end if
c
c  Get new input
c
      goto 10
c
c  End of program
c
 999  continue
      end
