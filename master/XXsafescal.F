c
c     (C) Rasmus Munk Larsen, Stanford University, 2000,2004
c     (C) David Wells, Virginia Tech, 2014
c
      subroutine XXsafescal(n,alpha,x)
c
c     Scale the vector x by 1/alpha avoiding unnecessary under- and overflow.
c

c     %-----------%
c     | Arguments |
c     %-----------%
      implicit none
      integer n
      XXX alpha, x(*)

c     %------------%
c     | Parameters |
c     %------------%
      XXX one
      parameter(one = 1.0XXXLITERAL)

c     %-----------------%
c     | Local variables |
c     %-----------------%
      integer i,info
      XXX sfmin

c     %----------------------%
c     | External Subroutines |
c     %----------------------%
      external XXscal,XXlascl

c     %--------------------%
c     | External Functions |
c     %--------------------%
      XXX XXlamch
      external XXlamch

c     %-----------------%
c     | Data statements |
c     %-----------------%
      save
      data sfmin /-1.0XXXLITERAL/

      if (sfmin.eq.-1.0XXXLITERAL) then
         sfmin = XXlamch('s')
      endif

      if (abs(alpha).ge.sfmin) then
         call pXXscal(n,one/alpha, x, 1)
      else
         call XXlascl('General',i,i,alpha,one,n,1,x,n,info)
      endif

      end
