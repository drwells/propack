c
c     (C) Rasmus Munk Larsen, Stanford University, 2004
c     (C) David Wells, Virginia Tech, 2014
c

#ifdef _OPENMP
      XXX function pXXnrm2(n, x, incx)
      implicit none
      integer n, incx
      XXX x(*)

      integer i
      XXX sum

      if ((n.gt.0).and.(incx.ne.0)) then
         sum = 0.0XXXLITERAL
         if (incx.eq.1) then
c$OMP PARALLEL DO  reduction(+:sum) schedule(static)
            do i=1,n
               sum = sum + x(i)**2
            enddo
         else
c$OMP PARALLEL DO firstprivate(incx) reduction(+:sum) schedule(static)
            do i=1,n
               sum = sum + x(1+(i-1)*incx)**2
            enddo
         endif
         pXXnrm2 = sqrt(sum)
      else
         pXXnrm2 = 0.0XXXLITERAL
      endif
      return
      end
c
c****************************************************************************
c


      subroutine pXXscal(n, alpha, x , incx)
      implicit none
      integer n, incx
      XXX alpha,x(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0)) then
         if (incx.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha) schedule(static)
            do i=1,n
               x(i) = alpha*x(i)
            enddo
         else
c$OMP PARALLEL DO firstprivate(alpha, incx) schedule(static)
            do i=1,n
               x(1+(i-1)*incx) = alpha*x(1+(i-1)*incx)
            enddo
         endif
      endif
      return
      end

c
c****************************************************************************
c


      subroutine pXXcopy(n, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX x(*),y(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0).and.(incy.ne.0)) then
         if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO  schedule(static)
            do i=1,n
               y(i) = x(i)
            enddo
         else
c$OMP PARALLEL DO firstprivate(incx, incy) schedule(static)
            do i=1,n
               y(1+(i-1)*incy) = x(1+(i-1)*incx)
            enddo
         endif
      endif
      return
      end

c
c****************************************************************************
c
      subroutine pXXaxpy(n, alpha, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX alpha,x(*),y(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0).and.(incy.ne.0)) then
         if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha)  schedule(static)
           do i=1,n
               y(i) = alpha*x(i) + y(i)
            enddo
         else
c$OMP PARALLEL DO firstprivate(alpha,incx,incy) schedule(static)
            do i=1,n
               y(1+(i-1)*incy) = alpha*x(1+(i-1)*incx) +
     c              y(1+(i-1)*incy)
            enddo
         endif
      endif
      return
      end

c
c****************************************************************************
c


      XXX function pXXdot(n, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX x(*),y(*)

      integer i
      XXX sum

      if ((n.gt.0).and.(incx.ne.0).and.(incy.ne.0)) then
         if (incx.eq.1 .and. incy.eq.1) then
            sum = 0.0XXXLITERAL
c$OMP PARALLEL DO reduction(+:sum) schedule(static)
            do i=1,n
               sum = sum + x(i) * y(i)
            enddo
         else
            sum = 0.0XXXLITERAL
c$OMP PARALLEL DO firstprivate(incx, incy) reduction(+:sum)
c$OMP& schedule(static)
            do i=1,n
               sum = sum + x(1+(i-1)*incx) * y(1+(i-1)*incy)
            enddo
         endif
         pXXdot = sum
      else
         pXXdot = 0.0XXXLITERAL
      endif
      return
      end


#else

      XXX function pXXnrm2(n, x, incx)
      implicit none
      integer n, incx
      XXX x(*), XXnrm2
      external XXnrm2

      pXXnrm2 = XXnrm2(n, x, incx)
      end

      subroutine pXXscal(n, alpha, x , incx)
      implicit none
      integer n, incx
      XXX alpha,x(*)

      call XXscal(n, alpha, x , incx)
      end

      subroutine pXXcopy(n, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX x(*),y(*)

      call XXcopy(n, x , incx, y, incy)
      end

      subroutine pXXaxpy(n, alpha, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX alpha,x(*),y(*)

      call XXaxpy(n, alpha, x , incx, y, incy)
      end


      XXX function pXXdot(n, x , incx, y, incy)
      implicit none
      integer n, incx, incy
      XXX x(*),y(*),XXdot
      external XXdot

      pXXdot = XXdot(n, x , incx, y, incy)
      end
#endif

c
c****************************************************************************
c

      subroutine pXXzero(n, x , incx)
      implicit none
      integer n, incx
      XXX x(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0)) then
         if (incx.eq.1) then
c$OMP PARALLEL DO
            do i=1,n
               x(i) = 0.0XXXLITERAL
            enddo
         else
c$OMP PARALLEL DO firstprivate(incx) schedule(static)
            do i=1,n
               x(1+(i-1)*incx) = 0.0XXXLITERAL
            enddo
         endif
      endif
      return
      end


      subroutine pizero(n, x , incx)
      implicit none
      integer n, incx
      integer x(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0)) then
         if (incx.eq.1) then
c$OMP PARALLEL DO  schedule(static)
            do i=1,n
               x(i) = 0
            enddo
         else
c$OMP PARALLEL DO firstprivate(incx) schedule(static)
            do i=1,n
               x(1+(i-1)*incx) = 0
            enddo
         endif
      endif
      return
      end


      subroutine pXXset(n, alpha, x , incx)
      implicit none
      integer n, incx
      XXX alpha,x(*)

      integer i

      if ((n.gt.0).and.(incx.ne.0)) then
         if (incx.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha) schedule(static)
            do i=1,n
               x(i) = alpha
            enddo
         else
c$OMP PARALLEL DO firstprivate(alpha, incx) schedule(static)
            do i=1,n
               x(1+(i-1)*incx) = alpha
            enddo
         endif
      endif
      return
      end



      subroutine pXXaxpby(n,alpha,x,incx,beta,y,incy)
c
c     Y = alpha*X + beta*Y
c

      implicit none
      XXX one,zero
      parameter(one = 1.0XXXLITERAL,zero = 0.0XXXLITERAL)
      integer n,incx,incy,i
      XXX alpha,beta,x(n),y(n)

      if (n.le.0 .or. incy.eq.0 .or. incx.eq.0) return
      if (alpha.eq.zero .and. beta.eq.zero) then
         if (incy.eq.1) then
c$OMP PARALLEL DO  schedule(static)
            do i=1,n
               y(i) = zero
            enddo
         else
c$OMP PARALLEL DO firstprivate(incy) schedule(static)
            do i=1,n
               y(1+(i-1)*incy) = zero
            enddo
         endif

      else if (alpha.eq.zero .and. beta.ne.zero) then

         call pXXscal(n,beta,y,incy)

      else if (alpha.ne.zero .and. beta.eq.zero) then

         if (alpha.eq.one) then
            call pXXcopy(n,x,incx,y,incy)
         else
            if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha) schedule(static)
               do i=1,n
                  y(i) = alpha*x(i)
               enddo
            else
c$OMP PARALLEL DO firstprivate(incx, incy, alpha)
c$OMP& schedule(static)
               do i=1,n
                  y(1+(i-1)*incy) = alpha*x(1+(i-1)*incx)
               enddo
            endif
         endif

      else

         if (beta.eq.one) then
c XXAXPY
            call pXXaxpy(n,alpha,x,incx,y,incy)
         else
            if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha,beta)
c$OMP& schedule(static)
               do i=1,n
                  y(i) = alpha*x(i) + beta*y(i)
               enddo
            else
c$OMP PARALLEL DO firstprivate(alpha,beta,incx,incy)
c$OMP& schedule(static)
               do i=1,n
                  y(1+(i-1)*incy) = alpha*x(1+(i-1)*incx) +
     c                 beta*y(1+(i-1)*incy)
               enddo
            endif
         endif
      endif
      return
      end



      subroutine pXXaxty(n,alpha,x,incx,y,incy)
c
c     Y = alpha*X*Y
c

      implicit none
      XXX one,zero
      parameter(one = 1.0XXXLITERAL,zero = 0.0XXXLITERAL)
      integer n,incx,incy,i
      XXX alpha,x(n),y(n)

      if (n.le.0 .or. incy.eq.0 .or. incx.eq.0) return
      if (alpha.eq.zero) then
         if (incy.eq.1) then
c$OMP PARALLEL DO schedule(static)
           do i=1,n
               y(i) = zero
            enddo
         else
c$OMP PARALLEL DO firstprivate(incy) schedule(static)
           do i=1,n
               y(1+(i-1)*incy) = zero
            enddo
         endif

      else if (alpha.ne.zero) then

         if (alpha.eq.one) then
            if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO  schedule(static)
               do i=1,n
                  y(i) = x(i)*y(i)
               enddo
            else
c$OMP PARALLEL DO firstprivate(incx,incy) schedule(static)
               do i=1,n
                  y(1+(i-1)*incy) = x(1+(i-1)*incx)*y(1+(i-1)*incy)
               enddo
            endif

         else
            if (incx.eq.1 .and. incy.eq.1) then
c$OMP PARALLEL DO firstprivate(alpha) schedule(static)
               do i=1,n
                  y(i) = alpha*x(i)*y(i)
               enddo
            else
c$OMP PARALLEL DO firstprivate(alpha,incx,incy)
c$OMP& schedule(static)
               do i=1,n
                  y(1+(i-1)*incy) = alpha*x(1+(i-1)*incx)*
     c                 y(1+(i-1)*incy)
               enddo
            endif
         endif
      endif
      return
      end

      subroutine XXzero(n, x , incx)
      implicit none
      integer n, incx
      XXX x(*),zero
      parameter (zero = 0.0XXXLITERAL)
      integer i

      if ((n.gt.0).and.(incx.ne.0)) then
         if (incx.eq.1) then
            do i=1,n
               x(i) = zero
            enddo
         else
            do i=1,n
               x(1+(i-1)*incx) = zero
            enddo
         endif
      endif
      return
      end
