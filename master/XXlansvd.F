      subroutine XXlansvd(jobu,jobv,m,n,k,kmax,aprod,U,ldu,Sigma,bnd,
     c     V,ldv,tolin,work,lwork,iwork,liwork,doption,ioption,info,
     c     dparm,iparm)


c     XXLANSVD: Compute the leading singular triplets of a large and
c     sparse matrix by Lanczos bidiagonalization with partial
c     reorthogonalization.
c
c     Parameters:
c
c     JOBU: CHARACTER(LEN=1). If JOBU.EQ.'Y' then compute the left singular
c           vectors.
c     JOBV: CHARACTER(LEN=1). If JOBV.EQ.'Y' then compute the right singular
c           vectors. ! TODO should this match functionality with slansvd?
c     M: INTEGER. Number of rows of A.
c     N: INTEGER. Number of columns of A.
c     K: INTEGER. Number of desired singular triplets. K <= MIN(KMAX,M,N)
c     KMAX: INTEGER. Maximal number of iterations = maximal dimension of
c           the generated Krylov subspace.
c     APROD: Subroutine defining the linear operator A.
c            APROD should be of the form:
c
c           SUBROUTINE DAPROD(TRANSA,M,N,X,Y,DPARM,IPARM)
c           CHARACTER(LEN=1) TRANSA
c           INTEGER M,N,IPARM(*)
c           XXXCAP X(*),Y(*),DPARM(*)
c
c           If TRANSA.EQ.'N' then the function should compute the matrix-vector
c           product Y = A * X.
c           If TRANSA.EQ.'T' then the function should compute the matrix-vector
c           product Y = A^T * X.
c           The arrays IPARM and DPARM are a means to pass user supplied
c           data to APROD without the use of common blocks.
c     U(LDU,KMAX+1): XXXCAP array. On return the first K columns of U
c               will contain approximations to the left singular vectors
c               corresponding to the K largest singular values of A.
c               On entry the first column of U contains the starting vector
c               for the Lanczos bidiagonalization. A random starting vector
c               is used if U is zero.
c     LDU: INTEGER. Leading dimension of the array U. LDU >= M.
c     SIGMA(K): XXXCAP array. On return Sigma contains approximation
c               to the K largest singular values of A.
c     BND(K)  : XXXCAP array. Error estimates on the computed
c               singular values. The computed SIGMA(I) is within BND(I)
c               of a singular value of A.
c     V(LDV,KMAX): XXXCAP array. On return the first K columns of V
c               will contain approximations to the right singular vectors
c               corresponding to the K largest singular values of A.
c     LDV: INTEGER. Leading dimension of the array V. LDV >= N.
c     TOLIN: XXXCAP. Desired relative accuracy of computed singular
c            values. The error of SIGMA(I) is approximately
c            MAX( 16*EPS*SIGMA(1), TOLIN*SIGMA(I) )
c     WORK(LWORK): XXXCAP array. Workspace of dimension LWORK.
c     LWORK: INTEGER. Dimension of WORK.
c            If JOBU.EQ.'N' and JOBV.EQ.'N' then  LWORK should be at least
c            M + N + 9*KMAX + 2*KMAX**2 + 4 + MAX(M+N,4*KMAX+4). !TODO M + N or M,N?
c            If JOBU.EQ.'Y' or JOBV.EQ.'Y' then LWORK should be at least
c            M + N + 9*KMAX + 5*KMAX**2 + 4 +
c            MAX(3*KMAX**2+4*KMAX+4, NB*MAX(M,N)), where NB>1 is a block !TODO NB>0?
c            size, which determines how large a fraction of the work in
c            setting up the singular vectors is done using fast BLAS-3
c            operation.
c     IWORK: INTEGER array. Integer workspace of dimension LIWORK.
c     LIWORK: INTEGER. Dimension of IWORK. Should be at least 8*KMAX if
c             JOBU.EQ.'Y' or JOBV.EQ.'Y' and at least 2*KMAX+1 otherwise.
c     DOPTION: XXXCAP array. Parameters for LANBPRO.
c        doption(1) = delta. Level of orthogonality to maintain among
c          Lanczos vectors.
c        doption(2) = eta. During reorthogonalization, all vectors with
c          with components larger than eta along the latest Lanczos vector
c          will be purged.
c        doption(3) = anorm. Estimate of || A ||.
c     IOPTION: INTEGER array. Parameters for LANBPRO.
c        ioption(1) = CGS.  If CGS.EQ.1 then reorthogonalization is done
c          using iterated classical GRAM-SCHMIDT. IF CGS.EQ.0 then
c          reorthogonalization is done using iterated modified Gram-Schmidt.
c        ioption(2) = ELR. If ELR.EQ.1 then extended local orthogonality is
c          enforced among u_{k}, u_{k+1} and v_{k} and v_{k+1} respectively.
c     INFO: INTEGER.
c         INFO = 0  : The K largest singular triplets were computed succesfully
c         INFO = J>0, J<K: An invariant subspace of dimension J was found.
c         INFO = -1 : K singular triplets did not converge within KMAX
c                     iterations.
c     DPARM: XXXCAP array. Array used for passing data to the APROD
c         function.
c     IPARM: INTEGER array. Array used for passing data to the APROD
c         function.
c
c     (C) Rasmus Munk Larsen, Stanford, 1999, 2004
c


c     %-----------%
c     | Arguments |
c     %-----------%
      implicit none
      include 'stat.h'
      character(len=1) jobu,jobv
      integer info,liwork
      integer m,n,k,kmax,lanmax,ldu,ldv,iwork(liwork),lwork
      XXX U(ldu,*),V(ldv,*),Sigma(*),bnd(*),work(lwork)
      XXX dparm(*),tolin,doption(*)
      integer iparm(*),ioption(*)
      external aprod

c     %------------%
c     | Parameters |
c     %------------%
      XXX one, zero
      parameter(one = 1.0XXXLITERAL, zero = 0.0XXXLITERAL)

c     %-----------------%
c     | Local variables |
c     %-----------------%
      integer i,j,dj,jold,ibnd,ib,ib1,iwrk,ierr,ip,iq,neig,lwrk,lapinfo
      XXX eps,eps34,epsn2,epsn,sfmin,anorm,rnorm,tol
      real t0,t1,t2,t3

c     %----------------------%
c     | External Subroutines |
c     %----------------------%
      external XXzero,izero,XXcopy,XXbdsqr

c     %--------------------%
c     | External Functions |
c     %--------------------%
      logical lsame
      XXX XXlamch,pXXnrm2
      external pXXnrm2,lsame
      external XXlamch

c-------------------- Here begins executable code ---------------------

c     %-------------%
c     | Start timer |
c     %-------------%
      call psecond(t0)

c     %---------------------------------%
c     | Set machine dependent constants |
c     %---------------------------------%
      eps = XXlamch('e')
      eps34 = eps**(3.0XXXLITERAL/4.0XXXLITERAL)
      epsn = XXXCAST(max(m,n))*eps/2.0XXXLITERAL
      epsn2 = sqrt(XXXCAST(max(m,n)))*eps/2.0XXXLITERAL
      sfmin = XXlamch('s')


c     %--------------------------------%
c     | Guard against absurd arguments |
c     %--------------------------------%
      lanmax = min(n+1,m+1,kmax)
      tol = min(one,max(16*eps,tolin))
      anorm = zero

c     %------------------------------%
c     | Set pointers into work array |
c     %------------------------------%
      ibnd = 1
      ib = ibnd + lanmax+1
      ib1 = ib + 2*lanmax
      ip = ib1 + 2*lanmax
      iq = ip + (lanmax+1)**2
      iwrk = iq + lanmax**2
      lwrk = lwork-iwrk+1
      call XXzero(7*lanmax + 2 + 2*lanmax**2,work,1)

c     %---------------------------------------------------------------%
c     | Set up random starting vector if none is provided by the user |
c     %---------------------------------------------------------------%
      rnorm = pXXnrm2(m,U(1,1),1)
      if (rnorm.eq.zero) then
         call XXgetu0('n',m,n,0,1,U,rnorm,U,ldu,aprod,
     c        dparm,iparm, ierr,ioption(1),anorm,work(iwrk))
      endif

      nsing = k
      info = 0
      neig = 0
      jold = 0
      j = min(k+max(8,k)+1,lanmax)

c     %------------------------------%
c     | Iterate until convergence... |
c     %------------------------------%
      do while (neig.lt.k)

c     %---------------------------------------------------%
c     | Compute bidiagonalization A*V_{j} = U_{j+1}*B_{j} |
c     %---------------------------------------------------%
         call XXlanbpro(m, n, jold, j, aprod, U, ldu, V, ldv,
     c        work(ib),lanmax,rnorm,doption(1),ioption(1),
     c        work(iwrk), iwork, dparm, iparm, ierr)
         jold = j

c     %---------------------------------------------%
c     | Compute and analyze SVD(B) and error bounds |
c     %---------------------------------------------%
         call XXcopy(2*lanmax, work(ib),1,work(ib1),1)
         call XXzero(j+1,work(ibnd),1)

         call psecond(t2)
         call XXbdqr((j.eq.min(m,n)),'N',j,work(ib1),work(ib1+lanmax),
     c        work(ibnd+j-1),work(ibnd+j),work(ip),lanmax+1)
         call XXbdsqr('u',j,0,1,0,work(ib1),work(ib1+lanmax),work,1,
     c        work(ibnd),1,work,1,work(iwrk),lapinfo)
         call psecond(t3)
         tbsvd = tbsvd + (t3-t2)
         nbsvd = nbsvd + 1

         if (j.gt.5) then
            anorm = work(ib1)
         else
            anorm = max(anorm,work(ib1))
         endif
         do i=1,j
            work(ibnd+i-1) = abs(rnorm*work(ibnd+i-1))
         enddo

c     %---------------------------------------------%
c     | Refine error bounds using the "Gap theorem" |
c     %---------------------------------------------%
c         if (lsame(jobu,'n') .and. lsame(jobv,'n')) then
            call XXrefinebounds(min(m,n),j,work(ib1),work(ibnd),
     c           epsn*anorm,eps34)
c         endif

c     %----------------------------------------------------%
c     | Determine the number of converged singular values  |
c     %----------------------------------------------------%
         do i=1,min(j,k)
            bnd(i) = work(ibnd+i-1)
c            write(*,*) 'sigma,bnd = ',work(ib1+i),bnd(i)
         enddo
         i = 0
         neig = 0
         do while(i.lt.min(j,k))
            if (work(ibnd+i).le.tol*work(ib1+i)) then
               neig = neig + 1
               sigma(neig) = work(ib1+i)
               i = i+1
            else
               i = k
            endif
         enddo

c     %--------------------------------------------------%
c     | Test if an invariant subspace has been found or |
c     | if the workspace has been exhausted.             |
c     %--------------------------------------------------%
         if (ierr.lt.0) then
            if (j.lt.k) then
               write(*,*) 'WARNING: Invariant subspace found.',
     c              ' Dimension = ',j
               info = j
            endif
            goto 50
         endif
         if (j.ge.lanmax) then
            if (neig.lt.k) then
               write(*,*) 'WARNING: Maximum dimension of Krylov',
     c              ' subspace exceeded prior to convergence.',
     c              ' Try increasing KMAX.'
               write(*,*) 'neig = ',neig
               info = -1
            endif
            goto 50
         endif

c     %----------------------------------------------------%
c     | Increase the dimension of the Krylov subspace.     |
c     | If any Ritz values have converged then try to      |
c     | estimate the average number of iterations per      |
c     | converged Ritz value.                              |
c     | Else increase the dimension by 50%.                |
c     %----------------------------------------------------%
         if (neig.gt.1) then
            dj = min(j/2,((k-neig)*(j-6))/(2*neig+1))
            dj = min(100,max(2,dj))
         else
            dj = j/2
            dj = min(100,max(10,dj))
        endif
         j = min(j + dj,lanmax)
      enddo

 50   if ((neig.ge.k .or. info.gt.0)  .and.
     c     (lsame(jobu,'y') .or. lsame(jobv,'y'))) then
c     %-----------------------------------------%
c     | Calculate singular vectors if requested %
c     %-----------------------------------------%
c         print *,'computing vectors: neig = ',neig,' jold = ',jold
         lwrk = lwrk + lanmax**2 + (lanmax+1)**2
         call XXritzvec('L',jobu,jobv,m,n,neig,jold,work(ib),
     c        work(ib+lanmax),U,ldu,V,ldv,work(ip),lwrk,iwork)
      endif
      k = neig
      nlandim = j
      call psecond(t1)
      tlansvd = t1-t0
      end
