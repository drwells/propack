c
c     (C) Rasmus Munk Larsen, Stanford University, 2004
c

      subroutine XXgemm_ovwr(transa,m,n,k,alpha,A,lda,beta,B,ldb,
     c     work,lwork)
c
c     compute B <- alpha*op(A)*B + beta*B
c
      implicit none
      character(len=1) transa
      integer m,n,k,lda,ldb,lwork
      XXX alpha,beta,A(lda,*),B(ldb,*),work(lwork)
      XXX zero
      parameter(zero = 0.0XXXLITERAL)
      integer i,j,l,blocksize

      if((m.le.0).or.(n.le.0).or.(k.le.0)) return
      if (lwork.lt.m) stop 'Too little workspace in XXGEMM_OVWR'
      if (m.gt.ldb) stop 'm>ldb in XXGEMM_OVWR'
      blocksize = int(lwork/m)
      do i=1,n-blocksize+1,blocksize
         call XXgemm(transa,'N',m,blocksize,k,alpha,A,lda,
     c              B(1,i),ldb,zero,work,m)
         if (beta.eq.0.0XXXLITERAL) then
            do j=0,blocksize-1
               do l=1,m
                  B(l,i+j)  = work(j*m+l)
               enddo
            enddo
         else
            do j=0,blocksize-1
               do l=1,m
                  B(l,i+j)  = work(j*m+l) + beta*B(l,i+j)
               enddo
            enddo
         endif
      enddo
      call XXgemm(transa,'N',m,n-i+1,k,alpha,A,lda,
     c           B(1,i),ldb,zero,work,m)
      if (beta.eq.0.0XXXLITERAL) then
         do j=0,n-i
            do l=1,m
               B(l,i+j)  = work(j*m+l)
            enddo
         enddo
      else
         do j=0,n-i
            do l=1,m
               B(l,i+j)  = work(j*m+l) + beta*B(l,i+j)
            enddo
         enddo
      endif
      return
      end


      subroutine XXgemm_ovwr_left(transb,m,n,k,alpha,A,lda,beta,B,ldb,
     c     work,lwork)
c
c     compute  A <- alpha*A*op(B)
c
      implicit none
      character(len=1) transb
      integer m,n,k,lda,ldb,lwork
      XXX alpha,beta,A(lda,*),B(ldb,*),work(lwork)
      XXX zero
      parameter(zero = 0.0XXXLITERAL)
      integer i,j,l,blocksize

      if((m.le.0).or.(n.le.0).or.(k.le.0)) return
      if (lwork.lt.n) stop 'Too little workspace in XXGEMM_OVWR_LEFT'
      blocksize = int(lwork/n)
      do i=1,m-blocksize+1,blocksize
         call XXgemm('n',transb,blocksize,n,k,alpha,A(i,1),lda,
     c              B,ldb,zero,work,blocksize)
         do j=0,n-1
            do l=0,blocksize-1
               A(i+l,j+1) = work(j*blocksize+1+l)
            enddo
         enddo
      enddo
      call XXgemm('n',transb,m-i+1,n,k,alpha,A(i,1),lda,
     c           B,ldb,zero,work,m-i+1)
      do j=0,n-1
         do l=0,m-i
            A(i+l,j+1) = work(j*(m-i+1)+1+l)
         enddo
      enddo
      return
      end
