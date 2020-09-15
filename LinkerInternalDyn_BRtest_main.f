      program main
      implicit none

      integer L
      parameter (L=3)
      integer ntrj
      parameter (ntrj=2000)
      integer ndummy
      parameter (ndummy=6)
      integer nlinker
      parameter (nlinker=6)
      integer ndimen
      parameter (ndimen=6)
      integer time_scale_max
      parameter (time_scale_max=100)
      integer time_scale_initial
      parameter (time_scale_initial=1)
      integer time_scale_trial
      parameter (time_scale_trial=0)
      integer nsample
      parameter (nsample=2*(ntrj-time_scale_max)-1)
      integer nsample2
      parameter (nsample2=1)

      integer i,j,k,id,index
      integer ii,jj
      integer time_scale
      real*8 dummy_l(ntrj,ndummy,nlinker)
      real*8 dummy_theta(ntrj,ndummy,nlinker)
      real*8 dummy_psai(ntrj,ndummy,nlinker)

      real*8 input(nsample,ndimen+1)
      real*8 output(nsample,ndimen+1)
      real*8 input2(nsample2,ndimen+1)
      real*8 output2(nsample2,ndimen+1)
      real*8 DynVector(2*(ntrj-time_scale_max),ndimen)
      integer linker_type(2*(ntrj-time_scale_max))
      integer N(L)
      real*8 testout(nsample2,ndimen+1)
      real*8 w(L,ndimen+1,ndimen+1)
      real*8 Sensitivity,Specificity,Precision,Accuracy
      integer TP,TN,FP,FN
      real*8 temp_dynvec
      integer temp_linker_type
      integer pick_i,pick_j
 
      real rand3
      double precision r3
      r3=5.0

ccccccccccccccccccccccccccccccccccccccccccccccc
c     input data
cccccccccccccccccccccccccccccccccccccccccccccc

      do i=1,ntrj
         do j=1,ndummy
            do k=1,nlinker
               dummy_l(i,j,k)=0
               dummy_theta(i,j,k)=0
               dummy_psai(i,j,k)=0
            enddo
         enddo
      enddo

      open(unit=10,file='TwoModuleInterCoord_GS15.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,1),j=1,ndummy-1),
     &        (dummy_theta(i,j,1),j=2,ndummy-1),
     &        (dummy_psai(i,j,1),j=2,ndummy-2)         
      enddo
      close(10)

      open(unit=10,file='TwoModuleInterCoord_GS30.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,2),j=1,ndummy-1),
     &        (dummy_theta(i,j,2),j=2,ndummy-1),
     &        (dummy_psai(i,j,2),j=2,ndummy-2)         
      enddo
      close(10)

      open(unit=10,file='TwoModuleInterCoord_PLP15.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,3),j=1,ndummy-1),
     &        (dummy_theta(i,j,3),j=2,ndummy-1),
     &        (dummy_psai(i,j,3),j=2,ndummy-2)         
      enddo
      close(10)

      open(unit=10,file='TwoModuleInterCoord_PLPII15.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,4),j=1,ndummy-1),
     &        (dummy_theta(i,j,4),j=2,ndummy-1),
     &        (dummy_psai(i,j,4),j=2,ndummy-2)         
      enddo
      close(10)

      open(unit=10,file='TwoModuleInterCoord_PLPII30.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,5),j=1,ndummy-1),
     &        (dummy_theta(i,j,5),j=2,ndummy-1),
     &        (dummy_psai(i,j,5),j=2,ndummy-2)         
      enddo
      close(10)

      open(unit=10,file='TwoModuleInterCoord_PLrigid.dat',
     &     status='old')
      read(10,*)      
      do i=1,ntrj
         read(10,2500) (dummy_l(i,j,6),j=1,ndummy-1),
     &        (dummy_theta(i,j,6),j=2,ndummy-1),
     &        (dummy_psai(i,j,6),j=2,ndummy-2)         
      enddo
      close(10)

cccccccccccccccccccccccccccccccccc

      open (unit=10,file=
     &     'BPNNLinkerInterDyn_output'
     &     //'.dat',
     &     status='unknown',access='append')
      
      write(10,*) 'The classification results based on '
      write(10,*) 'the BP neural network are shown as following:'

      write(10,*) 'The index for the linkers:'
      write(10,*) '1: GS15'
      write(10,*) '2: GS30'
      write(10,*) '3: PLP15'
      write(10,*) '4: PLPII15'
      write(10,*) '5: PLPII30'
      write(10,*) '6: PLrigid'

      write(10,*) 'T_sc ','lk_i ','lk_j ',' ',
     &     ' TP  ',' TN  ',' FP  ',' FN  ',
     &     ' SS  ',' SP  ',' PC  ',' AC '

               close(10)
         
      do time_scale=time_scale_initial,
     &     time_scale_initial+time_scale_trial

         do ii=1,nlinker-1
            do jj=ii+1,nlinker

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc   set up vector for DynVector and linker_type
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

               do i=1,2*(ntrj-time_scale_max)
                  linker_type(i)=0
                  do j=1,ndimen
                     DynVector(i,j)=0
                  enddo
               enddo

               index=0
               do i=1,ntrj-time_scale_max
                  index=index+1
                  linker_type(index)=ii
                  DynVector(index,1)=dummy_l(i+time_scale,3,ii)
     &                 -dummy_l(i,3,ii)
                  DynVector(index,2)=dummy_theta(i+time_scale,3,ii)
     &                 -dummy_theta(i,3,ii)
                  DynVector(index,3)=dummy_theta(i+time_scale,4,ii)
     &                 -dummy_theta(i,4,ii)
                  DynVector(index,4)=dummy_psai(i+time_scale,2,ii)
     &                 -dummy_psai(i,2,ii)
                  if(abs(DynVector(index,4)).gt.
     &                 abs(360.0-DynVector(index,4)))then
                     if(DynVector(index,4).lt.0.0)then
                        DynVector(index,4)=DynVector(index,4)+360.0
                     elseif(DynVector(index,4).gt.0.0)then
                        DynVector(index,4)=DynVector(index,4)-360.0
                     endif
                  endif
                  DynVector(index,5)=dummy_psai(i+time_scale,3,ii)
     &                 -dummy_psai(i,3,ii)
                  if(abs(DynVector(index,5)).gt.
     &                 abs(360.0-DynVector(index,5)))then
                     if(DynVector(index,5).lt.0.0)then
                        DynVector(index,5)=DynVector(index,5)+360.0
                     elseif(DynVector(index,5).gt.0.0)then
                        DynVector(index,5)=DynVector(index,5)-360.0
                     endif
                  endif
                  DynVector(index,6)=dummy_psai(i+time_scale,4,ii)
     &                 -dummy_psai(i,4,ii)
                  if(abs(DynVector(index,6)).gt.
     &                 abs(360.0-DynVector(index,6)))then
                     if(DynVector(index,6).lt.0.0)then
                        DynVector(index,6)=DynVector(index,6)+360.0
                     elseif(DynVector(index,6).gt.0.0)then
                        DynVector(index,6)=DynVector(index,6)-360.0
                     endif
                  endif
c                  DynVector(index,7)=dummy_l(i,3,ii)
c                  DynVector(index,8)=dummy_theta(i,3,ii)
c                  DynVector(index,9)=dummy_theta(i,4,ii)
c                  DynVector(index,10)=dummy_psai(i,2,ii)
c                  DynVector(index,11)=dummy_psai(i,3,ii)
c                  DynVector(index,12)=dummy_psai(i,4,ii)
               enddo

               do i=1,ntrj-time_scale_max
                  index=index+1
                  linker_type(index)=jj
                  DynVector(index,1)=dummy_l(i+time_scale,3,jj)
     &                 -dummy_l(i,3,jj)
                  DynVector(index,2)=dummy_theta(i+time_scale,3,jj)
     &                 -dummy_theta(i,3,jj)
                  DynVector(index,3)=dummy_theta(i+time_scale,4,jj)
     &                 -dummy_theta(i,4,jj)
                  DynVector(index,4)=dummy_psai(i+time_scale,2,jj)
     &                 -dummy_psai(i,2,jj)
                  if(abs(DynVector(index,4)).gt.
     &                 abs(360.0-DynVector(index,4)))then
                     if(DynVector(index,4).lt.0.0)then
                        DynVector(index,4)=DynVector(index,4)+360.0
                     elseif(DynVector(index,4).gt.0.0)then
                        DynVector(index,4)=DynVector(index,4)-360.0
                     endif
                  endif
                  DynVector(index,5)=dummy_psai(i+time_scale,3,jj)
     &                 -dummy_psai(i,3,jj)
                  if(abs(DynVector(index,5)).gt.
     &                 abs(360.0-DynVector(index,5)))then
                     if(DynVector(index,5).lt.0.0)then
                        DynVector(index,5)=DynVector(index,5)+360.0
                     elseif(DynVector(index,5).gt.0.0)then
                        DynVector(index,5)=DynVector(index,5)-360.0
                     endif
                  endif
                  DynVector(index,6)=dummy_psai(i+time_scale,4,jj)
     &                 -dummy_psai(i,4,jj)
                  if(abs(DynVector(index,6)).gt.
     &                 abs(360.0-DynVector(index,6)))then
                     if(DynVector(index,6).lt.0.0)then
                        DynVector(index,6)=DynVector(index,6)+360.0
                     elseif(DynVector(index,6).gt.0.0)then
                        DynVector(index,6)=DynVector(index,6)-360.0
                     endif
                  endif
c                  DynVector(index,7)=dummy_l(i,3,jj)
c                  DynVector(index,8)=dummy_theta(i,3,jj)
c                  DynVector(index,9)=dummy_theta(i,4,jj)
c                  DynVector(index,10)=dummy_psai(i,2,jj)
c                  DynVector(index,11)=dummy_psai(i,3,jj)
c                  DynVector(index,12)=dummy_psai(i,4,jj)
               enddo

c>>> data normalization

               do i=1,2*(ntrj-time_scale_max)
                  DynVector(i,1)=DynVector(i,1)/100.0
c                  DynVector(i,7)=DynVector(i,7)/100.0
                  DynVector(i,2)=DynVector(i,2)/180.0
                  DynVector(i,3)=DynVector(i,3)/180.0
c                  DynVector(i,8)=DynVector(i,8)/180.0
c                  DynVector(i,9)=DynVector(i,9)/180.0
                  DynVector(i,4)=DynVector(i,4)/360.0
                  DynVector(i,5)=DynVector(i,5)/360.0
                  DynVector(i,6)=DynVector(i,6)/360.0
c                  DynVector(i,10)=DynVector(i,10)/360.0
c                  DynVector(i,11)=DynVector(i,11)/360.0
c                  DynVector(i,12)=DynVector(i,12)/360.0
               enddo


c>>>   random shuffle

               do i=1,ntrj-time_scale_max
                  pick_i=int(rand3(r3)*(ntrj-time_scale_max)*2)+1
                  pick_j=int(rand3(r3)*(ntrj-time_scale_max)*2)+1
                  if(pick_i.ne.pick_j)then
                     temp_linker_type=linker_type(pick_i)
                     linker_type(pick_i)=linker_type(pick_j)
                     linker_type(pick_j)=temp_linker_type
                     do j=1,ndimen
                        temp_dynvec=DynVector(pick_i,j)
                        DynVector(pick_i,j)=DynVector(pick_j,j)
                        DynVector(pick_j,j)=temp_dynvec
                     enddo
                  endif
               enddo

cccccccccccccccccccccccccccccccccccccccc
c  set the topology of the BP network
cccccccccccccccccccccccccccccccccccccccc

               N(1)=ndimen+1
               N(2)=4
               N(3)=1
      
ccccccccccccccccccccccccccccccccccccccccc
c     set input and output value
cccccccccccccccccccccccccccccccccccccccccc

               TP=0
               FP=0
               FN=0
               TN=0
               
               do id=1,2*(ntrj-time_scale_max)

                  do i=1,nsample
                     do j=1,ndimen+1
                        input(i,j)=0
                        output(i,j)=0
                     enddo
                  enddo
                  
                  do i=1,nsample2
                     do j=1,ndimen+1
                        input2(i,j)=0
                        output2(i,j)=0
                        testout(i,j)=0
                     enddo
                  enddo
                  
                  index=0
                  do i=1,2*(ntrj-time_scale_max)
                     if(i.ne.id)then
                        index=index+1
                        do j=1,ndimen
                           input(index,j)=DynVector(i,j)
                        enddo
                        if(linker_type(i).eq.ii)then
                           output(index,1)=-1
                        elseif(linker_type(i).eq.jj)then
                           output(index,1)=1
                        endif
                     endif
                  enddo
                  
                  do j=1,ndimen
                     input2(1,j)=DynVector(id,j)
                  enddo
                  if(linker_type(id).eq.ii)then
                     output2(1,1)=-1
                  elseif(linker_type(id).eq.jj)then
                     output2(1,1)=1
                  endif
                  

cccccccccccccccccccccccccccccccccccccccccc
c  Initialize weight values
cccccccccccccccccccccccccccccccccccccccccc

                  do i=2,L
                     do j=1,N(i)
                        do k=1,N(i-1)
                           w(i,j,k)=(rand3(r3)-0.5)*0.2
                        enddo
                     enddo
                  enddo
         
ccccccccccccccccccccccccccccccccccccccc
c   Learning Process
ccccccccccccccccccccccccccccccccccccccc

                  call BPlearn(input,output,N,w,L,nsample)

ccccccccccccccccccccccccccccccccccccccc
c   Recall Process
ccccccccccccccccccccccccccccccccccccccc

                  call BPrecall(input2,output2,testout,N,w,L,nsample2)

cccccccccccccccccccccccccccccccccccccccccc
c  out put data
cccccccccccccccccccccccccccccccccccccccccc

                  if((linker_type(id).eq.ii)
     &                 .AND.(testout(1,1).le.0))then
                     TN=TN+1
                  elseif((linker_type(id).eq.ii)
     &                    .AND.(testout(1,1).gt.0))then
                     FP=FP+1
                  elseif((linker_type(id).eq.jj)
     &                    .AND.(testout(1,1).gt.0))then
                     TP=TP+1
                  elseif((linker_type(id).eq.jj)
     &                    .AND.(testout(1,1).le.0))then
                     FN=FN+1
                  endif


               enddo

               Sensitivity=real(TP)/real(TP+FN)
               Specificity=real(TN)/real(TN+FP)
               Precision=real(TP)/real(TP+FP)
               Accuracy=real(TP+TN)/real(TP+TN+FP+FN)

               open (unit=10,file=
     &              'BPNNLinkerInterDyn_output'
     &              //'.dat',
     &              status='unknown',access='append')

               write(10,3000) time_scale,ii,jj,TP,TN,FP,FN,
     &              Sensitivity,Specificity,Precision,Accuracy

               close(10)

            enddo
         enddo

      enddo

 2500 format(5x,12F10.3)
 3000 format(3I5,1x,4I5,1x,4F10.5)

      stop
      end

ccccccccccccccccccccccccccccccccccccccccccccccccc
c>> random number generator


      real  function rand3(r3)
      double precision s,u,v,r3
      s=65536.0
      u=2053.0
      v=13849.0
      m=r3/s
      r3=r3-m*s
      r3=u*r3+v
      m=r3/s
      r3=r3-m*s
      rand3=r3/s
      return
      end
      
ccccccccccccccccccccccccccccccccccccccccccccccccc
