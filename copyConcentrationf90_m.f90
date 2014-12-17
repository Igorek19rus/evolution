      program myPDF2
      integer, parameter :: n=1221, nt_max = 2000,nt_min = 1, pt=312,prn=10002 &
                         ,nx=220,ny=100,nz=100,y_cut=26,ntview=320
      integer nrandom,nt,t,jj,nv_test,k,tt,nv,nv_int,time
      real trash,x0p,y0p,z0p
      integer trash_int
      integer i
      integer iix,iiz
      integer ix,iz
      real dx,dz
      dimension x(1000*nt_max),y(1000*nt_max),z(1000*nt_max),nv_int(nx+1,nz+1) &
               ,nPartInFile(nt_max+1),nv(nx+1,nz+1) !,nv_test(151,101)
      character*100 NameMyCoord,NameConcentr,pdf_folder,pdf_folder2,fileToOpen,pathFile
	  integer pdf_type 
      read(*,*) time
      print 798, "time: ", time
	  pdf_type = 1
!	  if (pdf_type .eq. 1) write (pdf_folder,"(A)") "data/my/"
!	  if (pdf_type .eq. 5) write (pdf_folder,"(A)") "data/my5/"
!	  if (pdf_type .eq. 11) write (pdf_folder,"(A)") "data/bb1/"
!	  if (pdf_type .eq. 4) write (pdf_folder,"(A)") "data/bbPresent/"
	  if (pdf_type.eq.1) write(pdf_folder2, "(A)") "data/my/"
	  if (pdf_type.eq.2) write(pdf_folder2, "(A)") "data/my2/"
	  if (pdf_type.eq.11) write(pdf_folder2, "(A)") "data/bb1/"
	  if (pdf_type.eq.4) write(pdf_folder2, "(A)") "data/bbPresent/"
      if (time .gt. 0 .and. time .lt. 10) then
	  if (pdf_type .eq. 1) write(pdf_folder, "(A,i1,A)") "data/my/",time,"/"
	  if (pdf_type .eq. 2) write(pdf_folder, "(A,i1,A)") "data/my2/",time,"/"
	  if (pdf_type .eq. 11) write(pdf_folder, "(A,i1,A)") "data/bb1/",time,"/"
	  if (pdf_type .eq. 4) write(pdf_folder, "(A,i1,A)") "data/bbPresent/",time,"/"
      else
	  if (pdf_type .eq. 1) write(pdf_folder, "(A,i2,A)") "data/my/",time,"/"
	  if (pdf_type .eq. 2) write(pdf_folder, "(A,i2,A)") "data/my2/",time,"/"
	  if (pdf_type .eq. 11) write(pdf_folder, "(A,i2,A)") "data/bb1/",time,"/"
	  if (pdf_type .eq. 4) write(pdf_folder, "(A,i2,A)") "data/bbPresent/",time,"/"
      endif
      pathFile = trim(pdf_folder)//"concentr"
      call system('mkdir -p ' // trim(pathFile) )
      dx = 10000./nx
      dz = 2500./nz           !      dz = 2./nz

	  fileToOpen = trim(pdf_folder)//"data/tableRead.dat"
	  open(676, FILE="history.txt",STATUS="old",POSITION = "append")
	  write(676,*) "copy concentration : "//trim(pdf_folder)
      close(676)
      open(45,file=fileToOpen)
      do 10 i=1,nt_max+1 !n                     change               <----------------
            read (45,791) nPartInFile(i+1), trash_int, trash_int
10      continue
       nPartInFile(1)=0

      do 11 i=1,nt_max+1
            print 795, nPartInFile(i),i
11      continue
      close(45)

!      do 14 ii=1,nx+1
!            do 15 jj=1,nz+1
!                  nv(ii, jj) = 0
!15            continue
!14      continue

      do 12 nt=nt_min+1, nt_max
!      do 12 nt=nt_min, nt_max+1            !change      <----------
      t=nt-1	
      print 795,t			    
      do 14 ii=1,nx+1
            do 15 jj=1,nz+1
                  nv_int(ii, jj) = 0
15            continue
14      continue

       if (t.gt.-1 .and. t.lt.10) &
             write(NameMyCoord,'(A,i1,A)') 'data/mycoord000',t,'.dat'

       If(t .gt. 9 .and. t .lt. 100) &
            write(NameMyCoord,'(A,i2,A)') 'data/mycoord00',t,'.dat'

       If(t .gt. 99 .and. t .lt. 1000) &
            write(NameMyCoord,'(A,i3,A)') 'data/mycoord0',t,'.dat'

       If(t .gt. 999 .and. t .lt. 10000) &
            write(NameMyCoord,'(A,i4,A)') 'data/mycoord',t,'.dat'

!      write(NameMyCoord,'(A)') 'data\tableRead.txt'
	   NameMyCoord = trim(pdf_folder2)//NameMyCoord
       open (34,file=NameMyCoord)
       print 799, "---",nPartInFile(t+2),t
       do 13 j=1,nPartInFile(t+2)       !change      <----------
!       do 13 j=1,nPartInFile(t+1)
            read (34,729) x(j),trash,z(j),trash_int,trash_int
!+	     read (34,139) x(j),trash,z(j),trash_int,trash_int
13       continue
       close (34)

       do 16 j=1,nPartInFile(t+1)
            ix=-1
            iz=-1
            do 17 iix=1, nx
                  if (x(j).ge.(iix-1)*dx .and. x(j).lt.iix*dx ) &
                   ix = iix+1
17            continue
            do 18 iiz=1,nz
                  if (z(j).ge.(iiz-1)*dz .and. z(j).lt.iiz*dz ) &
                   iz = iiz+1
18            continue
            if (ix.gt.0 .and. iz.gt.0 .and. ix.le.nx+1 .and. iz.le.nz+1) &
              nv_int(ix, iz) = nv_int(ix, iz) + 1
!                 if (ix.ge.nx+2 .or. iz.ge.nz+2)
!     .        print 796,ix,iz
16       continue


      !
       if (t.gt.-1 .and. t.lt.10) &
             write(NameConcentr,'(A,i1,A)') &
                          'concentr/concentr000',t,'.dat'

       If(t .gt. 9 .and. t .lt. 100) &
            write(NameConcentr,'(A,i2,A)') &
                          'concentr/concentr00',t,'.dat'
!
       If(t .gt. 99 .and. t .lt. 1000) &
            write(NameConcentr,'(A,i3,A)') &
                          'concentr/concentr0',t,'.dat'

       If(t .gt. 999 .and. t .lt. 10000) &
            write(NameConcentr,'(A,i4,A)') &
                          'concentr/concentr',t,'.dat'

!      write(NameConcentr,'(A)') 'data\pict\data\hz.txt'
	   NameConcentr = trim(pdf_folder)//NameConcentr
       open (35,file=NameConcentr)
      do 40 ii=1,nx+1
            do 41 jj=1,nz+1
                  nv(ii, jj) = nv_int(ii, jj)   !*1./
!     .                     (nPartInFile(t+2)*1.)
41            continue
40      continue

                  do 20 jj=1,nz+1
                        write (35,797) &
                                            nv(1,jj),nv(2,jj),nv(3,jj) &
                         ,nv(5,jj),nv(6,jj),nv(7,jj),nv(8,jj),nv(9,jj) &
                    ,nv(10,jj),nv(11,jj),nv(12,jj),nv(13,jj),nv(14,jj) &
                    ,nv(15,jj),nv(16,jj),nv(17,jj),nv(18,jj),nv(19,jj) &
                    ,nv(20,jj),nv(21,jj),nv(22,jj),nv(23,jj),nv(24,jj) &
                    ,nv(25,jj),nv(26,jj),nv(27,jj),nv(28,jj),nv(29,jj) &
                    ,nv(30,jj),nv(31,jj),nv(32,jj),nv(33,jj),nv(34,jj) &
                    ,nv(35,jj),nv(36,jj),nv(37,jj),nv(38,jj),nv(39,jj) &
                    ,nv(40,jj),nv(41,jj),nv(42,jj),nv(43,jj),nv(44,jj) &
                    ,nv(45,jj),nv(46,jj),nv(47,jj),nv(48,jj),nv(49,jj) &
                    ,nv(50,jj),nv(51,jj),nv(52,jj),nv(53,jj),nv(54,jj) &
                    ,nv(55,jj),nv(56,jj),nv(57,jj),nv(58,jj),nv(59,jj) &
                    ,nv(60,jj),nv(61,jj),nv(62,jj),nv(63,jj),nv(64,jj) &
                    ,nv(65,jj),nv(66,jj),nv(67,jj),nv(68,jj),nv(69,jj) &
                    ,nv(70,jj),nv(71,jj),nv(72,jj),nv(73,jj),nv(74,jj) &
                    ,nv(75,jj),nv(76,jj),nv(77,jj),nv(78,jj),nv(79,jj) &
                    ,nv(80,jj),nv(81,jj),nv(82,jj),nv(83,jj),nv(84,jj) &
                    ,nv(85,jj),nv(86,jj),nv(87,jj),nv(88,jj),nv(89,jj) &
                    ,nv(90,jj),nv(91,jj),nv(92,jj),nv(93,jj),nv(94,jj) &
                    ,nv(95,jj),nv(96,jj),nv(97,jj),nv(98,jj),nv(99,jj) &
               ,nv(100,jj),nv(101,jj),nv(102,jj),nv(103,jj),nv(104,jj) &
               ,nv(105,jj),nv(106,jj),nv(107,jj),nv(108,jj),nv(109,jj) &
               ,nv(110,jj),nv(111,jj),nv(112,jj),nv(113,jj),nv(114,jj) &
               ,nv(115,jj),nv(116,jj),nv(117,jj),nv(118,jj),nv(119,jj) &
               ,nv(120,jj),nv(121,jj),nv(122,jj),nv(123,jj),nv(124,jj) &
               ,nv(125,jj),nv(126,jj),nv(127,jj),nv(128,jj),nv(129,jj) &
               ,nv(130,jj),nv(131,jj),nv(132,jj),nv(133,jj),nv(134,jj) &
               ,nv(135,jj),nv(136,jj),nv(137,jj),nv(138,jj),nv(139,jj) &
               ,nv(140,jj),nv(141,jj),nv(142,jj),nv(143,jj),nv(144,jj) &
               ,nv(145,jj),nv(146,jj),nv(147,jj),nv(148,jj),nv(149,jj) &
               ,nv(150,jj),nv(151,jj),nv(152,jj),nv(153,jj),nv(154,jj) &
               ,nv(155,jj),nv(156,jj),nv(157,jj),nv(158,jj),nv(159,jj) &
               ,nv(160,jj),nv(161,jj),nv(162,jj),nv(163,jj),nv(164,jj) &
               ,nv(165,jj),nv(166,jj),nv(167,jj),nv(168,jj),nv(169,jj) &
               ,nv(170,jj),nv(171,jj),nv(172,jj),nv(173,jj),nv(174,jj) &
               ,nv(175,jj),nv(176,jj),nv(177,jj),nv(178,jj),nv(179,jj) &
               ,nv(180,jj),nv(181,jj),nv(182,jj),nv(183,jj),nv(184,jj) &
               ,nv(185,jj),nv(186,jj),nv(187,jj),nv(188,jj),nv(189,jj) &
               ,nv(190,jj),nv(191,jj),nv(192,jj),nv(193,jj),nv(194,jj) &
               ,nv(195,jj),nv(196,jj),nv(197,jj),nv(198,jj),nv(199,jj) &
               ,nv(200,jj),nv(201,jj),nv(202,jj),nv(203,jj),nv(204,jj) &
               ,nv(205,jj),nv(206,jj),nv(207,jj),nv(208,jj),nv(209,jj) &
               ,nv(210,jj),nv(211,jj),nv(212,jj),nv(213,jj),nv(214,jj) &
               ,nv(215,jj),nv(216,jj),nv(217,jj),nv(218,jj),nv(219,jj) &
               ,nv(220,jj) 
!
20                  continue
!19            continue
       close(35)
      !

!       print 795,t
12      continue

      print *,"dx,dz"
      print 794, dx, dz
      print *,"x,z"
!      print 794, x(1), z(1)
      print *,"nv"
      jj=26
!      print 794, x(nPartInFile(t+2)-1),z(nPartInFile(t+2)-1)
      print 795,nPartInFile(t+1)
!      print 794, x(nPartInFile(nt_max+2)),z(nPartInFile(nt_max+2))



 999      continue
	  open(676, FILE="history.txt",STATUS="old",POSITION = "append")
	  write(676,*) "copy concentration cancel: "//trim(pdf_folder)
      close(676)
      open(45,file=fileToOpen)
 99   format(1x,2i3,e16.8,i3)
 100  format(1x,2e16.8)
 101  format(1x,7e16.8)
 111  format(1x,6e13.5)
 112  format(1x,15e12.4)
 113  format(1x,10i4)
 114  format(1x,"Stop ",i3,e12.5)
 115  format(1x,"END ",3(i3,1x))
 117  format(1x,"q= ",i4," x0=",i3)
 118  format(1x,"z= ",e12.5,"w2=",e12.5,"s=",e12.5,"eh=",e12.5)
 119  format(1x,e16.8)
 121  format(1x,6e12.4)
 120  format(1x,i3)
 122  format(1x,i3,1x,i3,1x,e16.8)
 123  format(1x,4e12.5)
 124  format(1x,A,f13.5)
 125  format(1x,6(A,e12.5))
 126  format(1x,4(A,e12.5))
 127  format(1x,A,i3)
 128  format(3(f8.5,1x),2i6)
 129  format(1x,4f10.5)
 729  format(1x,3f14.7,i7,i7)
 139  format(1x,3f10.5,i7,i7,A)
 130  format(1x,3f11.5,3(A,3f11.5))
 131  format(1x,i6,2f12.3)
 132  format(1x,A,i7)
 134  format(1x,A,i6)
 135  format(1x,3f12.3,3i6)
 136  format(1x,A,2i6,A,3f12.3,A,3i3)
 138  format(1x,A,i8)
 792  format(1x,8e16.8)
 793  format(1x,2e16.8)
 794  format(1x,3f10.5,A,3e12.4,A,3e12.4,A,3(f8.5,1x))
 791  format(1x,3i10)
 795  format(1x,i10,i15)
 796  format(1x,220e12.5)
 797  format(1x,220i7)
 798  format(1x,A,i3,A,e15.7)
 799  format(1x,A,i10,i15)

      stop
      end

