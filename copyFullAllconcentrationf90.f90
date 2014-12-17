      program myPDF2
      integer,parameter :: ntime = 1500, nt_max = 2000,nt_min = 1, pt=312,prn=10002,nx=220,ny=100,nz=100,y_cut=26
      real, parameter :: npart = 300.
      integer nrandom,nt,t,jj,k,tt
      real trash,x0p,y0p,z0p
      real nvall,nv_all, nv
      integer trash_int
      integer i
      integer iix,iiz
      integer ix,iz
      real dx,dz
      real a,b,b2,c
      dimension x(1000*nt_max),y(1000*nt_max),z(1000*nt_max),nv(nx+1,nz+1) &
               ,nPartInFile(nt_max+1),nvall(nx+1,nz+1),nv_all(nx+1,nz+1)
      character*100 NameMyCoord,NameConcentr,pdf_folder,fileToOpen, pathFile, full_concentr
	  integer pdf_type, time, sum1, timeStart, timeCount, ti, missT


      pathFile = "data/pictures/data/"
      call system('mkdir -p ' // trim(pathFile) )

      open(676, FILE="history.txt",STATUS="old",POSITION="append")
      write(676,*) "full all concentration : "//trim(pathFile) 
      close(676)

      timeStart = 15
      timeCount = 24
      missT = 0

      do 2 ii=1,nx+1
            do 3 jj=1,nz+1
                  nvall(ii, jj) = 0
3            continue
2      continue

      do 1 ti = 0, timeCount-1

      time = timeStart+ti
      if (time.gt.24) time = time-24
      if (time.eq.9) then 
        missT = missT+1
 	goto 1
      endif
      print 801, "time: ", time
      pdf_type = 1
      if (time .gt. 0 .and. time .lt. 10) then
	  if (pdf_type .eq. 1) write(pdf_folder, "(A,i1,A,i4,A)") "data/my/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 2) write(pdf_folder, "(A,i1,A,i4,A)") "data/my2/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 11) write(pdf_folder, "(A,i1,A,i4,A)") "data/bb1/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 4) write(pdf_folder, "(A,i1,A,i4,A)") "data/bbPresent/",time,"/all_concentr/all_concentr",ntime,".dat"
      else
	  if (pdf_type .eq. 1) write(pdf_folder, "(A,i2,A,i4,A)") "data/my/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 2) write(pdf_folder, "(A,i2,A,i4,A)") "data/my2/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 11) write(pdf_folder, "(A,i2,A,i4,A)") "data/bb1/",time,"/all_concentr/all_concentr",ntime,".dat"
	  if (pdf_type .eq. 4) write(pdf_folder, "(A,i2,A,i4,A)") "data/bbPresent/",time,"/all_concentr/all_concentr",ntime,".dat"
      endif

print 801, pdf_folder

!     calculate a coefficient
!      open(45,file=fileToOpen)
!      do 10 i=1,nt_max+1 !n
!            read (45,791) nPartInFile(i+1), trash_int, trash_int
!10    continue
!       nPartInFile(1)=0

!      do 11 i=1,n+1-620
!            print 795, nPartInFile(i),i
!11      continue
!      close(45)

      do 4 ii=1,nx+1
            do 5 jj=1,nz+1
                  nv(ii, jj) = 0
5            continue
4      continue

!      do 12 nt=nt_min+1, nt_max-1
!            t=nt-1
!       print 795,t
!     READ
!            if (t.gt.-1 .and. t.lt.10) &
!                  write(NameConcentr,'(A,i1,A)') &
!                    'concentr/concentr000',t,'.dat'
!            If(t .gt. 9 .and. t .lt. 100) &
!                  write(NameConcentr,'(A,i2,A)') &
!                    'concentr/concentr00',t,'.dat'
!            If(t .gt. 99 .and. t .lt. 1000) &
!                  write(NameConcentr,'(A,i3,A)') &
!                    'concentr/concentr0',t,'.dat'
!            If(t .gt. 999 .and. t .lt. 10000) &
!                  write(NameConcentr,'(A,i4,A)') &
!                    'concentr/concentr',t,'.dat'
!            NameConcentr = trim(pdf_folder)//NameConcentr
!	full_concentr = trim(pathFile)//"fullConcentr"

            open (35,file=pdf_folder)
                  do 20 jj=1,nz+1
                    read (35,800)  nv(1,jj),nv(2,jj),nv(3,jj),nv(4,jj) &
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
20                  continue
            close(35)


      do 6 ii=1,nx+1
            do 7 jj=1,nz+1
                  nvall(ii, jj) = nvall(ii, jj) + nv(ii,jj)
7            continue
6      continue

1 continue

      do 8 ii=1,nx+1
            do 9 jj=1,nz+1
                  nvall(ii, jj) = nvall(ii, jj)/(timeCount-missT)
9            continue
8      continue

	full_concentr = trim(pathFile)//"fullConcentr.dat"

            open (35,file=full_concentr)
                  do 40 jj=1,nz+1
                        write (35,800) &
                      nvall(1,jj),nvall(2,jj),nvall(3,jj),nvall(4,jj) &
                     ,nvall(5,jj),nvall(6,jj),nvall(7,jj),nvall(8,jj) &
                     ,nvall(9,jj),nvall(10,jj),nvall(11,jj),nvall(12,jj) &
                     ,nvall(13,jj),nvall(14,jj),nvall(15,jj) &
                     ,nvall(16,jj),nvall(17,jj),nvall(18,jj) &
                     ,nvall(19,jj),nvall(20,jj),nvall(21,jj) &
                     ,nvall(22,jj),nvall(23,jj),nvall(24,jj) &
                     ,nvall(25,jj),nvall(26,jj),nvall(27,jj) &
                     ,nvall(28,jj),nvall(29,jj),nvall(30,jj) &
                     ,nvall(31,jj),nvall(32,jj),nvall(33,jj) &
                     ,nvall(34,jj),nvall(35,jj),nvall(36,jj) &
                     ,nvall(37,jj),nvall(38,jj),nvall(39,jj) &
                     ,nvall(40,jj),nvall(41,jj),nvall(42,jj) &
                     ,nvall(43,jj),nvall(44,jj),nvall(45,jj) &
                     ,nvall(46,jj),nvall(47,jj),nvall(48,jj) &
                     ,nvall(49,jj),nvall(50,jj),nvall(51,jj) &
                     ,nvall(52,jj),nvall(53,jj),nvall(54,jj) &
                     ,nvall(55,jj),nvall(56,jj),nvall(57,jj) &
                     ,nvall(58,jj),nvall(59,jj),nvall(60,jj) &
                     ,nvall(61,jj),nvall(62,jj),nvall(63,jj) &
                     ,nvall(64,jj),nvall(65,jj),nvall(66,jj) &
                     ,nvall(67,jj),nvall(68,jj),nvall(69,jj) &
                     ,nvall(70,jj),nvall(71,jj),nvall(72,jj) &
                     ,nvall(73,jj),nvall(74,jj),nvall(75,jj) &
                     ,nvall(76,jj),nvall(77,jj),nvall(78,jj) &
                     ,nvall(79,jj),nvall(80,jj),nvall(81,jj) &
                     ,nvall(82,jj),nvall(83,jj),nvall(84,jj) &
                     ,nvall(85,jj),nvall(86,jj),nvall(87,jj) &
                     ,nvall(88,jj),nvall(89,jj),nvall(90,jj) &
                     ,nvall(91,jj),nvall(92,jj),nvall(93,jj) &
                     ,nvall(94,jj),nvall(95,jj),nvall(96,jj) &
                     ,nvall(97,jj),nvall(98,jj),nvall(99,jj) &
                     ,nvall(100,jj),nvall(101,jj),nvall(102,jj) &
                     ,nvall(103,jj),nvall(104,jj),nvall(105,jj) &
                     ,nvall(106,jj),nvall(107,jj),nvall(108,jj) &
                     ,nvall(109,jj),nvall(110,jj),nvall(111,jj) &
                     ,nvall(112,jj),nvall(113,jj),nvall(114,jj) &
                     ,nvall(115,jj),nvall(116,jj),nvall(117,jj) &
                     ,nvall(118,jj),nvall(119,jj),nvall(120,jj) &
                     ,nvall(121,jj),nvall(122,jj),nvall(123,jj) &
                     ,nvall(124,jj),nvall(125,jj),nvall(126,jj) &
                     ,nvall(127,jj),nvall(128,jj),nvall(129,jj) &
                     ,nvall(130,jj),nvall(131,jj),nvall(132,jj) &
                     ,nvall(133,jj),nvall(134,jj),nvall(135,jj) &
                     ,nvall(136,jj),nvall(137,jj),nvall(138,jj) &
                     ,nvall(139,jj),nvall(140,jj),nvall(141,jj) &
                     ,nvall(142,jj),nvall(143,jj),nvall(144,jj) &
                     ,nvall(145,jj),nvall(146,jj),nvall(147,jj) &
                     ,nvall(148,jj),nvall(149,jj),nvall(150,jj) &
                     ,nvall(151,jj),nvall(152,jj),nvall(153,jj) &
                     ,nvall(154,jj),nvall(155,jj),nvall(156,jj) &
                     ,nvall(157,jj),nvall(158,jj),nvall(159,jj) &
                     ,nvall(160,jj),nvall(161,jj),nvall(162,jj) &
                     ,nvall(163,jj),nvall(164,jj),nvall(165,jj) &
                     ,nvall(166,jj),nvall(167,jj),nvall(168,jj) &
                     ,nvall(169,jj),nvall(170,jj),nvall(171,jj) &
                     ,nvall(172,jj),nvall(173,jj),nvall(174,jj) &
                     ,nvall(175,jj),nvall(176,jj),nvall(177,jj) &
                     ,nvall(178,jj),nvall(179,jj),nvall(180,jj) &
                     ,nvall(181,jj),nvall(182,jj),nvall(183,jj) &
                     ,nvall(184,jj),nvall(185,jj),nvall(186,jj) &
                     ,nvall(187,jj),nvall(188,jj),nvall(189,jj) &
                     ,nvall(190,jj),nvall(191,jj),nvall(192,jj) &
                     ,nvall(193,jj),nvall(194,jj),nvall(195,jj) &
                     ,nvall(196,jj),nvall(197,jj),nvall(198,jj) &
                     ,nvall(199,jj),nvall(200,jj),nvall(201,jj) &
                     ,nvall(202,jj),nvall(203,jj),nvall(204,jj) &
                     ,nvall(205,jj),nvall(206,jj),nvall(207,jj) &
                     ,nvall(208,jj),nvall(209,jj),nvall(210,jj) &
                     ,nvall(211,jj),nvall(212,jj),nvall(213,jj) &
                     ,nvall(214,jj),nvall(215,jj),nvall(216,jj) &
                     ,nvall(217,jj),nvall(218,jj),nvall(219,jj) &
                     ,nvall(220,jj)
40                  continue
            close(35)

 999      continue
	  open(676, FILE="history.txt",STATUS="old",POSITION="append")
	  write(676,*) "full all concentration cancel : "//trim(pdf_folder)
	  close(676)
!     calculate a coefficient
 99   format(1x,2i3,e16.8,i3)
 100  format(1x,2e16.8)
 101  format(1x,7e16.8)
 111  format(1x,6e13.5)
 112  format(1x,15e12.5)
 113  format(1x,10i4)
 114  format(1x,"Stop ",i3,e12.5)
 115  format(1x,"END ",3(i3,1x))
 116  format(1x,i333,6e16.5)
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
 796  format(1x,260e12.5)
 797  format(1x,251e16.4)
 798  format(1x,251f16.4)
 799  format(1x,220i7)
 800  format(1x,220e16.5)
 801  format(1x,A,i3,A,e15.7)
 802  format(1x,A,2i7,5e15.7)
      stop
      end

