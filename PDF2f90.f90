      program myPDF2
      real if1,if2,if3,if4

      integer q,t,x0,xtest,ztest,ix0,ix1,iz0,iz1,time
      real sgs,s,ss,m1,m2,a1,a2,sgm_c1,sgm_c2,apb1,apb2,apb3,allu,allw, &
           cosk5,sink5,tangk5,cosfi,sinfi,newu,newv,slope,uz,wz,lambda, &
           zi,ws,h1,length,s2,s15,sgm_b,ix,iz,trash,vel_max,mysgm
      real ch,ch2ms,cu,cw,apbAll1,apbAll2,apbAll
      logical isDay

      dimension z(151),eh(151),ep(151),w2(151),w3(151),w(151,201) &
           ,ux(151,201),xx(151),zx(151,200),jj(151),erf(500000),u(151) &
          ,v(151),xer(500000),ee(151),utest(10,10),wtest(100),vel_max(5)
      character*60 name_cell,name_velos,name_all_cell,pdf_folder,fileToOpen, data_name
      character*60 tempString,tempString2,timeString
      integer pdf_type, tDay, tNight
      read(*,*) time
      !time = 15;
!      write(timeString,"(i2)") time
      print 796, "time: ", time
      tDay = 10
      tNight = 21
      pdf_type = 1
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
     print 802, "pdf_folder: ", pdf_folder
      if (time .ge. tDay .and. time .lt. tNight) then
	isDay=.true.
      else
	isDay=.false.
      endif
!	isDay=.true.
          call system('mkdir -p ' // trim(pdf_folder) )
          print 796, pdf_folder
	  open (676,FILE="history.txt",STATUS="old",POSITION="append")
	  write(676,*) "pdf create : "//trim(pdf_folder)
	  close(676)
!        call system('mkdir -p data/my/' // adjustl(trim(timeString) ) )
      write(data_name,'(A,i2,A)') 'dataBB/ndd',time,'tr.d'
      print 802, "data_name: ", data_name
      open(10,file=data_name)
! ------ од?н ??й?л?? --------------------
      vel_max(1)=0
      n=130
      api=3.14
      l=100
      read(10,792) zi,ws
      mysgm=0
!      isDay=.true.
!if (isDay.eqv..false.) then
!  zi = 0.42021
!  ws = 0.35811
!endif
! ============
      ch=1./zi
      ch2ms = zi*3000.
      cu=5.
      cw=1.
! ============
      print 796, "time--: ", time
      do 2 k=1,n
       read(10,792) z(k),eh(k),ep(k),w2(k),w3(k),u(k),v(k)
!! here
       z(k)=z(k)*ch
       print 796, "z(",k,")=", z(k)
       print 796, "z(",k,")m=", z(k)*ch2ms	
2     continue
      close(10)

!write(fileToOpen,'(A,i2,A)') 'data/zi.dat'
! open(10,file=pdf_folder<>fileToOpen)
! write(10,792) zi, ws
! close(10)

!      print 114,n,z(n)
      do 3 k=1,n
           if(z(k-1) .le. 0.25 .and. z(k) .gt. 0.25) k25=k
	       if(z(k-1) .le. 0.5  .and. z(k) .gt. 0.5 ) k5 =k
     	   if(z(k-1) .le. 0.75 .and. z(k) .gt. 0.75) k75=k
	       if(z(k-1) .le. 1.2  .and. z(k) .gt. 1.2 ) k12=k
! ========
3     continue
      do 200 k=1,n
       z(k)=z(k)/ch
200     continue
! ========
      print 796, "k25= ",k25
      print 796, "k5 = ",k5
      print 796, "k75= ",k75
      print 796, "k12= ",k12
      lambda=0
      lambda=2.*api/(sqrt (1.9*1.6/eh (k5)) ** 3*ep (k5))
if (isDay) then 
else
  lambda=0.18381 
endif 
!	  tempString = trim(pdf_folder)//"cell_tR("
!          write (tempString2,'(i2)') time
!          tempString = trim(tempString)//tempString2
!	  name_cell = trim(tempString)//").dat"
	  name_cell = trim(pdf_folder)//"cell_tR.dat"
!	  tempString = trim(pdf_folder)//"velosity_info("
!          write (tempString2,'(i2)') time
!          tempString = trim(tempString)//tempString2
!	  name_velos = trim(tempString)//").dat"
	  name_velos = trim(pdf_folder)//"velosity_info.dat"
if (isDay) then     ! add        <-----
      fileToOpen = trim(pdf_folder)//"celldata.dat"
      open(10,file=fileToOpen)
      fileToOpen = trim(pdf_folder)//"velosity.dat"
      open(9,file= fileToOpen)
      fileToOpen = trim(pdf_folder)//"pdf025.dat"
      open(12,file=fileToOpen)
      fileToOpen = trim(pdf_folder)//"pdf05.dat"
      open(15,file=fileToOpen)
	  fileToOpen = trim(pdf_folder)//"pdf075.dat"
      open(16,file=fileToOpen)
      fileToOpen = trim(pdf_folder)//"erf.dat"

      fileToOpen = trim(pdf_folder)//"parameters.dat"
      open(302,file=fileToOpen)


endif            ! add        <-----
      do 156 k=1,n
       do 157 j=1,2*l
       ux(k,j)=0
       w(k,j)=0
157    continue
156      continue
if (isDay) then     ! add        <-----
!      open(45,file='dispersia.dat')
! ---------------------------------
! calculate a pdf
      Do 1 k=1,n
      ee(k)       = eh(k)+w2(k)/2.
      sgm         = Sqrt(w2(k))
      s           = w3(k)/sgm**3.
	  ! my
	  if (pdf_type .eq. 1) then
        sgm_b       = w2(k)/3.
        s15         = (sgm**2.-sgm_b)**3.
        s2          = Sqrt((s**2.*sgm**6.)/s15)
        ss          = Sqrt(s2**2.+8.)
        m1          = sgm/4.*(s2+ss)
        m2          = sgm/4.*(s2-ss)
        a1          = -1.*(s2-ss)/2./ss
        a2          =  1.*(s2+ss)/2./ss
!        sgm_c1      = m1**2
!        sgm_c2      = m2**2
!/--
        sgm_c1      = m1**2.
		sgm_c2      = m2**2.
		sgm1        = sgm_c1+sgm_b
		sgm2        = sgm_c2+sgm_b
!--/
      endif
	  !my2
	  if (pdf_type .eq. 2) then
        sgm_b       = eh(k)
        s15         = (sgm**2.-sgm_b)**3.
        s2          = Sqrt((s**2.*sgm**6.)/s15)
        ss          = Sqrt(s2**2.+8.)
        m1          = sgm/4.*(s2+ss)
        m2          = sgm/4.*(s2-ss)
        a1          = -1.*(s2-ss)/2./ss
        a2          =  1.*(s2+ss)/2./ss
!        sgm_c1      = m1**2
!        sgm_c2      = m2**2
!/--
        sgm_c1      = m1**2.
		sgm_c2      = m2**2.
		sgm1        = sgm_c1+sgm_b
		sgm2        = sgm_c2+sgm_b
!--/
      endif

	  if (pdf_type .eq.4) then
		sgm_b  = eh(k)
		a1     = 0.4
		a2     = 0.6
		w3(1)  = 0.
		m1     = w3(k)**(1./3.)
		m2     = -2./3.*m1
		sgm1   = (w2(k)-0.28*w3(k)**(2./3.))**0.5
		sgm2   = (w2(k)-0.927*w3(k)**(2./3.))**0.5
!		sgm_c1 = sgm1-sgm_b
!		sgm_c2 = sgm2-sgm_b
		sgm_c1   = (w2(k)-0.28*w3(k)**(2./3.))**0.5
		sgm_c2   = (w2(k)-0.927*w3(k)**(2./3.))**0.5
!		sgm1 = sgm_c1+sgm_b
!		sgm2 = sgm_c2+sgm_b
		if (k.eq.1) then
			m1 = w3(2)**(1./3.)
			m2 = -2./3.*m1
			sgm1 = (w2(2)-0.28*w3(2)**(2./3.))**0.5
			sgm2 = (w2(2)-0.927*w3(2)**(2./3.))**0.5
		endif
		if(k.ge.121) then
			m1 = w3(120)**(1./3.)
			m2 = -2./3.*m1
			sgm1 = (w2(120)-0.28*w3(120)**(2./3.))**0.5
			sgm2 = (w2(120)-0.927*w3(120)**(2./3.))**0.5
		endif



	  endif


	  if (pdf_type .eq. 5) then
	    sgm_b       = eh(k)      
		s15         = (sgm**2.-sgm_b)**3.
        s2          = Sqrt((s**2.*sgm**6.)/s15)
        ss          = Sqrt(s2**2.+8.)
        m1          = sgm/4.*(s2+ss)
        m2          = sgm/4.*(s2-ss)
        a1          = -1.*(s2-ss)/2./ss
        a2          =  1.*(s2+ss)/2./ss
!        sgm_c1      = m1**2
!        sgm_c2      = m2**2
!/--
        sgm_c1      = m1**2.
		sgm_c2      = m2**2.
		sgm1        = sgm1+sgm_b
		sgm2        = sgm2+sgm_b
!--/
	  endif

	  if (pdf_type .eq. 11) then
	    sgm_b       = w2(k)/3.
        s15         = sqrt(s**2.+8.)
		a1          = -(s-s15)/(2.*s15)
		a2          =  (s+s15)/(2.*s15)
		m1          = 0.25*sgm*(s+s15)
		m2          = 0.25*sgm*(s-s15)
		sgm_c1        = m1**2.
		sgm_c2        = m2**2.
		sgm1      = sgm_c1+sgm_b
		sgm2      = sgm_c2+sgm_b
	  endif

      if(k .eq. 1) mysgm = sqrt(eh(k5))
	write(302,116) z(k),z(k)*zi,s,a1,a2,m1,m2,sgm_c1,sgm_c2,sgm1,sgm2,sgm,w3(k)
      if(k .eq. k5) print 795, " -= 0.5 =- ", z(k5),u(k5),v(k5), &
                          u(k5)*cu,v(k5)*cu,sqrt(u(k5)**2+v(k5)**2)*cu
      if(k .eq. n) print 795, " -= top =- ", z(n),u(n),v(n), &
                          u(n)*ws,v(n)*ws,sqrt(u(n)**2+v(n)**2)*ws
!      if(k .eq. k5) print 112,z(k),sgm,s,(s**2*sgm**6),sgm**2,
!     .            sgm_b,(sgm**2-sgm_b),(s**2*sgm**6)/(sgm**2-sgm_b),s2
      !sgm,m1,m2,a1,a2,sgm_c1,sgm_c2,
 !    .                    w2(k)/3.,s2
      if(z(k) .eq. 1) k1=k
!      if (k .eq. k5)
!     .   print 795, "u,v(k5)",u(k),v(k),sqrt(u(k)**2+v(k)**2)*ws
!      if(z(k) .gt. 1.2) goto 1

!me      if (k .eq. k5) lambda=2.*api/(sqrt(1.9*1.6/ee(k5))**3
!me     .                           *ep(k5) )
      erf(1)=0.
      nn=10000
!      if (k .eq. k5) print 795, "lambda = ",lambda
      do 33 kl=1,nn
      dww=6./(nn-1)
      ww=-3.+dww*(kl-1.)
      xer(kl)=ww
!      z1=ww/as
!      if(a_plus.le.0..or.a_minus.le.0.)goto 33
      apb1 = a1*exp(-.5*(ww-m1)**2./sgm_c1)/sqrt(2.*api*sgm_c1)
      apb2 = a2*exp(-.5*(ww-m2)**2./sgm_c2)/sqrt(2.*api*sgm_c2)
!      apb3 = exp(-.5*ww**2/(w2(k)/3.))/sqrt(2.*api*(w2(k)/3.))
      apb3 = exp(-.5*ww**2./sgm_b)/sqrt(2.*api*sgm_b)
      apb=apb1+apb2
	  apbAll1 = 1.*a1*exp(-.5*(ww-m1)**2./sgm1)/sqrt(2.*api*sgm1)
      apbAll2 = 1.*a2*exp(-.5*(ww-m2)**2./sgm2)/sqrt(2.*api*sgm2)
	  apbAll  = 1.*apbAll1+1.*apbAll2

      if(k .eq. k25) write(12,111) z(k),ww,apb1,apb2,apb3,apb,apbAll1 &
			  ,apbAll2,apbAll
      if(k .eq. k5)  write(15,111) z(k),ww,apb1,apb2,apb3,apb,apbAll1 &
			  ,apbAll2,apbAll
      if(k .eq. k75) write(16,111) z(k),ww,apb1,apb2,apb3,apb,apbAll1 &
			  ,apbAll2,apbAll
      if(kl .ne. 1) erf(kl)=erf(kl-1)+apb*dww
 33   continue
      do 34 kl=1,100
      xx(kl)=.01*kl
      xxx=xx(kl)
      zx(k,kl) = xx(kl)/2.
      zx(k,100+kl) = .5+xx(kl)/2.
      do 7 ll=1,nn-1
      if(xxx.ge.erf(ll).and.xxx.lt.erf(ll+1))then
      w(k,kl) = xer(ll)+(xer(ll+1)-xer(ll))*(xxx-erf(ll))/ &
                (erf(ll+1)-erf(ll))
      endif
  7   continue
      w(k,200-kl+1)=w(k,kl)
  34  continue
      w(k,100)=w(k,99)
      w(k,101)=w(k,99)
1      continue

!      close(45)

      do 57 k=2,n
!      if(z(k-1) .gt. 1.2) goto 57
!      dz=(z(k+1)-z(k-1))
      dz=(z(k)-z(k-1))
      wi1=0.
      wi2=0.
      do 56 j=2,99
!      dx1=zx(k-1,j+1)-zx(k-1,j-1)
!      dx2=zx(k+1,j+1)-zx(k+1,j-1)
!      wi1=wi1+(w(k-1,j-1)+w(k-1,j+1))/2.*dx1
!      wi2=wi2+(w(k+1,j-1)+w(k+1,j+1))/2.*dx2
!      ux(k,j)=-(wi2-wi1)/dz
!      ux(k,200-j)=-ux(k,j)
      dx2=zx(k,j)-zx(k,j-1)
      wi2=(w(k,j)-w(k-1,j))/dz
      ux(k,j)=-wi2*dx2+ux(k,j-1)
      ux(k,200-j)=-ux(k,j)
!      if (k.eq.2 .and. j.eq.50)
!     .print 795, "(2,2)",dx2 ,dz,w(k,j),w(k-1,j),wi2 !,ux(k,j-1)
!     .                          ,ux(k,j)
!      if (k.eq.3 .and. j.eq.50)
!     .print 795, "(3,2)",dx2 ,dz,w(k,j),w(k-1,j),wi2 !,ux(k,j-1)
!     .                          ,ux(k,j)
  56  continue
      ux(k,1)=0.
      ux(k,200)=0.
  57  continue
      Close(12)
      Close(15)
      Close(16)
!      Close(17)
endif     ! add        <-----
      vel=sqrt(u(k5)**2+v(k5)**2)
!      length = z(k5)/zi/lambda*3000.
!      h1=z(k1)*zi*3000.
      print 119,vel
	print 119,u(k5)
	print 119,v(k5)
	print 119,u(73)
!      print 119,length
!      open(12,file='u.dat')


!      do 58 k=1,n
!      u(k)=u(k)/length
!      v(k)=v(k)/length
!       do 59 kl=1,200
!            adis=sqrt(ux(k,kl)**2+w(k,kl)**2)
!            if(adis.le..001)then
!                   wz=0.
!                   uz=0.
!              endif
!            ux(k,kl)=ux(k,kl)/length
!            w(k,kl)=w(k,kl)/h1
! 59       continue
! 58      continue

!      print 114,k5,w(1,1)
!      print 114,k5,w(1,1)/1000.
!      print 114,k1,z(k1)
!      print 114,n,z(n)*zi*3000.
!      print 114,k1,z(k1)*zi*3000.
!      open(11,file='w.dat')
!      nl=1
!      jkk=200
!      jll=nl*jkk
!      do 77 k=1,n
!      do 76 j=1,200
!      all1=jkk-1.
!      all2=j-1.
!      zt=all2/all1
!      ln=(j-1)/jkk
!      zt=zx(k,j-ln*jkk)+ln
!      uz=ux(k,j-ln*jkk)+u(k)
!      uz=ux(k,j-ln*jkk)
!      wz=w(k,j-ln*jkk)
!      assdd=asd(k,j-ln*jkk)
!      adis=sqrt(uz**2+wz**2)
!      if(adis.le..001)then
!       wz=0.
!       uz=0.
!      endif
!      if(uz.ne.0.)then
!       ugl=atan(abs(wz/uz))*180./api
!       if(wz.ge.0..and.uz.ge.0.)atg=ugl
!       if(wz.le.0..and.uz.le.0.)atg=ugl+180.
!       if(wz.ge.0..and.uz.le.0.)atg=180.-ugl
!       if(wz.le.0..and.uz.ge.0.)atg=-ugl
!      else
!       if(wz.ge.0.)atg=90.
!       if(wz.lt.0.)atg=-90.
!      endif
!      if(z(k).gt.1.2)goto 77
!      xc=zt
!      jj(k)=j
      cosk5=u(k5)/sqrt(u(k5)**2+v(k5)**2)
      sink5=v(k5)/sqrt(u(k5)**2+v(k5)**2)
      tangk5=sink5/cosk5
!      do 80 k=1,n
!      cosfi=u(k)/sqrt(u(k)**2+v(k)**5)
!      sinfi=v(k)/sqrt(u(k)**2+v(k)**5)
!      tangfi=sinfi/cosfi
!      slope(k)=(tangfi-tangk5)/(1.+tangfi*tangk5)
!      newu(k)=sqrt(u(k)**2+v(k)**2)/sqrt(1+slope(k)**2)
!      newv(k)=slope(k)*newu(k)
! 180      continue
!      time = 0
      nl=1
!      dt=60
      jkk=200
      jll=nl*jkk
!      open(20,file='uvxo.dat')
!      open(21,file='x0_05.dat')
!      do 79 t=0,time
!      print 120,t
!      time=t
!      write(name_cell,'(A,i2,A)') 'cell_tR.dat'
!      write(name_all_cell,'(A,i2,A)') 'all_cell_t',t,'.dat'
      open(31,file=name_cell)
      open(32,file=name_velos)
      write(31,112) zi,ws,lambda,mysgm,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.
!      open(32,file=name_all_cell)
!      if (t .eq. 0)  open(31,file='t0.dat')
!      if (t .eq. 1)  open(32,file='t1.dat')
!      if (t .eq. 2)  open(33,file='t2.dat')

!      do 78 cell=1,1
!            if (cell .eq. 1)  open(21,file='cell1.dat')
!              if (cell .eq. 2)  open(22,file='cell2.dat')
!              if (cell .eq. 3)  open(23,file='cell3.dat')
!              if (cell .eq. 4)  open(24,file='cell4.dat')
      do 77 k=1,n

do 344 kl=1,100 ! add        <-----
      xx(kl)=.01*kl ! add        <-----
      xxx=xx(kl) ! add        <-----
      zx(k,kl) = xx(kl)/2. ! add        <-----
      zx(k,100+kl) = .5+xx(kl)/2. ! add        <-----
344 continue ! add        <-----

      cosfi=u(k)/sqrt(u(k)**2+v(k)**2)
      sinfi=v(k)/sqrt(u(k)**2+v(k)**2)
      tangfi=sinfi/cosfi
if (tangfi.gt. 1000 .or. tangk5 .gt. 1000) then 
  slope = 0
else
    slope=(tangfi-tangk5)/(1.+tangfi*tangk5)
endif
!+    slope=(tangfi-tangk5)/(1.+tangfi*tangk5)     ! it was <----------
      newu=sqrt(u(k)**2+v(k)**2)/sqrt(1+slope**2)
      newv=slope*newu
      if (k.eq.73) then
	print 119,u(k)
	print 119,sqrt(u(k)**2+v(k)**2)
	print 119,cosfi
	print 119,sinfi
	print 119,tangfi
	print 119,tangk5
	print 119,slope
	print 119,newu
	print 119,newv
      endif

     write(32,112) z(k),z(k)*ch,z(k)*ch*ch2ms,slope,newu,newv, &
                    sqrt(newu**2+newv**2),u(k),v(k), &
                    sqrt(newu**2+newv**2)*5.,eh(k)
!      print 121,z(k),z(k)*zi*3000.,newu,newv
!     .            ,sqrt((newu)**2+(newv)**2)
!     .            ,sqrt(u(k)**2+v(k)**2)
 !     vel=1.1
!      xxx0 = vel*t*dt*ws/length
!      x0=2*Floor(xxx0*100.)-Floor(2*Floor(xxx0*100.)/200.)*200
!      if (k.eq.k5) print 119,xxx0
!      if (k.eq.k5) print 120,x0
!      if (k.eq.k5) write(21,122) t,xxx0,x0
!      if (k.eq.k25) write(20,99) k,t,xxx0,x0
!      if (k.eq.k5) write(20,99) k,t,xxx0,x0
!      if (k.eq.k75) write(20,99) k,t,xxx0,x0
!      if (z(k).eq.1) write(20,99) k,t,xxx0,x0
!      if (k.eq.k12) write(20,99) k,t,xxx0,x0
!      x0=2*Floor(xxx0*100)-Floor(newu)*t*200
!      x0=0
!      x0=10*t10
      do 76 j=1,200
      q=0
      q=j-1
      if (q .lt. 0) q=200+q
      q=q+1
!      if (t .eq. 2 .and. k .eq. 1)      Print 117, q,x0
      all1=jkk-1.
      all2=j-1.

      zt=all2/all1
      ln=(j-1)*1./(jkk*1.)
      zt=zx(k,j-ln*jkk)+ln
      uz=ux(k,j-ln*jkk)+u(k)
!      uz=ux(k,j-ln*jkk)
!      wz=w(k,j-ln*jkk)
      uz=ux(k,q)
      wz=w(k,q)
      allu=ux(k,q)+newu
!      if (sqrt(uz**2+wz**2) .gt. vel_max(1)) then
!      if (sqrt(uz**2+wz**2) .gt. 2) then
            vel_max(1)=sqrt(uz**2+wz**2)
            vel_max(2)=k*1.
            vel_max(3)=j*1.
            vel_max(4)=wz !sqrt(uz**2+wz**2)
            vel_max(5)=uz
if (isDay) then     ! add        <-----
      write(10,794) vel_max(1),vel_max(2),vel_max(3), &
                    vel_max(4),vel_max(5)
      if (k .eq. 1 .and. j.eq.1 ) then

      write(9,795) &
       "sqrt(allu**2+wz**2)  k  j  wz  allu  wz  uz"
      print 795, &
       "sqrt(allu**2+wz**2)  k  j  wz  allu  wz  uz"
      endif
      write(9,794) sqrt(allu**2+wz**2),k*1.,j*1.,wz,allu, &
                    wz,uz
endif  ! add        <-----
!      endif
      if (sqrt(uz**2+wz**2) .gt. 2) then
            vel_max(1)=sqrt(uz**2+wz**2)
            vel_max(2)=k*1.
            vel_max(3)=j*1.
            vel_max(4)=wz !sqrt(uz**2+wz**2)
            vel_max(5)=uz
!      print 795,vel_max(1),vel_max(2),vel_max(3),
!     .              vel_max(4),vel_max(5)
      endif
!      assdd=asd(k,j-ln*jkk)
      adis=sqrt(uz**2+wz**2)
      if(adis.le..001)then
       wz=0.
       uz=0.
      endif
!      if(uz.ne.0.)then
!       ugl=atan(abs(wz/uz))*180./api
!       if(wz.ge.0..and.uz.ge.0.)atg=ugl
!       if(wz.le.0..and.uz.le.0.)atg=ugl+180.
!       if(wz.ge.0..and.uz.le.0.)atg=180.-ugl
!       if(wz.le.0..and.uz.ge.0.)atg=-ugl
!      else
!       if(wz.ge.0.)atg=90.
!       if(wz.lt.0.)atg=-90.
!      endif
!      if(z(k).gt.1.2) goto 77
!      xc=zt
      jj(k)=j
      write(31,112) zt,z(k),slope,adis,uz,wz,newu,newv,allu,v(k), &
                    sqrt(newu**2+newv**2),z(k)*3000.,u(k),v(k), &
                    sqrt(newu**2+newv**2)*5.
!                       print all cells
!      do 78 cell=1,nl
!      xc=cell-1+zt
!      write(32,112) xc,z(k),slope,adis,uz,wz,newu,newv,allu,v(k),
!     .              sqrt(newu**2+newv**2)
!
! 78   continue

 76   continue
print 803, "k, z/zi, z_m, newu, newv : ", k, z(k), z(k)*ch2ms, newu, newv
 77   continue

!      close(20)
!      close(21)
      close(31)
      close(32)
! 79   continue
      close(20)

      print 113, (jj(k),k=1,n)
!      close(12)
      close(10)
if (isDay) then     ! add        <-----
      close(9)
      close(302)
endif ! add        <-----


!      do 78 k=1.n
!      do 79 j=1,200
!      if (vel_max)
!78    continue
!79    continue

!      time = 5
!      dt=60
!      pointexz=107.5
!      do 80 t=0,time
!      xxx0 = vel*t*dt*ws/length
!      d=Floor(xxx0)
!            do 81 kl=1,2*l-1
!                  xx0 = kl-xxx0*2*l
!                  if(xx0.lt.0) xx0 = 2*l-1+xx0
!                  if(d.gt.0)   xx0 = d*2*l+kl-xxx0*2*l
!                  if(xx0.lt.0) xx0 = 2*l-1+xx0
!                  if (kl .eq. 1) point = Floor(xx0)
!                  if (kl .eq. 1) pointexx = xx0+1.
!                  if (kl .eq. 1) print 124, "---------------- t=",t*1.
!                  if (kl .eq. 1) print 124, "              w= ",fpoint
!                  if (kl .eq. 1) print 124, "xx0= ",xx0
!                  if (kl .eq. 1) print 124, "xxx0= ", xxx0*length/ws*ws
! 81            continue
!      fpoint = fint(w(Floor(pointexz),Floor(pointexx)),
!     .              w(Floor(pointexz),Floor(pointexx)+1.),
!     .              w(Floor(pointexz)+1.,Floor(pointexx)),
!     .              w(Floor(pointexz)+1.,Floor(pointexx)+1.),
!     .              pointexx,pointexz,Floor(pointexx),
!     .              Floor(pointexx)+1,Floor(pointexz),
!     .              Floor(pointexz)+1)
!!      print 124, "---------------- ",t*1.
!!      print 124, "              w= ",fpoint

!      print 124, "v= ",vel*ws
!      print 124, "xxx0= ",xxx0
! 80   continue
!      print 124, "asdasd ",w(107,200)

!      do 80 k=1,10
!            do 81 kl=1,10
!                  utest(kl,k)=(kl+2*k)*1.
! 81            continue
! 80   continue

!      xtest=1.3
!      ztest=2.5
!      ix=1.3
!      ix0=1
!      ix1=2
!      iz=3.5
!      iz0=3
!      iz1=4
!      if1=utest(ix0,iz0)*1.
!      if2=utest(ix1,iz0)*1.
!      if3=utest(ix0,iz1)*1.
!      if4=utest(ix1,iz1)*1.
!      ftest=fint(utest(ix0,iz0),utest(ix1,iz0),utest(ix0,iz1),
 !    .      utest(ix1,iz1),ix,iz,ix0,ix1,iz0,iz1)


!      print 114,100,utest(ix0,iz0)
!      print 114,100,utest(ix1,iz0)
!      print 114,100,utest(ix0,iz1)
!      print 114,100,utest(ix1,iz1)

!      print 100,ftest
!      print 792, vel_max(1),vel_max(2),vel_max(3),vel_max(4),vel_max(5)
!      print 795,"sgm = ",eh(108), k5*1., mysgm
	  open (676,FILE="history.txt",STATUS="old",POSITION="append")
	  write(676,*) "pdf create cancel : "//trim(pdf_folder)
	  close(676)
 99   format(1x,2i3,e16.8,i3)
 100  format(1x,2e16.8)
 101  format(1x,7e16.8)
 111  format(1x,999999999e13.5)
 112  format(1x,15e12.4)
 113  format(1x,10i4)
 114  format(1x,"Stop ",i3,e12.5)
 115  format(1x,"END ",3(i3,1x))
 116  format(1x,13e13.5)
 117  format(1x,"q= ",i4," x0=",i3)
 118  format(1x,"z= ",e12.5,"w2=",e12.5,"s=",e12.5,"eh=",e12.5)
 119  format(1x,e16.8)
 121  format(1x,6e12.4)
 120  format(1x,i3)
 122  format(1x,i3,1x,i3,1x,e16.8)
 123  format(1x,4e12.5)
 124  format(1x,A,f13.5)
 792  format(1x,8e16.8)
 793  format(1x,2e16.8)
 794  format(1x,e15.7,2f4.0,5e15.7)
 795  format(1x,A,8e15.7)
 796  format(1x,A,i3,A,e15.7)
 802  format(1x,6A)
 803  format(1x,A,i3,4e15.7)

      print 796, "end"
999   stop
      end

      function fint(iif1,iif2,iif3,iif4,iix,iiz,iix0,iix1,iiz0,iiz1)
      real iif1,iif2,iif3,iif4,iix,iiz
      integer iix0,iix1,iiz0,iiz1
      f0=iif1+(iif3-iif1)/(iiz1-iiz0)*(iiz-iiz0)
      f1=iif2+(iif4-iif2)/(iiz1-iiz0)*(iiz-iiz0)
      fint=f0+(f1-f0)/(iix1-iix0)*(iix-iix0)

!      print 125,iif1
!      print 125,iif2
!      print 125,iif3
!      print 125,iif4
!      print 125,f0
!      print 125,f1
!      print 125,fint
!      print 126,iif1,iif3,iif1
!      print 127,iiz1*1.,iiz0*1.,iiz*1.,iiz0*1.
      return
 125  format(1x,"Stop ",e12.5)
 126  format(1x,"--- ", 3e12.5)
 127  format(1x,"    ", 4e12.5)
      end
