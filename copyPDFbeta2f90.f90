      program myPDF2
      real if1,if2,if3,if4,ix,ix0,ix1,iz,iz0,iz1
      integer, parameter :: n=130,l=100,nrandom=100,dt=5,nt=1500,ntview=2000,pt=0,timeprint=1,print_mydata=0,ncell=10
      real, parameter :: zsource=0.5,dx=0.1,dy=0.1,dz=0.1,l_x=4., l_x_m=8000., Diff=0.2/10000, zsourcem = 100;
      integer z05,z12,bottom,t,zcentr,nv,nPartInFile,mybottom, tDay, tNight
      integer point,pointz,dsx
      logical isDay
      real lambda,zi,uc,wc,uw,vw,x,trash,vel,length, xunit,lw,h,mytemp, &
           mycox,mycoy,sigma,concentr,t1,t2,t3,t4,t_real,randx, randy, randz,dist
!      logical nextstep
!      dimension z(151),eh(151),ep(151),w2(151),w3(151),w(151,201)
!     .     ,ux(151,201),xx(151),zx(151,200),jj(151),erf(500000),u(151)
!     .     ,v(151),xer(500000),ee(151),utest(10,10),wtest(100),x(200)
      dimension z(151),uc(151,2*l+1),wc(151,2*l+1),x(2*l+1), &
         uw(151),vw(151),xrand(nt+1,nrandom),yrand(nt+1,nrandom), &
         zrand(nt+1,nrandom),x0p(nt+1,nrandom),eh(n), &
         y0p(nt+1,nrandom),z0p(nt+1,nrandom), &
         allu(151,2*l+1),allw(151,2*l+1), &
         mycox(nrandom*(nt+1)),mycoy(nrandom*(nt+1)),mytemp(nrandom), &
         nv(501,51,51),mybottom(ncell*2*l)
      character*50 name_cell,name_all_cell,alldata,coord,style_coord 
	  character*50  alldata2,coord2
      character*150 namechar,pdf_folder,pdf_folder2,fileToOpen,pathFile
	  integer pdf_type, time, tlimit
      read(*,*) time
      print 796, "time: ", time
	  dist=0
      xunit=3000.
      tDay = 10
      tNight = 21
      if (time .ge. tDay .and. time .lt. tNight) then
	isDay=.true.
      else
	isDay=.false.
      endif
      pdf_type = 1
	  if (pdf_type.eq.1) write(pdf_folder2, "(A)") "data/my/"
	  if (pdf_type.eq.2) write(pdf_folder2, "(A)") "data/my2/"
	  if (pdf_type.eq.11) write(pdf_folder2, "(A)") "data/bb1/"
	  if (pdf_type.eq.4) write(pdf_folder2, "(A)") "data/bbPresent/"
	pdf_folder2="data/my/"
      print 795, "pdf_folder2: ",trim(pdf_folder2)
      pathFile = trim(pdf_folder2)//"data"
      print 795, "pathFile: ",pathFile
      call system('mkdir -p ' // trim(pathFile) )

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
      print 795, "pdf_folder: ", trim(pdf_folder)
      pathFile = trim(pdf_folder)//"data"
      print 795, "pathFile: ",pathFile
      call system('mkdir -p ' // trim(pathFile) )


	  open (676, FILE="history.txt",STATUS="old",POSITION="append")
	  write(676,*) "copyPDFbeta2 : "//trim(pdf_folder)
	  close(676)
!
!     считывание с файла коэффициентов мастаба w* - ws,слоя инверсии - ziб длины конв ячейки
!     координат (x,z) скорости ячейки (uc,wc) и скорость ветра (uw,vw)
!       в масштабе w* (переход к натурному масштабу *ws)
!

      CALL CPU_TIME(t1)
      write(fileToOpen,'(A,i2,A)') 'dataBB/ndd',time,'tr.d'
      print 795, "data: ",fileToOpen
      open(10,file=fileToOpen)
      fileToOpen = ""
      fileToOpen = trim(pdf_folder)//"data/tableRead.dat"
      print 795, "tableRead: ",fileToOpen
      open(45,file=fileToOpen)
      read(10,793) trash,trash
      do 555 k=1,n
      read(10,792) trash,eh(k),trash,trash,trash,trash,trash,trash
555     continue
      close(10)
      
      name_cell = trim(pdf_folder)//"cell_tR.dat"
      open(31,file=name_cell)
      read(31,112) zi,ws,lambda,sigma,trash,trash,trash,trash,trash &
                  ,trash,trash,trash,trash,trash,trash
!      sigma=sigma/sqrt(3.)
!      sgm=sqrt(sgm/3)
!      print 123,ws,zi,lambda

! ============
      ch=1./zi
      ch2ms = zi*3000.
      cu=5.
      cw=1.
! ============

      do 2 k=1,n
            do 1 kl=1,2*l
              read(31,112) x(kl),z(k),trash,trash,uc(k,kl),wc(k,kl),uw(k) &
                         ,vw(k),trash,trash,trash,trash,trash,trash &
                         ,trash
!            if (k.eq.1 .and. kl.lt.100) print 796,"x(",kl,")=",x(kl)
!            if (kl.eq.2*l) print 119,z(k)
1            continue
            z(k)=z(k)/zi
print 796, "uw(",k,")=", uw(k)
!            print 796, "z(",k,")=", z(k)
2      continue
      Close(31)

!      приблизительное определение центра - z05 и верхней границы - z12 слоя инверсии
      do 3 k=2,n
      if(z(k-1) .le. 0.5  .and. z(k) .gt. 0.5 ) z05=k
      if(z(k-1) .le. 1.2  .and. z(k) .gt. 1.2 ) z12=k
      if(zsourcem .gt. 0) then 
        if(z(k-1)*ch2ms .le. zsourcem  .and. z(k)*ch2ms .gt. zsourcem) zcentr=k
      else
        if(z(k-1) .le. zsource  .and. z(k) .gt. zsource) zcentr=k
      endif

3      continue
      print 796, "z(05 -",z05,")m=", z(z05)*ch2ms
!      print 796, "z(12 -",z12,")=", z(z12)
!      print 796, "z(zcentr -",zcentr,")=", z(zcentr)

!      начальные координаты частички
      x0=0.
      y0=0.
      z0= z(zcentr) !z(z025)


      sigma = eh(z05)

!     uwp = fint2(uw(z05-1),uw(z05),0.5,z(z05-1),z(z05))
!      vwp = fint2(vw(z05-1),vw(z05),0.5,z(z05-1),z(z05))
!      print 119,z05*1.
!      вычисление скорости ячейки в середине слоя инверсии для вычисления
!         скорости сноса ячеек по ветру
      vel=Sqrt(uw(z05)**2+vw(z05)**2)
!      vel=Sqrt(uwp**2+vwp**2)
      print 119,vel
!      print 119,uw(z05)
!      print 119,vw(z05)




!      вычисление длины конвективной ячейки
      length = lambda*3000*5
	print 119,1.0001
	print 119,length
!      length=z(z05)*1./zi*3000./lambda
!      print 119,length
!      print 119,x(200)
!      print 119,xstar(200)

!      вычисление высоты слоя инверсии
      lw = 3000.*zi

!     переход к новому масштабу по горизонтали - задания коэффициента для x*
!      x_star=lambda*x(2*l)*ws/zi/vel
!      x_star=z(z05)*1./zi*3000./lambda
!      x_star=length/(lw*vel*ws)
!      x_star=length/(lw*vel*ws)
      x_star = ws/vel/zi
if (isDay.eqv..false.) x_star=1.5
!      x_star=1./(zi*vel*ws)


      s_t= ws/zi
!      coefficient of concentration
!      concentr=lw*vel*ws/(nrandom/dt)
      concentr=x_star/length*3000.*zi/5./nrandom

      print 124, "w*= ",ws
      print 124, "zcentr= ",z05*1.
      print 124, "t*=s_t*t      s_t=",s_t
      print 124, "sigma= ",sigma
      print 132, "namber of randoms= ",nrandom
      print 124, "length of cell= ",length
      print 124, "unit of x = ",xunit
!      print 124, "length of cell= ",ws/vel/ws/zi
!      print 124, "length of cell= ",1/vel/ws/zi
      print 124, "height of cell= ",lw
      print 124, "vel=",vel
      print 124, "uw(z05)=",uw(z05)*cu
      print 124, "vw(z05)=",vw(z05)*cu
      print 124, "vel_m/s=",vel*cu
      print 124, "zi=",zi
      print 124, "zn=",z(n)
      print 124, "zi_m=",ch2ms
      print 124, "lambda=",lambda
      print 124, "x_star=",x_star
      print 124, "z(source)/zi=",z(zcentr)
      print 124, "z(source)_m=",z(zcentr)*ch2ms
      print 124, "concentr= ",concentr

      namechar = trim(pdf_folder)//"data/characters.dat"

      open  (39,file= namechar)
      write (39,134) "namber of randoms          = ",nrandom
      write (39,134) "namber of timesteps        = ",nt
      write (39,134) "dt                         = ",dt
      write (39,124) "length of cell             = ",length
      write (39,124) "unit of x                  = ",xunit
      write (39,124) "height of cell             = ",lw
      write (39,124) "vel_m                      = ",vel*cu
      write (39,124) "vel_m_1.2                  = ",Sqrt(uw(z12)**2+vw(z12)**2)
      write (39,124) "zi                         = ",zi
      write (39,124) "zi_m                       = ",ch2ms
      write (39,124) "w*                         = ",ws
      write (39,124) "lambda                     = ",lambda
      write (39,124) "x_star                     = ",x_star
      write (39,124) "x_time                     = ",1./s_t
!      write (39,124) "zsource                    = ",zsource
      write (39,124) "zcentr_m                   = ",z(z05)*ch2ms
      write (39,124) "zsource/zi                 = ",z(zcentr)
      write (39,124) "z(source)_m                = ",z(zcentr)*ch2ms
      write (39,124) "all time                   = ",t4-t1
      write (39,124) "concentr                   = ",concentr




!      координаты частичек для цикла + считывание скоростей для фоновой турбулентности
!      взятых как случайные величины с гауссовским нормальным распределением

!
!      do 60 jj=1,21
!      read(37,128) trash,trash,trash
!60      continue

!       начальный точки для каждой частички
!       считывание случайных величин для фоновой турбулентности
	  fileToOpen = trim(pdf_folder)//"data/random_number.dat"
      open(37,file=fileToOpen)
!      open(41,file="data\random_all.txt")
      h=0
!      do 21 t=0,nt
!        do 22 jj=1,nrandom
!        x0p(t+1,jj)=x0
!        y0p(t+1,jj)=y0
!        z0p(t+1,jj)=z0
!        CALL gauss2(0,sgm,xrand(t+1,jj),yrand(t+1,jj))
!        CALL gauss1(0,sgm,zrand(t+1,jj))
!           write(37,128) xrand(t+1,jj),yrand(t+1,jj),zrand(t+1,jj),t,jj
!        write(41,128) xrand(t+1,jj),yrand(t+1,jj),zrand(t+1,jj),h
!        print 128,xrand(t+1,jj),yrand(t+1,jj),zrand(t+1,jj)
!22        continue
!21      continue

      do 21 innert=0,nt
        do 22 jj=1,nrandom
!          h=h+1
          x0p(innert+1,jj)=x0
          y0p(innert+1,jj)=y0
          z0p(innert+1,jj)=z0
          CALL gauss2(0,sigma,xrand(innert+1,jj),yrand(innert+1,jj))
          CALL gauss1(0,sigma,zrand(innert+1,jj))
!          if(innert.eq.0 .and. jj.eq.1) zrand(innert+1,jj) = -0.44345
             write(37,128) xrand(innert+1,jj),yrand(innert+1,jj), &
                        zrand(innert+1,jj),innert+1,jj
!          write(41,128) xrand(t+1,jj),yrand(t+1,jj),zrand(t+1,jj),h
!          print 128,xrand(jj),yrand(jj),zrand(jj)
22        continue
21      continue


      close(37)
!      close(41)

!       обнуляем массив для рисунка концетрации приземного слоя
!c      do 17 j=1, ncell*2*l
!c        mybottom(j)=0
!c17      continue
!c      do 31 i=1, 50
!c        do 32 j=1, 50
!c            do 33 k=1, 50
!c            nv(i,j,k)=0
!c33            continue
!c32        continue
!c31      continue

!
!      open (38,file="data\velosity.txt")
!      open (33,file="data\mydata.dat")
      fileToOpen = trim(pdf_folder)//"data/xyz_particle.dat"
      open (43,file=fileToOpen)

!    цикл по времени
      CALL CPU_TIME(t2)
!      do 7 t=0,nt                         ! change            <-------------
      do 7 t=0,ntview
      t_real=1.*t/s_t
!      if (mod(t,timeprint) .eq. 0)  print 130,t_real
!       считывание случайных величин для фоновой турбулентности


      if (print_mydata.eq.1) then
       if (t.gt.-1 .and. t.lt.10) then
        write(alldata2,'(A,i1,A)') 'data/mydata000',t,'.dat'
       endif
       if (t.ge.10 .and. t.lt.100) then
        write(alldata2,'(A,i2,A)') 'data/mydata00',t,'.dat'
       endif
       if (t.ge.100 .and. t.lt.1000) then
        write(alldata2,'(A,i3,A)') 'data/mydata0',t,'.dat'
       endif
       if (t.ge.1000 .and. t.lt.10000) then
        write(alldata2,'(A,i4,A)') 'data/mydata',t,'.dat'
       endif
	  alldata = trim(pdf_folder2)//alldata2
	  endif
      if (t.ge.-1 .and. t.lt.10) then
        write(coord2,'(A,i1,A)') 'data/mycoord000',t,'.dat'
      endif
      if (t.ge.10 .and. t.lt.100) then
        write(coord2,'(A,i2,A)') 'data/mycoord00',t,'.dat'
      endif
      if (t.ge.100 .and. t.lt.1000) then
        write(coord2,'(A,i3,A)') 'data/mycoord0',t,'.dat'
      endif
      if (t.ge.1000 .and. t.lt.10000) then
        write(coord2,'(A,i4,A)') 'data/mycoord',t,'.dat'
      endif
	  coord = trim(pdf_folder2)//coord2
!      write(alldata,'(A,i1,A)') 'mydata.dat'

      if (print_mydata.eq.1) open (32,file=alldata)
      open (34,file=coord)
      if (mod(t,timeprint) .eq. 0) print 134, "--------------- t=",t
!      print 124, alldata,t
!
      if (print_mydata.eq.1) write (32,134) "---------------- t=",t

            do      9 k=1,n
!            определение растояния сноса ячейки в масштабе l=1 для ячейки
!             xxx0 = vel*t*dt*ws/length/s_t
            xxx0 = vel*t*dt*cu/length
            d=Floor(xxx0)
                  do 10 kl=1,2*l+1

!                  определение номера в массиве - хх0 соответствующий положению ххх0
                  xx0 = kl-xxx0*2*l
                  if(xx0.lt.0) xx0 = 2*l+xx0
                  if(d.gt.0)   xx0 = d*2*l+kl-xxx0*2*l
                  if(xx0.lt.0) xx0 = 2*l+xx0
                  if(xx0.lt.1) xx0 = xx0+1
                  point = Floor(xx0)
!                  if (kl .eq. 1) pointexx = xx0+1.

!                  if (kl.eq.1 .and. k.eq.1) print 124, "        p= ",point
!                  if (kl.eq.1 .and. k.eq.1) print 124, "xxx0= ",xxx0
!                  if (kl.eq.1 .and. k.eq.1) print 124, "xxx0m= ",
!     .                                                  xxx0*length/ws*ws/xstar(200)
!                  if (kl.eq.1 .and. k.eq.1) print 124, "xx0= ",xx0
!                  if (kl.eq.1 .and. k.eq.1)
!                  print 124, "point= ",point
!                  if (kl.eq.1 .and. k.eq.1) print 124, "uc(200)= ",uc(1,200)

!=             задание новой скорости с учетом сноса ячеек ветром
!               h<z12    -> allu = uc+uw
!                    h<z12    -> allw = wc
!               h>z12    -> allu = uc
!                    h>z12    -> allw = 0
                  if (k.lt.z12) then
                    if (xx0.eq.point) then
!                                                                                      if (kl.eq.1)print 125,"point=",point," uc(k,point)=",uc(k,point)

!+                        allu(k,kl) = uc(k,point)+uw(k)            !it was  <---------------
if (isDay) then 
  allu(k,kl) = uc(k,point)+uw(k)
else  
  allu(k,kl) = uw(k)
endif
                  else
!                                                                                         allu(k,kl)=uc(k,point)+(uc(k,point+1)-uc(k,point))*
!                                                                                 .                       (xx0-point)
!+                        allu(k,kl) = fint2(uc(k,point),uc(k,point+1),xx0 &
!+                                ,point,point+1,.false.) + uw(k)                           ! it was <----------------
if(isDay) then 
  allu(k,kl) = fint2(uc(k,point),uc(k,point+1),xx0,point,point+1,.false.) + uw(k) 
else  
  allu(k,kl) = uw(k)
endif

                  endif
                  endif

                  if (k.lt.z12) then
                    if (xx0.eq.point) then
                        allw(k,kl) = wc(k,point)
                  else
                        allw(k,kl) = fint2(wc(k,point),wc(k,point+1),xx0 &
                                    ,point,point+1,.false.)
                  endif
                  endif

                  if (k.ge.z12) allu(k,kl) = uw(k)
                  if (k.ge.z12) allw(k,kl) = 0
!=/
!                        do 11, cell=1,ncell-1
!                  x(kl+2*l)=x(kl)+cell
!=             задание новой скорости с учетом сноса ячеек ветром для остальных ячеек
!                  if (k.lt.z12 .and. kl.eq.1 ) then
!                    if (xx0.eq.point) then
!                        allu(k,kl+cell*2*l) = uc(k,point)+uw(k)
!                  else
!                        allu(k,kl+cell*2*l) = fint2(uc(k,point),uc(k,point+1),
!     .                          xx0,point,point+1,.false.) + uw(k)
!                  endif
!                  endif
!
!                  if (k.lt.z12 .and. kl.eq.1 ) then
!                    if (xx0.eq.point) then
!                        allw(k,kl) = wc(k,point)
!                  else
!                        allw(k,kl) = fint2(wc(k,point),wc(k,point+1),xx0,
!     .                          point,point+1,.false.)
!                  endif
!                  endif
!
!                  if (k.gt.z12 .and. kl.eq.1 ) allu(k,kl) = uw(k)
!
!                  if (k.gt.z12 .and. kl.eq.1 ) allw(k,kl) = 0
!=/
!11                        continue
10                  continue
9            continue

      nPartInFile = 0;
!     цикл для прошедших времен
      if (t.gt.nt) then           ! change          <----------------
        tlimit=nt                 ! change          <----------------
      else			  ! change          <----------------
        tlimit=t		  ! change          <----------------
      endif			  ! change          <----------------
!      do 23 innert=0,t            ! change          <----------------
      do 23 innert=0,tlimit
!     цикл по частичкам
        do 8 jj=1,nrandom

!        присваивание для перехода к след интерации
        x0=x0p(innert+1,jj)
        y0=y0p(innert+1,jj)
        z0=z0p(innert+1,jj)
!        if (innert.ne.0) then
!            x0=x0p(innert,jj)
!            y0=y0p(innert,jj)                     ! ERROR обращение к 0вому элементу
!            z0=z0p(innert,jj)
!        else
!            x0=x0p(innert+1,jj)
!            y0=y0p(innert+1,jj)                     ! ERROR обращение к 0вому элементу
!            z0=z0p(innert+1,jj)
!            print 124, "x0=",x0
!            print 124, "y0=",y0
!            print 124, "z0=",z0
!        endif
        sx=0;sy=0;sz=0

!=       определение номера для массива - zp соответсвующий положению z0p
!      if (nextstep(jj).eq..true.) then
      if (z0.ne.0 .and. z0.lt.z(n)) then
        zp=0;xp=0
        do 12 k=2,n
          if(z(k-1) .le. z0  .and. z(k) .gt. z0) &
                             zp=k-1
12        continue
        pointz=Floor(zp)
        if (zp.eq.pointz) then
          zp=pointz
        else
            zp = pointz+(z0p(t+1,jj)-z(pointz))/ &
                 (z(pointz+1)-z(pointz))+pointz
             endif
!                                                                           print 125,"z(p)=",z(pointz)," z(p+1)=",z(pointz+1)," zx=",z0p(jj),
!                                                                        .                       " p0=",pointz," p1=",pointz+1," x=",zp
!с        if (t.gt.0) z0=z0p(t+1,jj)
!=/
!==      вычисление расстояния которая прошла частичка за еденицу времени dt
      xp2=x0*200+1
      xp=xp2

      dsx2=Floor(x0)
      if(xp.gt.200.) xp = xp-dsx2*200
      dsx=Floor(xp)
      vxt=fint10(allu(pointz,dsx),allu(pointz,dsx+1),allu(pointz+1,dsx), &
                 allu(pointz+1,dsx+1),xp,zp,dsx,dsx+1,pointz,pointz+1)
      vyt=fint10(vw(pointz),vw(pointz),vw(pointz+1), &
                 vw(pointz+1),dsx*1.,zp,dsx,dsx+1,pointz,pointz+1)
!                                                                                                            print 125,"vw(pointz)=",vw(pointz)," vw(pointz+1)=",vw(pointz+1),
!                                                                                                            .   " x=",dsx," z=",zp," x0=",dsx," dsx+1=",dsx+1," pointz=",pointz
      vzt=fint10(allw(pointz,dsx),allw(pointz,dsx+1),allw(pointz+1,dsx), &
                 allw(pointz+1,dsx+1),xp,zp,dsx,dsx+1,pointz,pointz+1)
!      sx=(vxt+xrand(innert+1,jj))*dt*ws
!      sy=(vyt+yrand(innert+1,jj))*dt*ws
!      sz=(vzt+zrand(innert+1,jj))*dt*ws
! print 797, "   -----   vx, vy : ", vxt,  " : ", vyt
!print 797, "   -----   pointz, dsx : ", pointz*1.,  " : ", dsx*1.
!print 797, "   -----   allu(100,5) : ", allu(100,5)*1.
!print 797, "   -----   uw(100,5) : ",uw(100)
!print 797, "   -----   uw(81,5) : ",uw(81)
!print 797, "   -----   allu :   ", allu(pointz,dsx)*1.,  " : ", allu(pointz,dsx+1)*1. ,  " : ", allu(pointz+1,dsx)*1. &
!                                 ,  " : ", allu(pointz+1,dsx+1)*1.


if (isDay) then     ! add        <-----
      sx=(vxt+xrand(innert+1,jj))*dt*cu
      sy=(vyt+yrand(innert+1,jj))*dt*cu
      sz=(vzt+zrand(innert+1,jj))*dt*cu
else     ! add        <-----
      CALL gauss1(0,1.,randx)     ! add        <-----
      CALL gauss1(0,1.,randy)     ! add        <-----
      CALL gauss1(0,1.,randz)     ! add        <-----
      sx=vxt*dt*cu + sqrt(2.*Diff*dt)*randx    ! add        <-----
      sy=vyt*dt*cu + sqrt(2.*Diff*dt)*randy    ! add        <-----
      sz=vzt*dt*cu + sqrt(2.*Diff*dt)*randz    ! add        <-----
end if     ! add        <-----

      endif
      if (z0.ge.z(n)) then
!      sx=(xrand(innert+1,jj))*dt*ws
!      sy=(yrand(innert+1,jj))*dt*ws
!      sz=(zrand(innert+1,jj))*dt*ws
if (isDay) then     ! add        <-----
      sx=(xrand(innert+1,jj))*dt*cu
      sy=(yrand(innert+1,jj))*dt*cu
      sz=(zrand(innert+1,jj))*dt*cu
else     ! add        <-----
      sx=0 ! sqrt(2.*Diff*dt)*randx     ! add        <-----
      sy=0 ! sqrt(2.*Diff*dt)*randy     ! add        <-----
      sz=0 ! sqrt(2.*Diff*dt)*randz     ! add        <-----
end if     ! add        <-----
      endif
!==/
!     вывод
      if(jj.eq.1 .and. t.eq.0 .and.innert.eq.0) &
        write(43,795) "      x0p       yop      zop   |" &
                     ,"    x0p(m/s)   yop(m/s)     zop(m/s)  |" &
                     ,"      sx         sy         sz        |" &
                     ,"   Vall_x(m/s) Vall_y(m/s) Vall_z(m/s)|" &
                     ,"     Vx(m/s)     Vy(m/s)     Vz(m/s)  |" &
                     ,"    Vr_x(m/s)   Vr_y(m/s)   Vr_z(m/s) |"


      if(jj.eq.1 .and. innert.eq.0) &
        write(43,794) &
               x0p(innert+1,jj),y0p(innert+1,jj),z0p(innert+1,jj) &
!              ," | ",x0*length,y0*length,z0*ch2ms &             changed  <---------
               ," | ",x0*xunit,y0*xunit,z0*ch2ms &
                  ," | ",sx,sy,sz &
                  ," | ",(vxt+xrand(innert+1,jj))*cu &
                    ,(vyt+yrand(innert+1,jj))*cu &
                    ,(vzt+zrand(innert+1,jj))*cu &
              ," | ",vxt*cu,vyt*cu,vzt*cu &
                  ," | ",xrand(innert+1,jj)*cu &
                    ,yrand(innert+1,jj)*cu &
                    ,zrand(innert+1,jj)*cu



!+      if (x0*x_star<=l_x) write (34,129) &
!+               x0*x_star,y0*x_star,z0,innert,jj             ! it was  <------------
      
      if (l_x_m .gt. 0) then 
        if (x0*xunit<=l_x_m) write (34,729) &
               x0*xunit,y0*xunit,z0*ch2ms,innert,jj             ! it was  <------------
        if (x0*xunit<=l_x_m) nPartInFile = nPartInFile+1
      else
        if (x0*x_star<=l_x) write (34,729) &
               x0**x_star,y0**x_star,z0,innert,jj             ! it was  <------------
        if (x0*x_star<=l_x) nPartInFile = nPartInFile+1
      endif       

!+      if (x0*x_star<=l_x) write (34,729) &
!+               x0*xunit,y0*xunit,z0*ch2ms,innert,jj             ! it was  <------------

!+      if (x0*x_star<=l_x) nPartInFile = nPartInFile+1
!+      if (innert==t .and. jj==nrandom) &
!+               write (45,791) nPartInFile, t, innert
!      if (jj.eq.1 .and.innert.eq.0)
!     .  write (38,130) x0*length,y0*length,z0*lw," --- ",
!     .  vxt*ws+xrand(innert+1,jj)*ws,xrand(innert+1,jj), !vyt*ws+yrand(innert+1,jj)*ws,
!     .  vzt*ws+zrand(innert+1,jj)*ws,
!     .  " --- ",sx,sy,sz," --- ",x0,xp2,xp

      if (innert.eq.0) then

!      if (jj.eq.1) write (38,134) "------------------",t
!      if (jj.eq.1 .and.t.eq.0) write (38,127)
!     ."x y z --- vx vy vz --- sx sy sz --- xp"
!      write (34,129) x0*x_star,y0*x_star,z0
      if (print_mydata.eq.1) then
       write (32,126) "xp= ",xp,"    zp= ",zp
       write (32,126) "x0=",x0*x_star,"    y0= ",y0*x_star, &
                   "     z0= ",z0
       write (32,126) "sx= ",sx,"    sy= ",sy,"     sz= ",sz
!       write (32,126) "xmetr= ",x0*length,"    u= ",vxt*ws,"+", &       ! changed <----------
       write (32,126) "xmetr= ",x0*xunit,"    u= ",vxt*ws,"+", &
                  xrand(innert+1,jj)*ws,"=",vxt*ws+xrand(innert+1,jj)*ws
!       write (32,126) "ymetr= ",y0*length,"    v= ",vyt*ws,"+", &       ! changed <----------
       write (32,126) "ymetr= ",y0*xunit,"    v= ",vyt*ws,"+", &       
                  yrand(innert+1,jj)*ws,"=",vyt*ws+yrand(innert+1,jj)*ws
       write (32,126) "zmetr= ",z0*lw,"    w= ",vzt*ws,"+", &
                  zrand(innert+1,jj)*ws,"=",vzt*ws+zrand(innert+1,jj)*ws
      endif
!      write (32,126) "ymetr= ",y0*length,"    v= ",vyt*ws,"+",
!     .            yrand(innert+1,jj)*ws,"=",vyt*ws+yrand(innert+1,jj)*ws
!      write (38,130) x0*length,y0*length,z0*lw," --- ",
!     .  vxt*ws+xrand(innert+1,jj)*ws,vzt*ws+zrand(innert+1,jj)*ws,
!     .  vyt*ws+yrand(innert+1,jj)*ws,
!     .  " --- ",sx,sy,sz," --- ",x0,xp2,xp
      endif

      if (pt.eq.1 .and. innert.eq.0) then
        print 126, "xp= ",xp,"    zp= ",zp

        print 126, "x0=",x0*x_star,"    y0= ",y0*x_star, &
                   "     z0= ",z0

        print 126, "sx= ",sx,"    sy= ",sy,"     sz= ",sz

!        print 126, "xmetr= ",x0*length,"    u= ",vxt*ws,"+", &         ! changed  <----------
        print 126, "xmetr= ",x0*xunit,"    u= ",vxt*ws,"+", &
                  xrand(innert+1,jj)*ws,"=",vxt*ws+xrand(innert+1,jj)*ws

!         print 126, "ymetr= ",y0*length,"    v= ",vyt*ws,"+", &         ! changed <----------
         print 126, "ymetr= ",y0*xunit,"    v= ",vyt*ws,"+", &         
                  yrand(innert+1,jj)*ws,"=",vyt*ws+yrand(innert+1,jj)*ws

            print 126, "zmetr= ",z0*lw,"    w= ",vzt*ws,"+", &
                  zrand(innert+1,jj)*ws,"=",vzt*ws+zrand(innert+1,jj)*ws

      endif

!      if(z0+sz/lw.lt.z(1) .and. nextstep(jj).eq..true.) then
!        nextstep(jj)=.false.
!        z0=0
!      endif
      if(z0+sz/lw.lt.z(1) .and. z0.ne.0) then
!        nextstep(jj)=.false.
        z0=0
      endif
!      if(nextstep(jj).eq..true.) print 127,"        true"
!      if(nextstep(jj).eq..false.) print 127,"        false"

!      if(nextstep(jj).eq..true.) then
      if(z0.ne.0) then
!        print 124, "weqweqweq",xp
if (jj .eq. 1 .and. innert .eq. 0) print 126, "x0= ",x0
!        x0=x0+sx/length          ! changed <----------
!        y0=y0+sy/length          ! changed <----------
        x0=x0+sx/xunit         
        y0=y0+sy/xunit         
        z0=z0+sz/lw
      endif


      if (l_x_m .gt. 0) then 
        if (dist .lt. abs((z(zcentr)-z0)*ch2ms) .and. x0*xunit .lt.l_x_m ) then 
    	  dist = abs((z(zcentr)-z0)*ch2ms) 
        endif
      else
	if (dist .lt. abs((z(zcentr)-z0)*ch2ms) .and. x0 .lt.l_x ) then 
    	  dist = abs((z(zcentr)-z0)*ch2ms) 
	endif
      endif    

!+      if (dist < abs((z(zcentr)-z0)*ch2ms) .and. x0<4. ) then 
!+	dist = abs((z(zcentr)-z0)*ch2ms) 
!+      endif

      x0p(innert+1,jj)=x0
      y0p(innert+1,jj)=y0
      z0p(innert+1,jj)=z0

8       continue
23      continue

      write (45,791) nPartInFile, t, innert	  ! add     <--------------


      if (print_mydata.eq.1) close(32)
      close(34)
7     continue
      print 124, " max dist from the centre of the source", dist
!      print 124, " x0=4 :", 4.*length
      CALL CPU_TIME(t3)
      Close(43)
!       конец цикла по времени


!       вывод координат частичек
	  fileToOpen = trim(pdf_folder)//"data/coordinates.dat"
      open (41,file=fileToOpen)
      do 61 innert=0,nt
            do 62 jj=1,nrandom
                  if (x0p(innert+1,jj)*x_star .le. 4) &
                    write(41,135) x0p(innert+1,jj)*x_star &
                             ,y0p(innert+1,jj)*x_star &
                             ,z0p(innert+1,jj) &
                             ,innert+1,jj
62            continue
61      continue
      close(41)

!      построение концентрации
!c      open (42,file="data\ERROR.txt")
!c      do 50 innert=0,nt
!c      do 51 jj=1,nrandom
!c      ix=-1
!c      iy=-1
!c      iz=-1
!c        do 52 i=1,500
!c            if (x0p(innert+1,jj) .ge. (i-1)*dx .and.
!c     .          x0p(innert+1,jj) .lt. i*dx     ) ix=i
!c52        continue
!c        do 53 j=1,50
!c            if (y0p(innert+1,jj) .ge. -25.*dy+(j-1)*dy .and.
!c     .          y0p(innert+1,jj) .lt.  -25.*dy+j*dy     ) iy=j
!c53        continue
!c        do 54 k=1,50
!c            if (z0p(innert+1,jj) .ge. (k-1)*dz .and.
!c     .          z0p(innert+1,jj) .lt. k*dz     ) iz=k
!c54        continue
!c        if (ix.ne.-1 .and. iy.ne.-1 .and. iz.ne.-1) then
!c            nv(ix,iy,iz)=nv(ix,iy,iz)+1
!c        else
!c          write (42,136) "ERROR ",innert+1,jj," coord ",x0p(innert+1,jj)
!c     .          ,y0p(innert+1,jj),z0p(innert+1,jj)," (x,y,z) ",ix,iy,iz
!c        endif

!c51      continue
!c50      continue
!c      close(42)

!      do 55 i=1,500
!        do 56 j=1,50
!             do 57 k=1,50
!c                  nv(ix,iy,iz)=nv(ix,iy,iz)*concentr
!57          continue
!56        continue
!55      continue

!      open (40,file="concentration.dat")
!      do 58 i=1, 500
!            do 59 j=1, 50
!                  do 60 k=1, 50
!c                    write(40,135) (i-0.5)*dx,-25.*dy+(j-0.5)*dy,(k-0.5)*dz,
!                    write(40,135) (i-1.)*dx,-25.*dy+(j-1)*dy,(k-1)*dz,
!     .                nv(i,j,k)
!60                  continue
!59            continue
!58      continue
!      close (40)



!      приземная концетрация
      ii=0
!      if(t.eq.nt) then       ! (innert .eq.0)  ! условие, что последняя итерация
!      open (35,file="earth.dat")
      print 134,"end"
!     do 25 innert=0,nt
!     do 13 jj=1,nrandom
!        if(z0p(innert+1,jj).eq.0 .and. x0p(innert+1,jj)*x_star<=l_x) then
!             ii=ii+1
!           mycox(ii)=x0p(innert+1,jj)*x_star
!           mycoy(ii)=y0p(innert+1,jj)*x_star
!             print 129,mycox(ii),mycoy(ii)
!             write (35,128)mycox(ii),mycoy(ii)
!        endif
!      close (35)
!13      continue
!25    continue
!      do 14 i=1,ii-1
!        do 15 j=i+1,ii
!        if (mycox(i).gt.mycox(j)) then
!          trash=mycox(j)
!            mycox(j)=mycox(i)
!            mycox(i)=trash
!            trash=mycoy(j)
!            mycoy(j)=mycoy(i)
!            mycoy(i)=trash
!        endif
!15        continue
!14      continue

!      open (35,file="data/earth.txt")
!      do 16 j=1,ii
!            write (35,129) mycox(j),mycoy(j)
!cc            print 129,mycox(j),mycoy(j)
!16    continue
!      close (35)
!
!      do 19 i=1, ii
!        do 18 j=0, ncell*2*l-1
!        if (mycox(i).gt.j/2./l*x_star .and.
!     .      mycox(i).le.(j+1)/2./l*x_star)
!     .                       mybottom(j+1)=mybottom(j+1)+1
!18        continue
!19      continue

!      open (36,file="data\earth2.txt")
!        do 20 j=1, ncell*2*l
!            write(36,131) mybottom(j),mybottom(j)*concentr, j*0.5/(l*1.)*x_star !j/2./l*x_star
!20        continue
!      close (36)

!      do 19 i=1, ii
!        do 18 j=0, 40
!        if (mycox(i).gt.j*1./l_x .and. mycox(i).le.(j+1)*1./l_x) &
!                             mybottom(j+1)=mybottom(j+1)+1
!18        continue
!19      continue


!      open (36,file="data/earth2.txt")
!        do 20 j=1, 40
!            write(36,131) mybottom(j),mybottom(j)*concentr, j*1./l_x !
!20        continue
!      close (36)

!      endif ! конец условие, что последняя итерация



!      close(33)
!      close(38)
!      close(37)


      CALL CPU_TIME(t4)
!      ( write to characters.txt)
      write (39,134) "number of ground particles = ",ii
      write (39,124) "time initialization        = ",t2-t1
      write (39,124) "time cycle                 = ",t3-t2
      write (39,124) "time writing               = ",t4-t3
      write (39,124) "all time                   = ",t4-t1
      write (39,124) " max dist from the centre of the source", dist
!      write (39,124) " x0=4 :", 4.*length

      close(39)
      close(45)
      print 124, "time= ",t4-t1

	  open (676, FILE="history.txt",STATUS="old",POSITION="append")
	  write(676,*) "copyPDFbeta2 cancel : "//trim(pdf_folder)
	  close(676)
 99   format(1x,2i3,e16.8,i3)
 100  format(1x,2e16.8)
 101  format(1x,7e16.8)
 111  format(1x,6e13.5)
 112  format(1x,15e12.4)
! 112  format(1x,15e12.4)
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
 129  format(1x,3f10.5,i7,i7,A)
 729  format(1x,3f14.7,i7,i7)
 130  format(1x,3f11.5,3(A,3f11.5))
 131  format(1x,i6,2f12.3)
 132  format(1x,A,i7)
 134  format(1x,A,i6)
 135  format(1x,3f12.3,3i6)
 136  format(1x,A,2i6,A,3f12.3,A,3i3)
 791  format(1x,3i10)
 792  format(1x,8e16.8)
 793  format(1x,2e16.8)
 !794  format(1x,3f10.5,A,3e12.4,A,3e12.4,A,3(f8.5,1x))
 794  format(1x,3f10.5,5(A,3e12.4))
 795  format(1x,6A)
 796  format(1x,A,i3,A,e15.7)
 797  format(1x,10(A,e15.7))
 798  format(1x,A,e15.7,A,e15.7)

 


 999  stop
      end

      function fint2(iif0,iif1,iix,iix0,iix1,ind)
      real iif0,iif1,iix
      integer iix0,iix1
      logical ind

      fint2=iif0+(iif1-iif0)/((iix1-iix0)*1.)*(iix-iix0*1.)

      if (ind .eqv. .true.) print 125,"f0=",iif0," f1=",iif1," fx=",iix, &
                             " x0=",iix0*1.," x1=",iix1*1.," fint2=",fint2
      return
 125  format(1x,6(A,e12.5))
      end

      function fint4(iif1,iif2,iif3,iif4,iix,iiz,iix0,iix1,iiz0,iiz1)
      real iif1,iif2,iif3,iif4,iix,iiz,iix0,iix1,iiz0,iiz1
      f0=iif1+(iif3-iif1)/(iiz1-iiz0)*(iiz-iiz0)
      f1=iif2+(iif4-iif2)/(iiz1-iiz0)*(iiz-iiz0)
      fint4=f0+(f1-f0)/(iix1-iix0)*(iix-iix0)

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


      function fint10(iif1,iif2,iif3,iif4,iix,iiz,iix0,iix1,iiz0,iiz1)
      real iif1,iif2,iif3,iif4,iix,iiz
      integer iix0,iix1,iiz0,iiz1
      pntz=Floor(iiz);pntx=Floor(iix)
      if (iiz.eq.iiz0*1.) then
        if(iix.eq.iix0*1.) fint10 = iif1
        if(iix.eq.iix1*1.) fint10 = iif2
        if(iix.gt.iix0*1. .and. iix.lt.iix1*1.) fint10=iif1+(iif2-iif1)/ &
                (iix1-iix0*1.)*(iix-iix0*1.)
      endif
      if (iiz.eq.iiz1) then
        if(iix.eq.iix0*1.) fint10 = iif3
        if(iix.eq.iix1*1.) fint10 = iif4
        if(iix.gt.iix0*1. .and. iix.lt.iix1*1.) fint10=iif3+(iif4-iif3)/ &
                (iix1-iix0)*1.*(iix-iix0*1.)
      endif

      if (iiz.gt.iiz0*1. .and. iiz.lt.iiz1*1.) then
        if (iix.eq.iix0*1.) fint10=iif1+(iif3-iif1)/ &
                (iiz1-iiz0)*1.*(iiz-iiz0*1.)
        if (iix.eq.iix1*1.) fint10=iif2+(iif4-iif2)/ &
                (iiz1-iiz0)*1.*(iiz-iiz0*1.)
      endif
      if (iiz.gt.iiz0*1. .and. iiz.lt.iiz1*1. .and. &
           iix.gt.iix0*1. .and. iix.lt.iix1*1.) then
        f0=iif1+(iif3-iif1)/(iiz1-iiz0)*1.*(iiz-iiz0*1.)
        f1=iif2+(iif4-iif2)/(iiz1-iiz0)*1.*(iiz-iiz0*1.)
        fint10=f0+(f1-f0)/(iix1-iix0)*1.*(iix-iix0*1.)
      endif

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

      SUBROUTINE gauss1(mu,sigma,r1)
      real x,y,sum
1     CALL RANDOM_NUMBER(x)
      CALL RANDOM_NUMBER(y)
      x=x+x-1.
      y=y+y-1.
      sum=x*x+y*y
      if (sum.gt.1) go to 1
      part=alog(sum)
      part=sqrt((-part-part)/sum)
      r1=x*part*sigma + mu
      return
      end

      SUBROUTINE gauss2(mu,sigma,r1,r2)
      real x,y,sum
1      CALL RANDOM_NUMBER(x)
      CALL RANDOM_NUMBER(y)
      x=x+x-1.
      y=y+y-1.
      sum=x*x+y*y
      if (sum.gt.1) go to 1
      part=alog(sum)
      part=sqrt((-part-part)/sum)
      r1=x*part*sigma + mu
      r2=y*part*sigma + mu
      return
      end
