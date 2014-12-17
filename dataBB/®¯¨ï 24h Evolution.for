      program main
      common /a/ z(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /qq/ aq(151),aqe(151),aq2(151),aq2e(151),aqi(151),aq2i(151),
     .      aqy(151),aq2y(151),wq(151),bqt(151),rad(151)
     . /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .        vw(151),ut(151),vt(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .       ,ehe(151),eh(151),ehi(151),ehy(151)
     . /kor3/ w3(151),w2t(151),wt2(151),t3(151),u2w(151),v2w(151)
     . /frm/ frm1(151),frm2(151),frm3(151),frm4(151),frm5(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const1/ sie,ce1,ce2,cq1,cq3,rr,dsq1
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      dimension ehw(151),aqt(151)
      character*7 nam_n
      character*8 nam_d
	character*20 nam_d_tr, nam_n_tr, nam_n_my
      open(10,file='nd.d')
      read(10,190) en,ug,fkor,hmax,z0,x
      n=en
      read(10,779) (z(k),u(k),v(k),e(k),ep(k),w2(k),t(k),t2(k),
     . aq(k),aq2(k),wq(k),k=1,n)
      close(10)
      open(10,file='zi.dat')
	open(17,file='ziclock.dat')
      write(10,95)
c ------- setup radiative transfer ------
      irad=1
c ---------------------------------------
      cap=.4
      cm=0.
      cs1=2.4
      cs2=-cs1
      cs7=-1.
c -------------------
	c1=1.8
      c2=.6
      c3=.5
c -------------------
      ct1=3.
      ct2=.346
      ct3=.333
      r=1.25
      ds1=0.5
      ds2=0.3
      dst1=0.806
      ds3=0.
      dst2=0.
c -------------------
	cq1=ct1
	cq3=ct3
	rr=r
	dsq1=dst1
      cep=21.
c -----cond: 3/(c7c8)=3/(c9c10)=4/(c8c9)=pi/18 ------
	    c7=4.2
	    c8=4.8
	    c9=c8
		c10=c7
          c11=0.
c -------------------------------------------------------------------
      cl=1.
      cw=0.
      cs=1./c7
      ced=cs
      ct=1./c9
	dst3=0.
	api=3.1416
      print 101, cs,c1,c2,ct1,ct2
      print 103, c7,c8,c9,c10,c11,dst3
      xt=x*hmax/ug/3600.
      qtq=qt0(xt+9.)/ug
      g=9.8*hmax/ug**2
      bt=1./285.
      print 77
      dx=.01
      jkl=96600
!      read(*,*) jkl,dx
! ===============
	dx=.001
	jkl=140000
      print 96
!      read(*,*) ljj
	ljj=3000
! ===============
c     dx=z0/hmax
c    -----------------------------------
      ney=1
      jc=3600./(dx*hmax/ug)
c    -----------------------------------
      i9=jkl
      i1=i9+100
      i2=i1+jc
      i3=i2+jc
      i4=i3+jc
      i5=i4+jc
      i6=i5+jc
      i7=i6+jc
      i8=i7+jc
c    -----------------------------------
      am=fkor*hmax/ug
      Ro=ug/fkor/z0
      print 74,ug,z0,hmax,Ro,qtq
      us=.035
      z00=z0/hmax
      t20=1.
      jprn=1
      kk=0
      prs=.01
      lfi=1
      nxx=0
      jh=0
      kres=20
      do 2 j=1,n
	if(j.ge.kres)then
	 k=j-kres+1
	 kk=1
	else
	 k=n-j+1
	 kk=0
	endif
	 frm1(k)=z(j)*kk
       ue(k)=u(j)*kk
       ve(k)=v(j)*kk
       ee(k)=e(j)*kk
       w2e(k)=w2(j)*kk
       epe(k)=ep(j)*kk
	 te(k)=t(j)*kk
	 t2e(k)=t2(j)*kk
       aqe(k)=aq(j)*kk
       aq2e(k)=aq2(j)*kk
	 z(j)=0.
  2    continue
 	do 4 k=1,n
	 z(k)=frm1(k)
       w2(k)=w2e(k)
       eh(k)=(ee(k)-w2e(k)/2.)
       ehe(k)=eh(k)
       u(k)=ue(k)
       v(k)=ve(k)
       e(k)=ee(k)
       ep(k)=epe(k)
       ui(k)=u(k)
       vi(k)=v(k)
       ei(k)=e(k)
       w2i(k)=w2(k)
       ehi(k)=eh(k)
       epi(k)=ep(k)
       uy(k)=u(k)
       vy(k)=v(k)
       ey(k)=e(k)
       w2y(k)=w2(k)
       ehy(k)=eh(k)
       epy(k)=ep(k)
       aq(k)=aqe(k)
       aqi(k)=aqe(k)
       aqy(k)=aqe(k)
       aq2(k)=aq2e(k)
       aq2i(k)=aq2e(k)
       aq2y(k)=aq2e(k)
       t(k)=te(k)
       t2(k)=t2e(k)
       ti(k)=t(k)
       t2i(k)=t2(k)
       wte(k)=wt(k)
       ty(k)=t(k)
       t2y(k)=t2(k)
       al=cap*z(k)/(1.+cap*z(k)*hmax/40.)
       fs(k)=al/cap/z(k)
   4   continue
      n=n-kres+1
      call akorr
      nzi=0
      wqi=0.
      do 6 k=1,n
      if(wte(k).lt.wqi)then
       wqi=wte(k)
       ki=k
      endif
   6  continue
      zi=z(ki)
      zim=zi*hmax
      if(qtq.le.0.)then
       ws=us
      else
       ws=(bt*g*qtq*zi)**(1./3.)
      endif
      us=(qtq+0.02)*.8
	xm=xt+9.
	usf=0.15*exp(-((xm-15.)/3)**2)+0.12*exp(-((xm-6.)/10)**2)
     .      +0.22*exp(-((xm-33.)/3)**2)
     .      +0.07+0.025*exp(-((xm-30.)/10)**2.)
	us=usf/ug
      ali=-us**3/(cap*bt*g*qtq)
      dus=us*us
      aqn=aq(n)
      do 40 i=1,jkl
      if(i.ne.1)then
        dx1=dx2
        dx2=dx2
        x=x+dx2
      else
       dx1=dx*.01
       dx2=dx
       x=x+dx2
      endif
       dx3=dx1+dx2
       vmax=0.
      xt=x*hmax/ug/3600.
	xm=xt+9.
      qtq=qt0(xt+9.)/ug
	tc=t1(xt+9.)
      h0=qtq*1.3*.0001
	usf=0.15*exp(-((xm-15.)/3)**2)+0.12*exp(-((xm-6.)/10)**2)
     .      +0.22*exp(-((xm-33.)/3)**2)
     .      +0.07+0.025*exp(-((xm-30.)/10)**2.)
	us=usf/ug
      do 20 jue=1,3
c-------- Boundary conditions ----------------
      ali=-us**3/(.35*bt*g*qtq)
	 sim1=sim(ali,z(1))
	 sim2=sim(ali,z(2))
       ded=(alog(z(1)/z00)-sim1)/(alog(z(2)/z00)-sim2)
      if(qtq.gt.0.00001)then
c ------- Convective PBL ----------------------
       ec=us**2*((7.+.52*(-zi/ali))**(2./3.)
     . +.85*(1.+3.*(-z(1)/ali))**(2./3.))
       epc=us**3/cap/z(1)*(1.+.5*(-z(1)/ali)**(2./3.))**(3./2.)
       w2c=(1.3*us*(1.-3.*z(1)/ali)**(1./3.))**2
c       w2c=(1.75+2.*(-z(1)/ali)**(2./3.))*us**2
       t2c=(qtq/us)**2*4.*(1-8.3*z(1)/ali)**(-2./3.)
c       t2c=(qtq/us)**2*2.4*(1-8.3*z(1)/ali)**(2./3.)
      else
c ------- Stable stratisfication --------------
       ec=us**2*(2.*7.**(2./3.)+1.7)
       ec=us**2*(7.**(2./3.)+.85)
       epc=us**3/cap/z(1)
       w2c=(1.3*us)**2
       t2c=(qtq/us)**2*4.
c      t2c=(qtq/us)**2*2.4
      endif
c ---------------------------------------------
      q2c=t2c*(.00013)**2
      call p6(dx1,dx2,qtq,t2c,irad,prs,tc,cl,ug,jt)
      call p7(dx1,dx2,ney,prs,am,ded,ju)
      call p8(dx1,dx2,qtq,prs,ec,epc,w2c,cw,cl,je)
      call p9(dx1,dx2,h0,prs,aqn,cl,jq)
      call akorr
      nzi=0
      wqi=0.
	aib1=0.
	aib2=0.
      do 12 k=1,n
c       ee(k)=ehe(k)+w2e(k)/2.
       ui(k)=ue(k)
       vi(k)=ve(k)
       w2i(k)=w2e(k)
       ehi(k)=ehe(k)
       ei(k)=ee(k)
       epi(k)=epe(k)
       ti(k)=te(k)
       t2i(k)=t2e(k)
       aqi(k)=aqe(k)
       aq2i(k)=aq2e(k)
       wti(k)=wte(k)
c       al=cap*z(k)/(1.+cap*z(k)/zi/.2)
       al=cap*z(k)/(1.+cap*z(k)/zi/.1)
c       al=cap*z(k)/(1.+cap*z(k)/.003)
       fs(k)=al/cap/z(k)
       if(k.ne.1.and.k.ne.n.and.nzi.ne.1.and.wte(k).lt.0.)then
        if(wte(k+1).ge.0.and.wte(k).lt.0.)nzi=1
        if(wte(k).lt.wqi)then
         wqi=wte(k)
         ki=k
        endif
       endif
	if(k.ne.n)then
       aib1=aib1+(sqrt(ee(k))*z(k)+sqrt(ee(k+1))*z(k+1))*(z(k+1)-z(k))
	 aib2=aib2+(sqrt(ee(k))+sqrt(ee(k+1)))*(z(k+1)-z(k))
	endif

  12  continue
      alb0=0.1*aib1/aib2
      wnn=bt*g*dt(n-2)
c      ali=-us**3/(.35*bt*g*(qtq+0.61*h0))
      if(qtq.le.0.)then
       zi=.5
       zi=1.7*us/sqrt(-fkor*wnn)
       wqi=0.
c       ali=100000.
      else
       zi=z(ki)
       wqi=wqi/qtq
c       ali=-us**3/.35/bt/g/qtq
      endif
      zim=zi*hmax
      if(qtq.le.0.)then
       ws=us
      else
       ws1=(bt*g*qtq*zi)**(1./3.)
	 if(ws1.ge.us)ws=ws1
      endif
      if(irad.eq.0)then
       if(jt.eq.1.and.ju.eq.1.and.je.eq.1.and.jue.ge.2)go to 11
      else
       if(jt.eq.1.and.ju.eq.1.and.je.eq.1.and.jq.eq.1.
     . and.jue.ge.2)go to 11
      endif
  20   continue
  11  continue
      all=0.
      do 21 k=1,n
      uy(k)=u(k)
      vy(k)=v(k)
      ey(k)=e(k)
      w2y(k)=w2(k)
      ehy(k)=eh(k)
      epy(k)=ep(k)
      ty(k)=t(k)
      t2y(k)=t2(k)
      aqy(k)=aq(k)
      aq2y(k)=aq2(k)
c      w2ty(k)=w2t(k)
      u(k)=ui(k)
      v(k)=vi(k)
      e(k)=ei(k)
      w2(k)=w2i(k)
      eh(k)=ehi(k)
      ep(k)=epi(k)
      t(k)=ti(k)
      t2(k)=t2i(k)
      aq(k)=aqi(k)
      aq2(k)=aq2i(k)
      wt(k)=wti(k)
c      w2t(k)=w2ti(k)
      ehw(k)=(u2w(k)+v2w(k))/2.
      agg=atan(v(k)/u(k))*57.3
      if(agg.lt.agll)agll=agg
      all0=w2e(k)**(3./2.)/ep(k)
      if(k.le.ki.and.all0.gt.all)then
            kil=k
            all=all0
      endif
  21   continue
      lkj=i/ljj-jjl
      xt=x*hmax/ug/3600.
      xtt=xt+9.
      if(lkj.ge.1)then
       jjl=i/ljj
       wuss=w2e(1)/w2c
	wuss=bt*g*dt(1)
	sswu=wuss/abs(wuss)*sqrt(abs(wuss))
       wss=ws*ug
	 uss=us*ug
       eu=e(1)/us/us
       epu=ep(1)/us/us/us*cap*z(1)
       all=.26*w2e(kil)**(3./2.)/ep(kil)/zi
       qttq=qtq*ug
       wq1=wq(1)*ug*100000.
       usv=sqrt(sqrt(uw(1)**2+vw(1)**2))*ug
c       usv=sqrt((u2(1)+v2(1))/2.8)*ug
       all0=z(1)/(ee(1)**(3./2.)/ep(1))
	 te1=te(1)
	 if(xtt.ge.24.)then
	  xttt=xtt-24.
       else
	  xttt=xtt
       endif
      dwt=(wt(2)-wt(1))/(z(2)-z(1))
	hdwt=-dwt/hmax*ug
	hrad=-rad(k)/hmax*ug
	bud=hdwt+hrad
	bud=t(1)-ty(1)
	tau1=ee(1)/epe(1)
	bwn1=bt*g*dt(1)
	det=1.+(1.-c3)/ct1/(c1+3./2.*ds1*fs(1))*bwn1*tau1**2
	uw1=tau1/(c1+3./2.*ds1*fs(1))/det
     .   *((1.-c2*(1-2./3.*ds2*fs(1)))*w2e(1)-
     .   (1.-ct2)/ct1*bt*g*tau1*qtq)*du(1)
	vw1=tau1/(c1+3./2.*ds1*fs(1))/det
     .   *((1.-c2*(1-2./3.*ds2*fs(1)))*w2e(1)-
     .   (1.-ct2)/ct1*bt*g*tau1*qtq)*dv(1)
      h1=z(2)-z(1)
	h2=z(3)-z(2)
       uw1=uw(2)-h1/h2*(uw(3)-uw(2))
       vw1=vw(2)-h1/h2*(vw(3)-vw(2))
	 uvw1=sqrt(sqrt(uw1**2+vw1**2))
       print 70,i,xttt,jue,ju,je,jt,jq,uss,agll,eu,all,w2c/us
       print 93,qttq,wq1,aqe(1),te1
       print 100,zim,wss,wqi
       qtqq=qtq*ug
       write(10,774) xttt,uss,uvw1*ug,qtqq,zim,w2c,te1
      endif
      wss=ws*ws
c------- запись данных через каждый час -----
      ntame=int(xtt)+1
      tame1=xtt-ntame
      dxt=dx2*hmax/ug/3600.
      if(abs(xtt-ntame).le.dxt.or.i.eq.jkl)then
       if(ntame.gt.24)ntame=ntame-24
! ===============	
       write(nam_n,'(A,i2,A)') 'n',ntame,'.dat'
	 write(nam_n_tr,'(A,i2,A)') 'n',ntame,'tr.dat'
	 write(nam_n_my,'(A,i2,A)') 'my_n',ntame,'tr.dat'
       write(nam_d,'(A,i2,A)') 'ndd',ntame,'.d'
	 write(nam_d_tr,'(A,i2,A)') 'ndd',ntame,'tr.d'
       if(i.ne.jkl)then
        open(11,file=nam_d)
        open(12,file=nam_n)
        open(13,file=nam_d_tr)
        open(14,file=nam_n_tr)
        open(15,file=nam_n_my)
       else
        open(11,file='nddE.d')
        open(12,file='nE.dat')
        open(13,file='nddEtr.d')
        open(14,file='nEtr.dat')
        open(15,file='my_nE.dat')
       endif
       write(11,190) en,ug,fkor,hmax,z0,x,dx2
       write(11,779) (z(k),u(k),v(k),e(k),ep(k),w2(k),t(k),t2(k),               ! 'ndd15.d'
     .                aqe(k),aq2e(k),wq(k),k=1,n)
	 write(13,792) zi,ws,0.,0.,0.,0.,0.,0.,0.,0.
       write(13,792) (z(k),eh(k),ep(k),w2(k),w3(k),ue(k),ve(k)				  ! 'nddtr.d'
     .               ,sqrt(u(k)**2+v(k)**2)
     .               ,2.*api/(sqrt(1.9*1.6/eh(k))**3*ep(k) ),k=1,n)
! ===============	
      wsss=wss*ws
      c41=(2.*c7-1.)
      c42=(2.*c8-1.)
	qtt=abs(qtq)
	write(14,792) zi,ws,0.,0.,0.,0.,0.,0.,0.
      do 61 k=1,n
      zz=z(k)/zi
      zh=z(k)*Hmax
      hq=aqe(k)*1000.
      hu=ue(k)/ug
      hv=ve(k)/ug
      hw2=w2e(k)/wss
      heh=ehe(k)/wss
      hepx=ep(k)/ws/wss*zi
      huw=uw(k)/wss
      hvw=vw(k)/wss
      hwt=wt(k)/qtt
      ht2=t2(k)/(qtt/ws)**2
      hq2=aq2e(k)*1000000.
      hwq=wq(k)*100000.*ug
      hw3=w3(k)/wsss
      hwt2=wt2(k)/qtt/qtt*ws
      hw2t=w2t(k)/ws/qtt
      hew=.5*(v2w(k)+u2w(k)+w3(k))/wsss
      hu2w=u2w(k)/wsss
      hv2w=v2w(k)/wsss
      alb=cap*z(k)/(1.+cap*z(k)/alb0)
	abb=sqrt(ee(k))**3
      albn=abb/epe(k)
      if(dt(k).ge.0.)then
		als=0.75*sqrt(ee(k)/(bt*g*dt(k)))
      	alan=amin1(als,alb)
          epb=(0.019+0.051*alan/alb)*abb/alan
      else
	 epb=(0.019+0.051)*abb/alb
      endif
      ta=ee(k)/epe(k)
      wn=bt*g*dt(k)
	tkk=t(k)
	umod=sqrt(hu**2+hv**2)
! ===============	
	abb=sqrt(e(k)*ug*ug/ws/ws)**3
      albn=abb/(ep(k)/ws/ws*zi*ug*ug)
	lambda = 2.*api/(sqrt(1.9*1.6/(e(k)*ug*ug/ws/ws))**3
     .           *ep(k)/ws/ws*zi*ug*ug)
! ===============
	tka=(te(k)+273.)/(1.+0.61*aqe(k))-30.*z(k)
      write(12,783) zz,zh,umod,hv,tkk,hq,hw2,heh,hepx,ht2,hwt,			! 'n15.dat'
     . huw,hvw,hwq,hq2,hw3,hew,hw2t,epb,epe(k),rad(k)/hmax*ug,tka-273.
! ===============	
      write(14,792) zz,heh,hepx,hw2,hw3,hu,hv,sqrt(hu**2+hv**2)           ! 'n15tr.dat'
     .             ,lambda
      write(15,792) z(k)/zi,e(k)*ug*ug/ws/ws,ep(k)/ws/ws*zi*ug*ug			! 'my_n15tr.dat'
     .             ,w2/ws/ws,w3/ws/ws/ws,u*ug/ws,v*ug/ws
     .             ,sqrt((u*ug/ws)**2+(v*ug/ws)**2),lambda
! ===============	
 61   continue
       close(11)
       close(12)
	 close(13)
	 close(14)
	 close(15)
      endif
  40  continue
      close(10)
	close(17)
      open(12,file='ndd.d')
      write(12,190) en,ug,fkor,hmax,z0,x,dx2
      write(12,779) (z(k),u(k),v(k),e(k),ep(k),w2(k),t(k),t2(k),
     . aqe(k),aq2e(k),wq(k),k=1,n)
      close(12)
      open(12,file='nddN.d')
      write(12,190) en,ug,fkor,hmax,z0,x,dx2
      write(12,784) (z(k),u(k),v(k),e(k),ep(k),w2(k),t(k),t2(k),
     . aqe(k),aq2e(k),wq(k),aqt(k),k=1,n)
      close(12)
      wsss=wss*ws
      open(14,file='res.dat')
      open(15,file='frm.dat')
      open(16,file='rfr.dat')
      do 69 k=1,n
      tau=ee(k)/epe(k)
      if(wn.ge.0.)then
       dtt=1.+dst3*tau**2*wn
      else
       dtt=1.
      endif
	tad=tau/dtt
      zz=z(k)/zi
      ww2=w2e(k)/wss
      wtt=wt2(k)/qtq/qtq*ws
      wwt=w2t(k)/ws/qtq
      www=w3(k)/wsss
      eww=.5*(v2w(k)+u2w(k)+w3(k))/wsss
      uuw=u2w(k)/wsss
      vvw=v2w(k)/wsss
      weh=ehe(k)/wss
      wee=ee(k)/wss
      if(k.ne.1.and.k.ne.n)then
       h1=z(k)-z(k-1)
       h2=z(k+1)-z(k)
       h3=z(k+1)-z(k-1)
       dwt=h2/h1/h3*(wt(k)-wt(k-1))+h1/h2/h3*(wt(k+1)-wt(k))
      else
       if(k.eq.1)then
        h11=z(2)-z(1)
        dwt=(wt(2)-wt(1))/h11
       else
        h22=z(n)-z(n-1)
        dwt=(wt(n)-wt(n-1))/h22
       endif
      endif
	hdwt=-dwt/hmax*ug
	hrad=-rad(k)/hmax*ug
	bud=hdwt+hrad
	hfrm=frm1(k)/hmax*ug
      write(14,779) zz,eww,www,wwt,wtt,ww2,weh,eww,wq(k),vvw,bqt(k)
      write(15,99) z(k),frm1(k)/hmax*ug,frm2(k)/hmax*ug,frm3(k)/hmax*ug,
     . frm4(k)/hmax*ug,frm5(k)/hmax*ug
      write(16,71) zz,z(k)*hmax,w2e(k),hfrm,bud,hdwt,hrad
 69   continue
      close(14)
      close(15)
      close(16)
  70  format('t(',i5,')=',f5.2,1x,'j='i2,i2,i2,i2,i2,1x,'u*=',f5.4,
     . 1x,'alf=',f5.1,1x,'e=',f5.2,1x,'l=',f6.3,1x,'w2c=',e9.2)
  71  format(1x,7e14.5)
  72  format(2(2x,f10.7))
  73  format(2x,f12.2,2x,3f15.10)
  74  format(2x,'G=',f4.1,'m/s',2x,'Zo=',f5.4,'m',2x,'H=',f6.1,'m',
     . 2x,'Ro=',e7.1,2x,'Qo=',f8.4)
  75  format(2x,f10.4,2f7.4)
  76  format(2x,f10.4,2f7.4,2(f14.1,f7.4))
  77  format('jkl=?,dx=?')
  78  format(1x,5f13.5)
  79  format(2x,8f9.7)
  80  format(2x,'x=',e19.5)
  81  format(2x,'juv=',i2,2x,'pru=',f10.6,2x,'prv=',f10.6)
  82  format(2x,'je=',i2,2x,'pre=',f10.6)
  83  format(2x,'wind direction at x=',f15.1)
  84  format(5x,i4,2x,i4,2x,f10.2,2x,i2,2x,f15.12,2x,f10.8)
  85  format(2x,i3,2x,12e10.3)
  86  format(3x,'n',7x,'z(n)',6x,'u',8x,'v',8x,'w',10x,'e',
     *8x,'l',8x,'k',10x,'<uw>',6x'a',9x,'diff',6x,'p',8x,'diss')
  87  format(2x,'u*=',f10.3,2x,'ec=',f12.5,2x,'epc=',f15.9)
  88  format(70x,'the vertical profiles for x(',i4,')=',f7.2)
  89  format(2x,3e12.4)
  90  format(5(5x,e10.4))
  91  format(2x,'x(',i4,')=',e9.3)
  92  format(6x,'z',11x,'x=',e9.3,3x,'x=',e9.3,3x,
     *       'x=',e9.3,3x,'x=',e9.3)
  93  format(7x,'Qo=',e15.7,2x,'<wq>c=',e10.4,2x,
     . 'qc=',e10.4,2x,'Tc=',f6.2)
  94  format(6x,'z',16x,'G',10x,'A',11x,'diff',10x,'p',13x,'diss')
  95  format(8x,'t',9x,'u*',11x,'w*',12x,'Qo',10x,'Alf',10x,'zi')
  96  format(1x,'Задать частоту вывода информации на экран: llj=?')
  97  format(8(5x,e9.3))
  98  format(2x,4e12.4)
  99  format(6(5x,e10.4))
 100  format(7x,'Zi=',f7.2,' m',2x,'W*='f6.2,
     . ' m/s',2x,'<wt>i/Q=',f9.6,/)
 101  format(2x,'Cs=',f7.3,2x,'C1=',f9.3,2x,'C2=',f9.3
     . ,2x,'Ct1=',f9.3,2x,'Ct2=',f9.3)
 102  format(2x,'W2/u*=',f9.3)
 103  format(2x,'C7=',f5.2,2x,'C8=',f5.2,2x,'C9=',f5.2
     . ,2x,'C10=',f5.2,2x,'C11=',f5.2,2x,'dst2=',f9.6)
 190  format(2x,7e12.5)
 191  format(2x,8e12.5)
 192  format(2x,'------')
 193  format(2x,7e12.5)
 194  format(2x,5e13.5)
 393  format(5(5x,e10.4))
 771  format(1x,i3,5f13.9)
 772  format(1x,8e13.5)
 773  format(1x,10e16.5)
 774  format(2x,7e13.5)
 775  format(2x,9e16.8)
 776  format(2x,8e16.8)
 778  format(1x,12e16.8)
 779  format(1x,11e16.8)
 780  format(1x,48e16.8)
 781  format(1x,13e16.8)
 782  format(1x,16e16.8)
 783  format(1x,22e16.8)
 784  format(1x,12e16.8)
 792  format(1x,9e16.8)
 999  stop
      end
      
      subroutine puz(zi,qtq,h0,z00,us,ku)
      common /a/ z(151)
     . /res/ u(151),v(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      zfik=.0548
	zfik=0.1
c	zfik=0.05
      do 1 k=1,n-1
        if(z(k).le.zfik*zi.and.z(k+1).gt.zfik*zi)ku=k
    1 continue
c      ku=5
      amo=sqrt(u(ku)**2+v(ku)**2)
      azq=alog(z(ku)/z00)
      us=cap*amo/azq
	kt=ku
	dt1p=-us**2/qtq*epe(kt)/1.75/ee(kt)*(ct1+dst1*fs(kt))
      dt1v=bt*g*(1-ct3)*4./1.75*qtq**2/us**4
	dt1p=-qtq*epe(kt)/ee(kt)*(ct1+dst1*fs(kt))
      dt1v=bt*g*(1-ct3)*t2e(kt)/w2e(kt)
	dt1=dt1p+dt1v
c	dt1=-us**4/(3.5*1.75*cap*z(kt)*qtq)
      do 2 k=1,100
      ali=-us**3/(.35*bt*g*(qtq+0.61*h0))
      ali=-us**3/(.35*bt*g*qtq)
	az=z(ku)/ali
       if(qtq.gt.0.)then
        a1=1./sqrt(sqrt(1.-15.*az))
       else
        a11=1.+4.7*az
	  al=0.75*sqrt(ee(kt)/bt*g*dt1)
c	  az=z(ku)/al
	  a1=1.+4.7*az
c	  a1=amin1(a11,a12)
       endif
c       print 10, qtq, az, us
       amo1=sqrt(u(ku-1)**2+v(ku-1)**2)
       amo3=sqrt(u(ku+1)**2+v(ku+1)**2)
       us1=(amo3-amo1)/(z(ku+1)-z(ku-1))*cap*z(ku)/a1
       if(abs((us1-us)/us).le..00001)then
        goto 3
       else
        us=us1
       endif
 2    continue
 3    continue
 10   format(2x, 3e12.4)
      return
      end

      function sim(ali,a)
      common /a/ z(151)
c------- Calculation of stability function for BC -------
 	rfc=0.19
      xs=a/ali
	if(xs.le.0.)then
	 sim=-0.9904*alog(1.-14.264*xs)
      else
       sim=xs/rfc+1.1223*exp(1.-1.6666/xs)
	endif
      return
      end

      subroutine p9(dx1,dx2,h0,prs,aqn,cl,jq)
      common /a/ z(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /qq/ aq(151),aqe(151),aq2(151),aq2e(151),aqi(151),aq2i(151),
     .      aqy(151),aq2y(151),wq(151),qt(151),rad(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .        vw(151),ut(151),vt(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /w2tw/ w2t(151),wt2(151),w3(151),u2w(151),v2w(151)
     . /frm/ frm1(151),frm2(151),frm3(151),frm4(151),frm5(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const1/ sie,ce1,ce2,cq1,cq3,rr,dsq1
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      dimension a(151,2,2),b(151,2,2),c(151,2,2),f(151,2),rm(151,2)
     . ,ak(151),akt(151),w2q(151),qt2(151)
	cq8=c8
	cq9=c9
	cq10=c10
      dx3=dx1+dx2
      f1=dx2/dx1
      f2=-dx3**2/dx1/dx2
      f3=2.+dx1/dx2
c	do 9 k=1,n
c	aqe(k)=aqi(k)
c	aq2e(k)=aq2i(k)
c  9    continue
      do 11 k=1,n
      ta=ei(k)/epi(k)
      wn=bt*g*dt(k)
      if(wn.ge.0.)then
       dtt=1.+dst3*ta**2*wn
      else
       dtt=1.
      endif
      if(k.ne.1.and.k.ne.n)then
       h1=z(k)-z(k-1)
       h2=z(k+1)-z(k)
       h3=z(k+1)-z(k-1)
       dwq=h2/h1/h3*(wq(k)-wq(k-1))+h1/h2/h3*(wq(k+1)-wq(k))
       dqt=h2/h1/h3*(qt(k)-qt(k-1))+h1/h2/h3*(qt(k+1)-qt(k))
       dw2=h2/h1/h3*(w2e(k)-w2e(k-1))+h1/h2/h3*(w2e(k+1)-w2e(k))
       dwt=h2/h1/h3*(wte(k)-wte(k-1))+h1/h2/h3*(wte(k+1)-wte(k))
       dt2=h2/h1/h3*(t2e(k)-t2e(k-1))+h1/h2/h3*(t2e(k+1)-t2e(k))
       if(k.eq.1)then
        dwq=(wq(2)-wq(1))/(z(2)-z(1))
        dqt=(qt(2)-qt(1))/(z(2)-z(1))
        dw2=(w2e(2)-w2e(1))/(z(2)-z(1))
        dwt=(wte(2)-wte(1))/(z(2)-z(1))
        dt2=(t2e(2)-t2e(1))/(z(2)-z(1))
       endif
       if(k.eq.n)then
        dwq=(wq(n)-wq(n-1))/(z(n)-z(n-1))
        dqt=(qt(n)-qt(n-1))/(z(n)-z(n-1))
        dw2=(w2e(n)-w2e(n-1))/(z(n)-z(n-1))
        dwt=(wte(n)-wte(n-1))/(z(n)-z(n-1))
        dt2=(t2e(n)-t2e(n-1))/(z(n)-z(n-1))
       endif
      endif
c --------- triple correlations ----------------------------------------
      det1=1.+1./cq9*(2./cq8+1./cq10)*ta**2*wn
	if(det1.le.0.)det1=1.
      wqt=-ta/det1/cq9*((w2e(k)+2.*bt*g*ta/cq10*wte(k))*dqt+
     . wq(k)*(dwt-ta/cq8*dt(k)*dw2)+(wte(k)-ta/cq8*dt(k)*dw2)*dwq+
     . bt*g*ta/cq10*wq(k)*dt2+
     . (w2t(k)+bt*g*ta/cq10*wt2(k)-ta/cq8*dt(k)*w3(k))*dq(k))
      qt2(k)=-ta/cq10*(wqt*dt(k)+wt2(k)*dq(k)+wq(k)*dt2+2.*wte(k)*dqt)
	w2q(k)=-ta/cq8*(w3(k)*dq(k)+wq(k)*dw2+2.*w2e(k)*dwq-2.*bt*g*wqt) 
c --------------------------------------------------------------------------
      tau=ta/dtt
      det2=1.+(1.-cq3)/(cq1+dsq1*fs(k))/rr*ta**2*wn
	if(det2.le.0.)det2=1.
      ak(k)=tau/c9*w2e(k)
      akt(k)=ta/det2/(cq1+dsq1*fs(k))
     .  *(w2(k)+(1.-cq3)/rr*bt*g*ta*wte(k))
  11  continue
      do 2 ii=1,2
      do 1 j=1,2
      c(1,ii,j)=0.
      b(1,ii,j)=0.
      c(n,ii,j)=0.
      a(n,ii,j)=0.
   1  continue
      a(n,ii,ii)=1.
      c(1,ii,ii)=1.
      b(1,ii,ii)=1.
      c(n,ii,ii)=1.
   2  continue
      do 10 j=1,15
      ta=ei(1)/epi(1)
      wn=bt*g*dt(1)
      c(1,1,1)=-1.
      b(1,1,1)=-1.
      f(1,1)=-h0/akt(1)*(z(2)-z(1))
c      f(1,1)=0.
      b(1,2,2)=0.
      c(1,2,1)=-2.*ta/rr*h0/(z(2)-z(1))
      b(1,2,1)=-2.*ta/rr*h0/(z(2)-z(1))
      f(1,2)=0.
c      f(1,2)=q2c
      f(n,1)=aq(n)-aq(n-1)
	a(n,1,1)=0.
	f(n,1)=aqn
      do 3 k=2,n-1
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      tm=ei(k-1)/epi(k-1)
      tn=ei(k)/epi(k)
      tp=ei(k+1)/epi(k+1)
      wm=bt*g*dt(k-1)
      wn=bt*g*dt(k)
      wp=bt*g*dt(k+1)
      dwq=h2/h1/h3*(wq(k)-wq(k-1))+h1/h2/h3*(wq(k+1)-wq(k))
      dwqm=(wq(k)-wq(k-1))/h1
      dwqp=(wq(k+1)-wq(k))/h2
	dwm=tm/cq9*(2.*w2q(k-1)*dq(k-1)+2.*wq(k-1)*dwqm-bt*g*qt2(k-1))
	dw=tn/cq9*(2.*w2q(k)*dq(k)+2.*wq(k)*dwq-bt*g*qt2(k))
	dwp=tp/cq9*(2.*w2q(k+1)*dq(k+1)+2.*wq(k+1)*dwqp-bt*g*qt2(k+1))
	dwm=tm/cq9*2.*wq(k-1)*dwqm
	dw=tn/cq9*2.*wq(k)*dwq
	dwp=tp/cq9*2.*wq(k+1)*dwqp
      dwq2=h2/h1/h3*(dw-dwm)+h1/h2/h3*(dwp-dw)
      a(k,1,1)=dx3/h3*(h2/h1/h1*(akt(k)+akt(k-1))-
     .    2./h3*((h2/h1)**2-1.)*akt(k))
      b(k,1,1)=dx3/h3*(h1/h2/h2*(akt(k)+akt(k+1))-
     .    2./h3*((h1/h2)**2-1.)*akt(k))
      c(k,1,1)=f3+a(k,1,1)+b(k,1,1)
      f(k,1)=-aq(k)*f2-aqy(k)*f1
      a(k,2,2)=dx3/h3*(h2/h1/h1*(ak(k)+ak(k-1))-
     .    2./h3*((h2/h1)**2-1.)*ak(k))
      b(k,2,2)=dx3/h3*(h1/h2/h2*(ak(k)+ak(k+1))-
     .    2./h3*((h1/h2)**2-1.)*ak(k))
      c(k,2,2)=f3+a(k,2,2)+b(k,2,2)+rr*epi(k)/ei(k)*dx3+
     . 2.*akt(k)*dq(k)**2*dx3
      a(k,2,1)=-4.*akt(k)*dq(k)*dx3/h3*h2/h1
      b(k,2,1)=4.*akt(k)*dq(k)*dx3/h3*h1/h2
      c(k,2,1)=-4.*akt(k)*dq(k)*dx3/h3*(h2/h1-h1/h2)
      f(k,2)=-aq2(k)*f2-aq2y(k)*f1
     .  +cl*dwq2*dx3
   3  continue
      call prgm(n,a,b,c,f,rm)
      pru=0.
      prv=0.
      do 4 k=1,n
      if(aqe(k).ne.0.)then
       upr=abs((aqe(k)-rm(k,1))/aqe(k))
      else
       if(rm(k,1).ne.0.)then
        upr=abs((aqe(k)-rm(k,1))/rm(k,1))
       else
        upr=0.
       endif
      endif
      if(aq2e(k).ne.0.)then
       vpr=abs((aq2e(k)-rm(k,2))/aq2e(k))
      else
       if(rm(k,2).ne.0.)then
        vpr=abs((aq2e(k)-rm(k,2))/rm(k,2))
       else
        vpr=0.
       endif
      endif
      if(upr.ge.pru)pru=upr
      if(vpr.ge.prv)prv=vpr
      aqe(k)=rm(k,1)
      aq2e(k)=rm(k,2)
	if(aqe(k).lt.0.)aqe(k)=0.
   4  continue
      pr=amax1(prv,pru)
c      print 13,j,pru,prv
      jq=j
c     call akorr(lf)
      if(pr.lt.prs)go to 12
  10  continue
  12  continue
  13  format(2x,i3,2x,'prt=',f10.7,2x,'prt2=',f10.7)
  14  format(2x,'---------')
      return
      end

      subroutine prad(ug)
      common /a/ z(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /qq/ aq(151),aqe(151),aq2(151),aq2e(151),aqi(151),aq2i(151),
     .      aqy(151),aq2y(151),wq(151),qt(151),rad(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /frm/ frm1(151),frm2(151),frm3(151),frm4(151),frm5(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      dimension rfr(151,151),Q(151,151),tabs(150),rfrb(151),
     . 	frmm(151),frmp(151),rfg(151),rm(151),rp(151),
     .   Q0(151),rfr0(151),dfltop(151), rxt(151),rxs(151),rx(151)
c ---------Velocity cm/s -----------------------------------------
      uug=ug*100.
c ------- пdensity of airа g/sm^3 ----------------------------
	roa=0.001225
c -------  specific heats of air at constant pressure ----
c--------       cal/g/deg                                            ----
	cp=0.2402
c ------- Stefan^s constant cal/(cm^2 sec deg^4) -------
	stcoff=1.38*0.000000000001
c ----- Coeff 1/deg^3 -----------------------------------------
	stkof=stcoff/cp/roa/uug
c ------ height of PBL (cm)-----------------
      Hm=3000.*100.
c ------- Mean amout of water vapor Q0 g/cm^2 from surface to z1  ----------
	aqe0=aqe(1)-(aqe(2)-aqe(1))*z(1)/(z(2)-z(1))
      Q0(1)=roa*(aqe0+aqe(1))/2.*z(1)*Hm
      do 11 j=1,n-1
c ------- Calculation of absolut temperature ------------------------------------
c -----   T=Tvir/(1+0.61*q(z)) - Hmax*z(k)/100            -------
	tabs(j)=(te(j)+273.)/(1.+0.61*aqe(j))-30.*z(j)
c ------- Mean amout of water vapor Q0 g/cm^2 from surface to z  ----------
c -------                                  and A(0,z)                                        ----------
      Q0(j+1)=Q0(j)+roa*(aqe(j)+aqe(j+1))/2.*(z(j+1)-z(j))*Hm
	rfr0(j)=8.34*tabs(j)**(.353*alog10(Q0(j))-.44)*
     .                  Q0(j)**(-.03455*alog10(Q0(j))-0.705)
  11  continue
      tabs(n)=(te(n)+273.)/(1.+0.61*aqe(n))-30.*z(n)
	tabs0=tabs(1)-(tabs(2)-tabs(1))*z(1)/(z(2)-z(1))
c ------- Calculation of mean amout of water vapor Q(i) g/cm^2 ----------
      do 4 i=1,n
      do 3 j=1,n
      Q(i,j)=0.
      if(i.eq.j)then
       Q(i,j)=0.
      else
      if(i.lt.j)then
       do 1 k=i,j-1
        Q(i,j)=Q(i,j)+roa*(aqe(k)+aqe(k+1))/2.*(z(k+1)-z(k))*Hm
  1    continue
      else
       do 2 k=j,i-1
        Q(i,j)=Q(i,j)+roa*(aqe(k)+aqe(k+1))/2.*(z(k+1)-z(k))*Hm
  2    continue
      endif
      endif
	if(i.eq.j) Q(i,j)=0.
c ------ Calculation total absorptivity of water vapor between i end j ----
      if(i.ne.j)then
	 rfr(i,j)=8.34*tabs(j)**(.353*alog10(Q(i,j))-.44)*
     .                  Q(i,j)**(-.03455*alog10(Q(i,j))-0.705)
	else
	  rfr(i,j)=0.
	endif
  3   continue
c ----------- total absorptivity between Z=2 km end  top of atmos Q=0.3 g/cm^2 --------------
      jtop=144
	Qb=Q(i,n)+0.3
	rfrb(i)=8.34*tabs(i)**(.353*alog10(Qb)-.44)*
     .                  Qb**(-.03455*alog10(Qb)-0.705)
  4   continue
c -------------
c ----------- total absorptivity between surface end  z1 --------------
c	rfr0=8.34*tabs0**(.353*alog10(Q0)-.44)*
c     .                  Q0**(-.03455*alog10(Q0)-0.705)
c ----- Calculation radiative from surface and z0-z1 -------
	 h1=z(1)
	 h2=z(2)-z(1)
	 h3=h1+h2
       dtabs0=-1./h3*(tabs0**4*(2.+h2/h1)-
     .                        tabs(1)**4*h3**2/h2/h1+
     .                        tabs(2)**4*h1/h2)
	 drfr0=-1./h3*(rfr0(1)*(2.+h2/h1)-
     .                        rfr0(1)*h3**2/h2/h1+
     .                        rfr0(2)*h1/h2)
      dtabs1=tabs(2)**4-tabs0**4
	drfr0=(rfr0(2)-rfr0(1))/h2
	frmm0=rfr0(1)*dtabs0*z(1)
	frms=tabs0**4
c --
c ----- Calculation int of A(z,Z)*dB(Z)/dZ for Z is from 0 to z(j) ---------
c --

      frmp(1)=0.
      frmm(1)=frms+frmm0
      do 6 j=2,n-1
	frinm=0.
c	fimtm=rfr0(1)*dtabs0
	frinp=0.
c ----- Calculation of A(z,Zt)*B(Zt) ------------------------
      rfg(j)=rfrb(j)*tabs(n)**4
c ------- Integral model for flux ---------------
      do 5 i=2,n-1
      arfn=(rfr(i,j)+rfr(i-1,j))/2.
      arfp=(rfr(i,j)+rfr(i+1,j))/2.
      dbsn=(tabs(i)**4-tabs(i-1)**4)/(z(i)-z(i-1))
      dbsp=(tabs(i+1)**4-tabs(i)**4)/(z(i+1)-z(i))
	If(i.le.j)then
      	frinm=frinm+(dbsn*arfn+dbsp*arfp)*(z(i+1)-z(i-1))/4.
	else
	    frinp=frinp-(dbsn*arfn+dbsp*arfp)*(z(i+1)-z(i-1))/4.
	endif
  5   continue
       frmm(j)=frinm
	 frmp(j)=frinp
  6   continue
      h1=z(2)-z(1)
	h2=z(3)-z(2)
       frmm(1)=frmm(2)-h1/h2*(frmm(3)-frmm(2))
       frmp(1)=frmp(2)-h1/h2*(frmp(3)-frmp(2))
       frmm(n)=frmm(n-1)
       frmp(n)=frmp(n-1)
      do 66 j=1,n
      frmp(j)=frmp(j)-frmp(n)
	frmm(j)=frmm(j)-frmm(n)+frmp(1)
  66   continue
c ----- Calculation d/dz*Flux from  z(j) to Zn ---------
	do 7 j=2,n-1
      h=z(j+1)-z(j-1)
      rxt(j)=(frmm(j+1)-frmm(j-1))/h
      rxs(j)=(frmp(j+1)-frmp(j-1))/h
	ffp=frmm(j+1)-frmp(j+1)
	ffm=frmm(j-1)-frmp(j-1)
	dfltop(j)=(rfg(j+1)-rfg(j-1))/h
	rx(j)=(ffp-ffm)/h-dfltop(j)
c -------------------------------------------------------
  7   continue
      h1=z(2)-z(1)
	h2=z(3)-z(2)
      rxt(1)=rxt(2)-h1/h2*(rxt(3)-rxt(2))
	rxt(n)=rxt(n-1)
      rxs(1)=rxs(2)-h1/h2*(rxs(3)-rxs(2))
      rxs(n)=rxs(n-1)
      rfg(1)=rfg(2)-h1/h2*(rfg(3)-rfg(2))
      rfg(n)=rfg(n-1)
      dfltop(1)=dfltop(2)-h1/h2*(dfltop(3)-dfltop(2))
      dfltop(n)=dfltop(n-1)
      rx(1)=rx(2)-h1/h2*(rx(3)-rx(2))
      rx(n)=rx(n-1)
      do 76 j=1,n
      rxs(j)=rxs(j)-rxs(n)
	rxt(j)=rxt(j)-rxt(n)+rxs(1)
  76   continue
      do 8 j=1,n
	frm1(j)=frmm(j)*stkof
	frm2(j)=frmp(j)*stkof
	frm3(j)=(frmm(j)-frmp(j))*stkof
	frm4(j)=dfltop(j)*stkof
	frm5(j)=rx(j)*stkof
	rad(j)=frm5(j)
   8  continue
      rad(n)=rad(n-1)
      return
      end

      function t1(x)
c------- Calculation of heat flux in the serface - qtq (mK/s) -------
c-----------           x - time of day (0-24)                 -------
      api=3.14159265
      if(x.lt.17.)then
	t1=12.*exp(-((x-17.)/8.)**4)+3.
	else
      t1=13.5*exp(-((x-17.)/3.5)**1.4)+1.5+16.*exp(-((x-40.)/7.)**4)
	endif
      return
      end


      function qt0(x)
c------- Calculation of heat flux in the serface - qtq (mK/s) -------
c-----------           x - time of day (0-24)                 -------
      api=3.14159265
      x1=x 
      if(x1.gt.24.)x1=x1-24.
      y0=6.8
      y1=10.
      b1=1.+(y0-6.)/5.
      q0=-.01-.01*(1.-cos(api/2.*(y0-4.)))
      q1=.18*sin(api/9.5*(y1-8.))
      ask=1.+10.*(x1-17.5)/2.
      if(x1.le.4.)qtq=-.01
      if(x1.gt.6.)b=1.+(x1-6.)/5.
      if(x1.gt.4..and.x1.le.y0)qtq=-.01-.01*(1.-cos(api/2.*(x1-4.)))
      if(x1.gt.y0.and.x1.le.y1)qtq=(q1-q0)*(y0-x1)/(y0-y1)+q0
      if(x1.ge.y1.and.x1.le.17.5)qtq=.18*sin(api/9.5*(x1-8.))
      if(x1.gt.17.5.and.x1.le.20.)then
	  qtq=.18/ask*sin(api/9.5*(x1-8.))
	  if(qtq.lt.-.01)qtq=-.01
      endif
      if(x1.gt.20.)qtq=-.01
	if(x1.gt.y0.and.x1.lt.y1)then
       qtq=(q1-q0)*(y0-x1)/(y0-y1)+q0
      endif
      qt0=qtq
      return
      end





c  ------- subroutine to solve matrix equation -----
c  -------  for Reynold's stress and heat flux -----
c  -------   (Enger - 1984 - Atmospheric Env.) -----
      subroutine akorr
      common /a/ z(151)
     .  /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .         vw(151),ut(151),vt(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /qq/ aq(151),aqe(151),aq2(151),aq2e(151),aqi(151),aq2i(151),
     .      aqy(151),aq2y(151),wq(151),qt(151),rad(151)
     . /frm/ frm1(151),frm2(151),frm3(151),frm4(151),frm5(151)
     .  /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     .  /const1/ sie,ce1,ce2,cq1,cq3,rr,dsq1
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
	 double precision wr
      do 1 j=1,n
      if(j.ne.1.and.j.ne.n)then
       h1=z(j)-z(j-1)
       h2=z(j+1)-z(j)
       h3=z(j+1)-z(j-1)
       du(j)=h2/h1/h3*(ue(j)-ue(j-1))+h1/h2/h3*(ue(j+1)-ue(j))
       dv(j)=h2/h1/h3*(ve(j)-ve(j-1))+h1/h2/h3*(ve(j+1)-ve(j))
       dt(j)=h2/h1/h3*(te(j)-te(j-1))+h1/h2/h3*(te(j+1)-te(j))
       dti=h2/h1/h3*(t(j)-t(j-1))+h1/h2/h3*(t(j+1)-t(j))
       dq(j)=h2/h1/h3*(aqe(j)-aqe(j-1))+h1/h2/h3*(aqe(j+1)-aqe(j))
      else
       if(j.eq.1)then
        h1=z(2)-z(1)
        du(1)=(ue(2)-ue(1))/h1
        dv(1)=(ve(2)-ve(1))/h1
        dt(1)=(te(2)-te(1))/h1
        dti=(t(2)-t(1))/h1
        dq(1)=(aqe(2)-aqe(1))/h1
       else
        h22=z(n)-z(n-1)
        du(n)=(ue(n)-ue(n-1))/h22
        dv(n)=(ve(n)-ve(n-1))/h22
        dt(n)=(te(n)-te(n-1))/h22
        dti=(t(n)-t(n-1))/h22
	  dq(n)=(aqe(n)-aqe(n-1))/h22
       endif
      endif
      ta=e(j)/ep(j)
      wn=bt*g*dt(j)
      det1=1.+(1.-c3)/(c1+1.5*ds1*fs(j))/ct1*ta**2*wn
      det2=1.+(1.-cq3)/(cq1+dsq1*fs(j))/rr*ta**2*wn
c	if(det1.le.0.)det1=1.
c	if(det2.le.0.)det2=1.
	wr=ta*w2(j)*dt(j)
c	wr=2.*ta*w2(j)*dt(j)-tai*w2(j)*dti
c	wr=ta*w2e(j)*dt(j)
c	if(abs(wr).le..0001)wr=0.
      wte(j)=-wr/(ct1+dst1*fs(j))+
     . ta*(1.-ct3)*bt*g*t2e(j)/(ct1+dst1*fs(j))
      aku=(1.-c2*(1.-1.5*ds2*fs(j)))/det1/(c1+1.5*ds1*fs(j))*ta*
     . (w2i(j)+(1.-c3)*(1.-ct2)/(1.-c2*(1.-1.5*ds2*fs(j)))
     . /ct1*bt*g*ta*wte(j))
      akt=(1.-ct2)/det1/ct1*ta*
     . (wte(j)-(1.-c2*(1.-1.5*ds2*fs(j)))/
     . (1.-ct2)/(c1+1.5*ds1*fs(j))*ta*w2i(j)*dt(j))
      uw(j)=-aku*du(j)
      vw(j)=-aku*dv(j)
      ut(j)=-akt*du(j)
      vt(j)=-akt*dv(j)
      uv(j)=-ta/c1*(1.-c2)*(uw(j)*dv(j)+vw(j)*du(j))
      u2(j)=eh(j)-(1.-c2)/c1*ta*(uw(j)*du(j)-vw(j)*dv(j))
      v2(j)=eh(j)-(1.-c2)/c1*ta*(vw(j)*dv(j)-uw(j)*du(j))
      akq=ta/det2/(cq1+dsq1*fs(j))
     .  *(w2(j)+(1.-cq3)/rr*bt*g*ta*wte(j))
	wq(j)=-akq*dq(j)
	qt(j)=-ta/rr*(wq(j)*dt(j)+wte(j)*dq(j))
c	frm5(j)=ta/det1
  1   continue
c       wte(1)=wte(2)-(wte(3)-wte(2))/(z(3)-z(2))*(z(2)-z(1))
c     call filtr(z,dt,n)
	return
      end

	subroutine filtr(x,r,n)
	dimension x(n),r(n), f(n)
	do 1 k=2,n-1
   1    f(k)=(r(k-1)+r(k)+r(k+1))/3.
      h1=x(2)-x(1)
	h2=x(3)-x(2)
	h3=h1+h2
	f(1)=f(3)*(1.-h3/h2)+h3/h2*f(2)
	h1=x(n)-x(n-1)
	h2=x(n-1)-x(n-2)
	h3=h1+h2
	f(n)=f(n-2)*(1.-h3/h2)+h3/h2*f(n-1)
      do 2 k=1,n
   2 	r(k)=f(k)
	return
	end


      subroutine p6(dx1,dx2,tq,t2c,irad,prs,tc,cl,ug,jt)
      common /a/ z(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     . /qq/ aq(151),aqe(151),aq2(151),aq2e(151),aqi(151),aq2i(151),
     .      aqy(151),aq2y(151),wq(151),qt(151),rad(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .        vw(151),ut(151),vt(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /kor3/ w3(151),w2t(151),wt2(151),t3(151),u2w(151),v2w(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      dimension a(151,2,2),b(151,2,2),c(151,2,2),f(151,2),rm(151,2)
     . ,tau2(151),ak(151)
      akt(j)=tau2(j)*w2(j)
      if(irad.eq.1)call prad(ug)
      dx3=dx1+dx2
      f1=dx2/dx1
      f2=-dx3**2/dx1/dx2
      f3=2.+dx1/dx2
      do 11 k=1,n
c      te(k)=ti(k)
c      t2e(k)=t2i(k)
      ta=ei(k)/epi(k)
      wn=bt*g*dt(k)
      if(wn.ge.0.)then
       dtt=1.+dst3*ta**2*wn
      else
       dtt=1.
      endif
      tau=ta/dtt
	det=1.+4.*(1.-2./3.*c11)/c9/c8*tau**2*wn
	det=1.+3.*(1.-2./3.*c11)/c9/c10*tau**2*wn
	if(det.le.0.)det=1.
      tau2(k)=ta/(ct1+dst1*fs(k))
      ak(k)=tau/det/c9*w2e(k)
  11  continue
      do 2 ii=1,2
      do 1 j=1,2
      c(1,ii,j)=0.
      b(1,ii,j)=0.
      c(n,ii,j)=0.
      a(n,ii,j)=0.
   1  continue
      a(n,ii,ii)=1.
      c(1,ii,ii)=1.
      b(1,ii,ii)=1.
      c(n,ii,ii)=1.
   2  continue
      do 10 j=1,15
      gp=bt*g*wte(1)
      p=-uw(1)*du(1)-vw(1)*dv(1)
      ta=ei(1)/epi(1)
	c211=w2e(1)/(bt*g)/(1.-ct3)/(z(2)-z(1))
	f21=tq/(bt*g*ta)*(ct1-dst1*fs(1))/(1.-ct3)
      c(1,1,1)=-1.
      b(1,1,1)=-1.
      c(1,1,2)=-(1.-ct3)*bt*g*(z(2)-z(1))/w2e(1)
      f(1,1)=(-(ct1+dst1*fs(1))*tq/ta)/w2e(1)*(z(2)-z(1))
      b(1,2,2)=0.
c      f(1,2)=2.*tq*(ct1+dst1*fs(1))*tq/r/w2e(1)
c     . /(1.+2./r*(1.-ct3)*bt*g*ta*tq/w2e(1)+.1*aqe(1)*ta/r)
c      f(1,2)=t2c
c	c(1,1,1)=1.
c	c(1,1,2)=0.
c	b(1,1,1)=0.
c	b(1,1,2)=0.
c	f(1,1)=tc
c	c(1,2,2)=1.
c	c(1,2,1)=c211
c      c(1,2,1)=0.
c	b(1,2,1)=c211
c	b(1,2,2)=0.
	f(1,2)=t2c
      f(n,1)=t(n)-t(n-1)
      do 3 k=2,n-1
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      tm=ei(k-1)/epi(k-1)
      tn=ei(k)/epi(k)
      tp=ei(k+1)/epi(k+1)
      wm=bt*g*dt(k-1)
      wn=bt*g*dt(k)
      wp=bt*g*dt(k+1)
      dwt=h2/h1/h3*(wte(k)-wte(k-1))+h1/h2/h3*(wte(k+1)-wte(k))
      dwtm=(wte(k)-wte(k-1))/h1
      dwtp=(wte(k+1)-wte(k))/h2
	dwm=tm/c9*(2.*w2t(k-1)*dt(k-1)+2.*wt(k-1)*dwtm-bt*g*t3(k-1))
	dw=tn/c9*(2.*w2t(k)*dt(k)+2.*wt(k)*dwt-bt*g*t3(k))
	dwp=tp/c9*(2.*w2t(k+1)*dt(k+1)+2.*wt(k+1)*dwtp-bt*g*t3(k+1))
	detm=1.+4.*(1.-2./3.*c11)/c9/c8*tm**2*wm
	detm=1.+3.*(1.-2./3.*c11)/c9/c10*tm**2*wm
c	if(detm.le.0.)detm=1.
	detn=1.+4.*(1.-2./3.*c11)/c9/c8*tn**2*wn
	detn=1.+3.*(1.-2./3.*c11)/c9/c10*tn**2*wn
c	if(detn.le.0.)detn=1.
	detp=1.+4.*(1.-2./3.*c11)/c9/c8*tp**2*wp
	detp=1.+3.*(1.-2./3.*c11)/c9/c10*tp**2*wp
c	if(detp.le.0.)detp=1.
	dwm=tm/detm/c9*2.*w2t(k-1)*dt(k-1)
	dw=tn/detn/c9*2.*w2t(k)*dt(k)
	dwp=tp/detp/c9*2.*w2t(k+1)*dt(k+1)
      dwt2=h2/h1/h3*(dw-dwm)+h1/h2/h3*(dwp-dw)
	a(k,1,1)=dx3/h3*(h2/h1/h1*(akt(k)+akt(k-1))-
     .    2./h3*((h2/h1)**2-1.)*akt(k))
      a(k,1,2)=dx3/h3*h2/h1*tau2(k-1)*g*bt*(1.-ct3)
      a(k,2,1)=-4.*tau2(k)*w2(k)*dt(k)*dx3/h3*h2/h1
      a(k,2,2)=dx3/h3*(h2/h1/h1*(ak(k)+ak(k-1))-
     .    2./h3*((h2/h1)**2-1.)*ak(k))
      b(k,1,1)=dx3/h3*(h1/h2/h2*(akt(k)+akt(k+1))-
     .    2./h3*((h1/h2)**2-1.)*akt(k))
      b(k,1,2)=-dx3/h3*h1/h2*tau2(k+1)*bt*g*(1.-ct3)
      b(k,2,1)=4.*tau2(k)*w2(k)*dt(k)*dx3/h3*h1/h2
      b(k,2,2)=dx3/h3*(h1/h2/h2*(ak(k)+ak(k+1))-
     .    2./h3*((h1/h2)**2-1.)*ak(k))
      c(k,1,1)=f3+a(k,1,1)+b(k,1,1)
      c(k,1,2)=dx3/h3*(h2/h1-h1/h2)*tau2(k)*bt*g*(1.-ct3)
      c(k,2,1)=-4.*tau2(k)*w2(k)*dt(k)*dx3/h3*(h2/h1-h1/h2)
      c(k,2,2)=f3+a(k,2,2)+b(k,2,2)
     . +r*epi(k)/ei(k)*dx3+2.*tau2(k)*(1.-ct3)*bt*g*dt(k)*dx3
     . +irad*.1*aqe(k)*dx3+cl*dwt2*dx3
      f(k,1)=-t(k)*f2-ty(k)*f1-dx3*irad*rad(k)
      ff=2.*tau2(k)*w2(k)*dt(k)**2*dx3
      f(k,2)=-t2(k)*f2-t2y(k)*f1-ff
   3  continue
      call prgm(n,a,b,c,f,rm)
      pru=0.
      prv=0.
      do 4 k=1,n
      if(te(k).ne.0.)then
       upr=abs((te(k)-rm(k,1))/te(k))
      else
       if(rm(k,1).ne.0.)then
        upr=abs((te(k)-rm(k,1))/rm(k,1))
       else
        upr=0.
       endif
      endif
      if(t2e(k).ne.0.)then
       vpr=abs((t2e(k)-rm(k,2))/t2e(k))
      else
       if(rm(k,2).ne.0.)then
        vpr=abs((t2e(k)-rm(k,2))/rm(k,2))
       else
        vpr=0.
       endif
      endif
      if(upr.ge.pru)pru=upr
      if(vpr.ge.prv)prv=vpr
      te(k)=rm(k,1)
      t2e(k)=rm(k,2)
	if(te(k).lt.0.)te(k)=0.
   4  continue
      pr=amax1(prv,pru)
c      print 13,j,pru,prv
      jt=j
c     call akorr(lf)
      if(pr.lt.prs)go to 12
  10  continue
  12  continue
  13  format(2x,i3,2x,'prt=',f10.7,2x,'prt2=',f10.7)
  14  format(2x,'---------')
      return
      end

      subroutine p7(dx1,dx2,ney,prs,am,ded,ju)
      common /a/ z(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .        vw(151),ut(151),vt(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
      dimension a(151,2,2),b(151,2,2),c(151,2,2),f(151,2),rm(151,2)
      tau(j)=ei(j)/epi(j)/(c1+1.5*ds1*fs(j))
      ak(j)=(1.-c2*(1.-1.5*ds2*fs(j)))*tau(j)*w2e(j)
      ta(j)=(1.-c3)*tau(j)
      dx3=dx1+dx2
       f1=dx2/dx1
      f2=-dx3**2/dx1/dx2
      f3=2.+dx1/dx2
c      do 11 k=1,n
c      ue(k)=ui(k)
c      ve(k)=vi(k)
c  11  continue
      do 2 ii=1,2
      do 1 j=1,2
      c(1,ii,j)=0.
      b(1,ii,j)=0.
      c(n,ii,j)=0.
      a(n,ii,j)=0.
   1  continue
      if(ney.eq.0)then
       a(n,1,1)=0.
       a(n,2,2)=0.
      else
       a(n,1,1)=1.
       a(n,2,2)=1.
      endif
      c(1,ii,ii)=1.
      b(1,ii,ii)=ded
      c(n,ii,ii)=1.
      f(1,ii)=0.
   2  continue
      f(n,1)=0.
      f(n,2)=0.
      if(a(n,1,1).eq.0.)f(n,1)=1.
      if(a(n,2,2).eq.0.)f(n,2)=0.
      do 10 j=1,10
      h1=z(2)-z(1)
      du(1)=(u(2)-u(1))/h1
      dv(1)=(v(2)-v(1))/h1
      h2=z(n)-z(n-1)
      du(n)=(u(n)-u(n-1))/h2
      dv(n)=(v(n)-v(n-1))/h2
      do 5 k=2,n-1
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      du(k)=(h2/h1*(ue(k)-ue(k-1))+h1/h2*(ue(k+1)-ue(k)))/h3
      dv(k)=(h2/h1*(ve(k)-ve(k-1))+h1/h2*(ve(k+1)-ve(k)))/h3
   5  continue
      do 3 k=2,n-1
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      tp=ta(k+1)*bt*g
      ts=ta(k)*bt*g
      tm=ta(k-1)*bt*g
      a(k,1,1)=dx3/h3*(h2/h1/h1*(ak(k)+ak(k-1))-
     .    2./h3*((h2/h1)**2-1.)*ak(k))
      b(k,1,1)=dx3/h3*(h1/h2/h2*(ak(k)+ak(k+1))-
     .    2./h3*((h1/h2)**2-1.)*ak(k))
      c(k,1,1)=f3+a(k,1,1)+b(k,1,1)
      ff=-dx3/h3*(h1/h2*ut(k+1)*tp+(h2/h1-h1/h2)*ut(k)*ts
     . -h2/h1*ut(k-1)*tm)
      f(k,1)=-u(k)*f2-uy(k)*f1+ff
      ff=-dx3/h3*(h1/h2*vt(k+1)*tp+(h2/h1-h1/h2)*vt(k)*ts
     . -h2/h1*vt(k-1)*tm)+am*dx3
      f(k,2)=-v(k)*f2-vy(k)*f1+ff
      a(k,2,2)=a(k,1,1)
      a(k,1,2)=0.
      a(k,2,1)=0.
      c(k,2,2)=c(k,1,1)
      c(k,1,2)=-am*dx3
      c(k,2,1)=am*dx3
      b(k,2,2)=b(k,1,1)
      b(k,1,2)=0.
      b(k,2,1)=0.
   3  continue
      call prgm(n,a,b,c,f,rm)
      pru=0.
      prv=0.
      do 4 k=1,n
      if(ue(k).ne.0.)then
       upr=abs((ue(k)-rm(k,1))/ue(k))
      else
       if(rm(k,1).ne.0.)then
        upr=abs((ue(k)-rm(k,1))/rm(k,1))
       else
        upr=0.
       endif
      endif
      if(ve(k).ne.0.)then
       vpr=abs((ve(k)-rm(k,2))/ve(k))
      else
       if(rm(k,2).ne.0.)then
        vpr=abs((ve(k)-rm(k,2))/rm(k,2))
       else
        vpr=0.
       endif
      endif
      if(upr.ge.pru)pru=upr
      if(vpr.ge.prv)prv=vpr
      ue(k)=rm(k,1)
      ve(k)=rm(k,2)
   4  continue
      pr=amax1(prv,pru)
c     call akorr(lf)
c      print 13,j,pru,prv
      ju=j
      if(pr.lt.prs)go to 12
  10  continue
  12  continue
  13  format(2x,i3,2x,'pru=',f10.7,2x,'prv=',f10.7)
  14  format(2x,5f14.9)
      return
      end



      subroutine p8(dx1,dx2,qtq,prs,ec,epc,wc,cw,cl,je)
      common /a/ z(151)
     . /tot/ u(151),v(151),e(151),ep(151),t(151),t2(151),wt(151)
     . /tit/ ui(151),vi(151),ei(151),epi(151),ti(151),t2i(151),wti(151)
     . /res/ ue(151),ve(151),ee(151),epe(151),te(151),t2e(151),wte(151)
     . /tmp/ uy(151),vy(151),ey(151),epy(151),ty(151),t2y(151),wty(151)
     .  /dcom/ du(151),dv(151),dt(151),dq(151)
     . /akor/ u2(151),v2(151),w2(151),uv(151),uw(151),
     .        vw(151),ut(151),vt(151)
     . /const0/ n,cap,cm,ced,ct,cs,cs1,cs2,cs7,r,al,bt,g
     . /const2/ c1,c2,c3,ct1,ct2,ct3,c7,c8,c9,c10,c11
     . /constw/ ds1,ds2,ds3,dst1,dst2,dst3,cep,fs(151)
     . /wwe/ w2e(151),w2y(151),w2i(151)
     .,ehe(151),eh(151),ehi(151),ehy(151)
     . /kor3/ w3(151),w2t(151),wt2(151),t3(151),u2w(151),v2w(151)
     . /frm/ frm1(151),frm2(151),frm3(151),frm4(151),frm5(151)
      dimension a(151,3,3),b(151,3,3),c(151,3,3),f(151,3),rm(151,3),
     . ak(151),akw(151),ake(151)
      dx3=dx1+dx2
      f1=dx2/dx1
      f2=-dx3**2/dx1/dx2
      f3=2.+dx1/dx2
c      do 11 k=1,n
c      w2e(k)=w2i(k)
c      ehe(k)=ehi(k)
c      epe(k)=epi(k)
c  11  continue
      do 10 j=1,10
      do 2 ii=1,3
      do 1 jj=1,3
      c(1,ii,jj)=0.
      b(1,ii,jj)=0.
      c(n,ii,jj)=0.
      a(n,ii,jj)=0.
      f(jj,ii)=0.
   1  continue
      a(n,ii,ii)=1.
      c(1,ii,ii)=1.
      b(1,ii,ii)=1.
      c(n,ii,ii)=1.
      f(n,ii)=0.
   2  continue
      b(1,1,1)=0.
      b(1,2,2)=0.
c      b(1,3,3)=0.
      f(1,1)=ec-w2e(1)/2.
      f(1,1)=ec-wc/2.
c	f(1,1)=0.
      f(1,2)=epc
c	f(1,2)=0.
      f(1,3)=wc
      f(1,3)=0.
      do 5 k=1,n
      if(k.ne.1.and.k.ne.n)then
       h1=z(k)-z(k-1)
       h2=z(k+1)-z(k)
       h3=z(k+1)-z(k-1)
       du2=h2/h1/h3*(u2(k)-u2(k-1))+h1/h2/h3*(u2(k+1)-u2(k))
       dv2=h2/h1/h3*(v2(k)-v2(k-1))+h1/h2/h3*(v2(k+1)-v2(k))
       dw2=h2/h1/h3*(w2i(k)-w2i(k-1))+h1/h2/h3*(w2i(k+1)-w2i(k))
       dwt=h2/h1/h3*(wte(k)-wte(k-1))+h1/h2/h3*(wte(k+1)-wte(k))
       dt2=h2/h1/h3*(t2e(k)-t2e(k-1))+h1/h2/h3*(t2e(k+1)-t2e(k))
       dut=h2/h1/h3*(ut(k)-ut(k-1))+h1/h2/h3*(ut(k+1)-ut(k))
       dvt=h2/h1/h3*(vt(k)-vt(k-1))+h1/h2/h3*(vt(k+1)-vt(k))
       duw=h2/h1/h3*(uw(k)-uw(k-1))+h1/h2/h3*(uw(k+1)-uw(k))
       dvw=h2/h1/h3*(vw(k)-vw(k-1))+h1/h2/h3*(vw(k+1)-vw(k))
       if(k.eq.1)then
	  h11=z(2)-z(1)
        du2=(u2(2)-u2(1))/h11
        dv2=(v2(2)-v2(1))/h11
        dw2=(w2i(2)-w2i(1))/h11
        dwt=(wte(2)-wte(1))/h11
        dt2=(t2e(2)-t2e(1))/h11
        dut=(ut(2)-ut(1))/h11
        dvt=(vt(2)-vt(1))/h11
        duw=(uw(2)-uw(1))/h11
        dvw=(vw(2)-vw(1))/h11
       endif
       if(k.eq.n)then
	   h22=z(n)-z(n-1)
        du2=(u2(n)-u2(n-1))/h22
        dv2=(v2(n)-v2(n-1))/h22
        dw2=(w2i(n)-w2i(n-1))/h22
        dwt=(wte(n)-wte(n-1))/h22
        dt2=(t2e(n)-t2e(n-1))/h22
        dut=(ut(n)-ut(n-1))/h22
        dvt=(vt(n)-vt(n-1))/h22
        duw=(uw(n)-uw(n-1))/h22
        dvw=(vw(n)-vw(n-1))/h22
       endif
      endif
      ta=ei(k)/epi(k)
      wn=bt*g*dt(k)
      if(wn.ge.0.)then
       dtt=1.+dst3*ta**2*wn
      else
       dtt=1.
      endif
      tau=ta/dtt
c      det=1.+4.*(1.-2./3.*c11)/c9/c8*tau**2*wn
c      adet=1.+4./8./8.*tau**2*wn
c      tae=tau/(1.+1./c7/c8*tau**2*wn)
c ---------- triple correlations --------------------
      det1=1.+3./c9/c10*ta**2*wn
      det2=1.+4./c8/c9/det1*ta**2*wn
	det3=1.+3./c7/c8/det2*ta**2*wn
c      if(det1.le.0..or.det2.le.0..or.det3.le.0.)then
c      det1=1.
c      det2=1.
c	det3=1.
c	endif
	w3(k)=-3.*ta/det3/c7*((w2e(k)+cw*bt*g*ta/det2/c8*wte(k))*dw2+
     . bt*g*ta/det2/c8*(2.*cw*(w2e(k)+2.*bt*g*ta/det1/c9*wte(k))*dwt+
     . 2.*cl*bt*g*ta/det1/c9*(w2e(k)+3.*cw*bt*g*ta/c10*wte(k))*dt2))
	w2t(k)=-ta/det2/c8*(cw*w3(k)*dt(k)+cw*wte(k)*dw2+
     . 2.*cw*(w2e(k)+2.*bt*g*ta/det1/c9*wte(k))*dwt+
     . 2.*bt*g*ta/det1/c9*(w2e(k)+3.*cw*bt*g*ta/c10*wte(k))*dt2)
	wt2(k)=-ta/det1/c9*(2*cl*w2t(k)*dt(k)+
     . (w2e(k)+3.*cw*bt*g*ta/c10*wte(k))*dt2+2.*cw*wte(k)*dwt)
c ----------------------------------------------------------
	w3(k)=-3.*ta/det3/c7*(w2e(k)*dw2+
     .            cl*2.*(bt*g*ta)**2/det2/c8/det1/c9*w2e(k)*dt2)
	w2t(k)=-ta/det2/c8*(2.*bt*g*ta/det1/c9*w2e(k)*dt2)*cl
     .           - (cl-1.)*2.*ta/det2/c8*w2e(k)*dwt
	wt2(k)=-ta/det1/c9*(cl*2*w2t(k)*dt(k)+w2e(k)*dt2)
	 t3(k)=-3.*ta/c10*(wt2(k)*dt(k)+wte(k)*dt2)
c ----------------------------------------------------
c      det1=1.+3.*ta**2*wn*(c9*c10+3.*ta**2*wn)/
c     . c7/c8/c9/c10/(1.+(3.*c8+4.*c9)*ta**2*wn/c8/c9/c9)
c	 det2=det1*(1.+(3.*c8+4.*c9)*ta**2*wn/c8/c9/c9)
c	wt2(k)=-tau/det/c9*w2e(k)*dt2
c      w2t(k)=2.*tau/c8*(1.-2./3.*c11)*bt*g*wt2(k)
c      w3(k)=-3.*tau/c7*(w2e(k)*dw2-(1.-c11)*bt*g*w2t(k))
c      frm1(k)=ta/det1
c      frm2(k)=ta/det2
c      frm3(k)=ta/det3
c      frm4(k)=wn
c -----------------------------------------------------
      akw(k)=3.*ta/det3/c7*w2i(k)
c      akw(k)=3.*ta/c7*w2e(k)
c --------------------------------------------
c      ake(k)=ced*ta*(w2i(k)+bt*g*ta*wte(k)/c8)
      ake(k)=ced*ta*w2i(k)
c --------------------------------------------
c      ak(k)=cs*ta*(w2i(k)+bt*g*ta*wte(k)/c8)
      ak(k)=cs*ta*w2i(k)
      u2w(k)=-ak(k)*du2
      v2w(k)=-ak(k)*dv2
  5   continue
       do 3 k=2,n-1
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      b11=u2(k)/2./ee(k)-1./3.
      b22=v2(k)/2./ee(k)-1./3.
      b33=w2e(k)/2./ee(k)-1./3.
      b12=uv(k)/2./ee(k)
      b13=uw(k)/2./ee(k)
      b23=vw(k)/2./ee(k)
      beh=ehi(k)/2./ee(k)-1./3
      ain11=(b11**2+b22**2+b33**2)/2.+b12**2+b13**2+b23**2
      ain11=(beh**2+b33**2)/2.+b12**2+b13**2+b23**2
      cs0=3.8-8.*ain11
      dd=-uw(k)*du(k)-vw(k)*dv(k)
      dg=bt*g*wte(k)
      tm=ee(k-1)/epe(k-1)
      tn=ee(k)/epe(k)
      tp=ee(k+1)/epe(k+1)
      wm=bt*g*dt(k-1)
      wn=bt*g*dt(k)
      wp=bt*g*dt(k+1)
c      if(wm.ge.0.)then
       detm=1.+dst3*tm**2*wm
       det=1.+dst3*tn**2*wn
       detp=1.+dst3*tp**2*wp
c      else
c       detm=1.
c	 det=1.
c	 detp=1.
c      endif
      dw2t=h2/h1/h3*(w2t(k)*tn/det-w2t(k-1)*tm/detm)+
     .         h1/h2/h3*(w2t(k+1)*tp/detp-w2t(k)*tn/det)
      diw=3.*(1.-c11)/c7*bt*g*dw2t
c ------------------------------------------------------------------------
      if(k.ne.2)then
       h1=z(k-1)-z(k-2)
       h2=z(k)-z(k-1)
       h3=h1+h2
       dt2m=h2/h1/h3*(t2i(k-1)-t2i(k-2))+h1/h2/h3*(t2i(k)-t2i(k-1))
      else
	 h11=z(2)-z(1)
       dt2m=(t2i(2)-t2i(1))/h11
 	endif
      if(k.ne.n-1)then
       h1=z(k+1)-z(k)
       h2=z(k+2)-z(k+1)
       h3=h1+h2
       dt2p=h2/h1/h3*(t2i(k+1)-t2i(k))+h1/h2/h3*(t2i(k+2)-t2i(k+1))
	else
	 h22=z(n)-z(n-1)
       dt2p=(t2i(n)-t2i(n-1))/h22
	endif      
      h1=z(k)-z(k-1)
      h2=z(k+1)-z(k)
      h3=z(k+1)-z(k-1)
      dt2n=h2/h1/h3*(t2i(k)-t2i(k-1))+h1/h2/h3*(t2i(k+1)-t2i(k))
      det1m=1.+3./c9/c10*tm**2*wm
      det1n=1.+3./c9/c10*tn**2*wn
      det1p=1.+3./c9/c10*tp**2*wp
      det2m=1.+4./c8/c9/det1m*tm**2*wm
      det2n=1.+4./c8/c9/det1n*tn**2*wn
      det2p=1.+4./c8/c9/det1p*tp**2*wp
	det3m=1.+3./c7/c8/det2m*tm**2*wm
	det3n=1.+3./c7/c8/det2n*tn**2*wn
	det3p=1.+3./c7/c8/det2p*tp**2*wp
c      if(qtq.lt.0.)then
c      if(det1m.le.0..or.det1n.le.0..or.det1p.le.0.)then
c      det1m=1.
c      det1n=1.
c      det1p=1.
c	endif
c      if(det2m.le.0..or.det2n.le.0..or.det2p.le.0.)then
c      det2m=1.
c      det2n=1.
c      det2p=1.
c	endif
c      if(det3m.le.0..or.det3n.le.0..or.det3p.le.0.)then
c 	det3m=1.
c	det3n=1.
c 	det3p=1.
c	endif
	dvm=6.*cl*(bt*g*tm)**2*tm*dt2m/(c7*c8*c9*det1m*det2m*det3m)
	dvn=6.*cl*(bt*g*tn)**2*tn*dt2n/(c7*c8*c9*det1n*det2n*det3n)
	dvp=6.*cl*(bt*g*tp)**2*tp*dt2p/(c7*c8*c9*det1p*det2p*det3p)
      a(k,1,1)=dx3/h3*(h2/h1/h1*(ak(k)+ak(k-1))-
     .    2./h3*((h2/h1)**2-1.)*ak(k))
      b(k,1,1)=dx3/h3*(h1/h2/h2*(ak(k)+ak(k+1))-
     .    2./h3*((h1/h2)**2-1.)*ak(k))
      c(k,1,1)=f3+a(k,1,1)+b(k,1,1)+c1/tn*dx3
	a33=dx3/h3*(h2/h1/h1*(akw(k)+akw(k-1))-
     .    2./h3*((h2/h1)**2-1.)*akw(k))
	b33=dx3/h3*(h1/h2/h2*(akw(k)+akw(k+1))-
     .    2./h3*((h1/h2)**2-1.)*akw(k))
      a(k,3,3)=a33-dx3/h3*h2/h1*dvm
      b(k,3,3)=b33+dx3/h3*h1/h2*dvp
      c(k,3,3)=f3+a33+b33-dx3/h3*(h2/h1-h1/h2)*dvn
     . +(c1+2.*ds1*fs(k))/tn*dx3
      c(k,1,3)=-ds1*fs(k)/tn*dx3
      a(k,2,2)=dx3/h3*(h2/h1/h1*(ake(k)+ake(k-1))-
     .    2./h3*((h2/h1)**2-1.)*ake(k))
      b(k,2,2)=dx3/h3*(h1/h2/h2*(ake(k)+ake(k+1))-
     .    2./h3*((h1/h2)**2-1.)*ake(k))
      c(k,2,2)=f3+a(k,2,2)+b(k,2,2)+2.*cs0/2.*epi(k)/ei(k)*dx3
     .  -cs1/2./ei(k)*dd*dx3+cs2/2/ei(k)*dg*dx3
      c(k,1,2)=-2./3.*(c1-1.)*dx3
c      c(k,3,2)=-2./3.*(c1-1.)*dx3
      c(k,3,2)=-2./3.*(c1+2.*ds1*fs(k)-1.)*dx3
      fw=(1.-c2*(1.-2./3.*(1.+ds2*fs(k))))*dd+2./3.*c3*dg
      f(k,1)=-eh(k)*f2-ehy(k)*f1+fw*dx3
      fw=-cs7*bt*g*(ut(k)*du(k)+vt(k)*dv(k))+cs0/2.*epi(k)**2/ei(k)
      f(k,2)=-ep(k)*f2-epy(k)*f1+fw*dx3
      fw=2./3.*c2*(1.-2.*ds1*fs(k))*dd+2.*(1.-2./3.*c3)*dg
c     . 	-diw
      f(k,3)=-w2(k)*f2-w2y(k)*f1+fw*dx3
   3  continue
      call pr3(n,a,b,c,f,rm)
      pru=0.
      prv=0.
      prw=0.
      do 4 k=1,n
      if(ehe(k).ne.0.)then
       upr=abs((ehe(k)-rm(k,1))/ehe(k))
      else
       if(rm(k,1).ne.0.)then
        upr=abs((ehe(k)-rm(k,1))/rm(k,1))
       else
        upr=0.
       endif
      endif
      if(w2e(k).ne.0.)then
       wpr=abs((w2e(k)-rm(k,3))/w2e(k))
      else
       if(rm(k,3).ne.0.)then
        wpr=abs((w2e(k)-rm(k,3))/rm(k,3))
       else
        wpr=0.
       endif
      endif
      if(epe(k).ne.0.)then
       vpr=abs((epe(k)-rm(k,2))/epe(k))
      else
       if(rm(k,2).ne.0.)then
        vpr=abs((epe(k)-rm(k,2))/rm(k,2))
       else
        vpr=0.
       endif
      endif
      if(upr.ge.pru)pru=upr
      if(vpr.ge.prv)prv=vpr
      if(wpr.ge.prw)prw=wpr
      ehe(k)=rm(k,1)
      w2e(k)=rm(k,3)
      epe(k)=rm(k,2)
      ee(k)=eh(k)+w2e(k)/2.
   4  continue
      pr=amax1(prv,pru,prw)
c      print 13,j,pru,prv,prw
      je=j
c      call akorr
      if(pr.lt.prs)go to 12
  10  continue
  12  continue
  13  format(2x,i3,2x,'pre=',f10.7,2x,'prep=',f10.7,2x,'prep=',f10.7)
  14  format(2x,5f14.9)
  15  format(2x,i3,f14.9)
  77  format('-------------')
      return
      end


      subroutine prg(n,a,b,c,f,y)
      dimension al(152),bl(152),a(151),b(151),c(151),f(151),y(151)
      al(1)=b(1)/c(1)
      bl(1)=f(1)/c(1)
      do 1 k=1,n-1
      al(k+1)=b(k+1)/(c(k+1)-a(k+1)*al(k))
   1  bl(k+1)=(f(k+1)+a(k+1)*bl(k))/(c(k+1)-a(k+1)*al(k))
      y(n)=(f(n)+a(n)*bl(n-1))/(c(n)-a(n)*al(n-1))
      do 2 k=1,n-1
      l=n-k
   2  y(l)=al(l)*y(l+1)+bl(l)
      return
      end
      subroutine prgm(n,a,b,c,f,rm)
      dimension al(152,2,2),bl(152,2),
     *  a(151,2,2),b(151,2,2),c(151,2,2),f(151,2),rm(151,2)
c     print 34
      d=c(1,1,1)*c(1,2,2)-c(1,2,1)*c(1,1,2)
      al(1,1,1)=1./d*(c(1,2,2)*b(1,1,1)-c(1,1,2)*b(1,2,1))
      al(1,1,2)=1./d*(c(1,2,2)*b(1,1,2)-c(1,1,2)*b(1,2,2))
      al(1,2,1)=1./d*(c(1,1,1)*b(1,2,1)-c(1,2,1)*b(1,1,1))
      al(1,2,2)=1./d*(c(1,1,1)*b(1,2,2)-c(1,2,1)*b(1,1,2))
      bl(1,1)=1./d*(c(1,2,2)*f(1,1)-c(1,1,2)*f(1,2))
      bl(1,2)=1./d*(c(1,1,1)*f(1,2)-c(1,2,1)*f(1,1))
      do 1 k=2,n
      d1=(c(k,1,1)-a(k,1,1)*al(k-1,1,1)-a(k,1,2)*al(k-1,2,1))
      d2=(c(k,2,2)-a(k,2,1)*al(k-1,1,2)-a(k,2,2)*al(k-1,2,2))
      d3=(c(k,1,2)-a(k,1,1)*al(k-1,1,2)-a(k,1,2)*al(k-1,2,2))
      d4=(c(k,2,1)-a(k,2,1)*al(k-1,1,1)-a(k,2,2)*al(k-1,2,1))
      d=d1*d2-d3*d4
      r11=1./d*d2
      r22=1./d*d1
      r12=-1./d*d3
      r21=-1./d*d4
      r1=f(k,1)+a(k,1,1)*bl(k-1,1)+a(k,1,2)*bl(k-1,2)
      r2=f(k,2)+a(k,2,1)*bl(k-1,1)+a(k,2,2)*bl(k-1,2)
      if(k.eq.n)go to 3
      al(k,1,1)=r11*b(k,1,1)+r12*b(k,2,1)
      al(k,2,2)=r21*b(k,1,2)+r22*b(k,2,2)
      al(k,1,2)=r11*b(k,1,2)+r12*b(k,2,2)
      al(k,2,1)=r21*b(k,1,1)+r22*b(k,2,1)
      bl(k,1)=r11*r1+r12*r2
      bl(k,2)=r21*r1+r22*r2
c      print 34
c      print 35,k
c      print 33,al(k,1,1),al(k,1,2),bl(k,1),f(k,1)
c      print 33,al(k,2,1),al(k,2,2),bl(k,2),f(k,2)
      go to 4
    3 rm(k,1)=r11*r1+r12*r2
      rm(k,2)=r21*r1+r22*r2
    4 continue
    1 continue
      do 2 k=1,n-1
      l=n-k
      rm(l,1)=bl(l,1)+al(l,1,1)*rm(l+1,1)+al(l,1,2)*rm(l+1,2)
    2 rm(l,2)=bl(l,2)+al(l,2,1)*rm(l+1,1)+al(l,2,2)*rm(l+1,2)
  33  format(2x,2f12.7,3x,2f15.7)
  34  format('---------------------------------------------')
  35  format(2x,'j='i3)
      return
      end

      subroutine pr3(n,a,b,c,f,y)
      dimension al(152,3,3),bl(152,3),
     . a(151,3,3),b(151,3,3),c(151,3,3),f(151,3),y(151,3)
      r11=c(1,2,2)*c(1,3,3)-c(1,2,3)*c(1,3,2)
      r12=c(1,2,3)*c(1,3,1)-c(1,2,1)*c(1,3,3)
      r13=c(1,2,1)*c(1,3,2)-c(1,2,2)*c(1,3,1)
      r21=c(1,1,3)*c(1,3,2)-c(1,1,2)*c(1,3,3)
      r22=c(1,1,1)*c(1,3,3)-c(1,1,3)*c(1,3,1)
      r23=c(1,1,2)*c(1,3,1)-c(1,1,1)*c(1,3,2)
      r31=c(1,1,2)*c(1,2,3)-c(1,1,3)*c(1,2,2)
      r32=c(1,1,3)*c(1,2,1)-c(1,1,1)*c(1,2,3)
      r33=c(1,1,1)*c(1,2,2)-c(1,1,2)*c(1,2,1)
      d=r11*c(1,1,1)+r12*c(1,1,2)+r13*c(1,1,3)
      e11=r11/d
      e12=r21/d
      e13=r31/d
      e21=r12/d
      e22=r22/d
      e23=r32/d
      e31=r13/d
      e32=r23/d
      e33=r33/d
      al(1,1,1)=e11*b(1,1,1)+e12*b(1,2,1)+e13*b(1,3,1)
      al(1,2,1)=e21*b(1,1,1)+e22*b(1,2,1)+e23*b(1,3,1)
      al(1,3,1)=e31*b(1,1,1)+e32*b(1,2,1)+e33*b(1,3,1)
      al(1,1,2)=e11*b(1,1,2)+e12*b(1,2,2)+e13*b(1,3,2)
      al(1,2,2)=e21*b(1,1,2)+e22*b(1,2,2)+e23*b(1,3,2)
      al(1,3,2)=e31*b(1,1,2)+e32*b(1,2,2)+e33*b(1,3,2)
      al(1,1,3)=e11*b(1,1,3)+e12*b(1,2,3)+e13*b(1,3,3)
      al(1,2,3)=e21*b(1,1,3)+e22*b(1,2,3)+e23*b(1,3,3)
      al(1,3,3)=e31*b(1,1,3)+e32*b(1,2,3)+e33*b(1,3,3)
      bl(1,1)=e11*f(1,1)+e12*f(1,2)+e13*f(1,3)
      bl(1,2)=e21*f(1,1)+e22*f(1,2)+e23*f(1,3)
      bl(1,3)=e31*f(1,1)+e32*f(1,2)+e33*f(1,3)
      do 1 i=2,n
      j=i-1
      rr11=a(i,1,1)*al(j,1,1)+a(i,1,2)*al(j,2,1)+a(i,1,3)*al(j,3,1)
      rr21=a(i,2,1)*al(j,1,1)+a(i,2,2)*al(j,2,1)+a(i,2,3)*al(j,3,1)
      rr31=a(i,3,1)*al(j,1,1)+a(i,3,2)*al(j,2,1)+a(i,3,3)*al(j,3,1)
      rr12=a(i,1,1)*al(j,1,2)+a(i,1,2)*al(j,2,2)+a(i,1,3)*al(j,3,2)
      rr22=a(i,2,1)*al(j,1,2)+a(i,2,2)*al(j,2,2)+a(i,2,3)*al(j,3,2)
      rr32=a(i,3,1)*al(j,1,2)+a(i,3,2)*al(j,2,2)+a(i,3,3)*al(j,3,2)
      rr13=a(i,1,1)*al(j,1,3)+a(i,1,2)*al(j,2,3)+a(i,1,3)*al(j,3,3)
      rr23=a(i,2,1)*al(j,1,3)+a(i,2,2)*al(j,2,3)+a(i,2,3)*al(j,3,3)
      rr33=a(i,3,1)*al(j,1,3)+a(i,3,2)*al(j,2,3)+a(i,3,3)*al(j,3,3)
      r11=c(i,1,1)-rr11
      r12=c(i,1,2)-rr12
      r13=c(i,1,3)-rr13
      r21=c(i,2,1)-rr21
      r22=c(i,2,2)-rr22
      r23=c(i,2,3)-rr23
      r31=c(i,3,1)-rr31
      r32=c(i,3,2)-rr32
      r33=c(i,3,3)-rr33
      rr11=r22*r33-r23*r32
      rr12=r23*r31-r21*r33
      rr13=r21*r32-r22*r31
      rr21=r13*r32-r12*r33
      rr22=r11*r33-r13*r31
      rr23=r12*r31-r11*r32
      rr31=r12*r23-r13*r22
      rr32=r13*r21-r11*r23
      rr33=r11*r22-r12*r21
      d=r11*rr11+r12*rr12+r13*rr13
      e11=rr11/d
      e12=rr21/d
      e13=rr31/d
      e21=rr12/d
      e22=rr22/d
      e23=rr32/d
      e31=rr13/d
      e32=rr23/d
      e33=rr33/d
      r1=a(i,1,1)*bl(j,1)+a(i,1,2)*bl(j,2)+a(i,1,3)*bl(j,3)+f(i,1)
      r2=a(i,2,1)*bl(j,1)+a(i,2,2)*bl(j,2)+a(i,2,3)*bl(j,3)+f(i,2)
      r3=a(i,3,1)*bl(j,1)+a(i,3,2)*bl(j,2)+a(i,3,3)*bl(j,3)+f(i,3)
      if(i.ne.n)then
       al(i,1,1)=e11*b(i,1,1)+e12*b(i,2,1)+e13*b(i,3,1)
       al(i,2,1)=e21*b(i,1,1)+e22*b(i,2,1)+e23*b(i,3,1)
       al(i,3,1)=e31*b(i,1,1)+e32*b(i,2,1)+e33*b(i,3,1)
       al(i,1,2)=e11*b(i,1,2)+e12*b(i,2,2)+e13*b(i,3,2)
       al(i,2,2)=e21*b(i,1,2)+e22*b(i,2,2)+e23*b(i,3,2)
       al(i,3,2)=e31*b(i,1,2)+e32*b(i,2,2)+e33*b(i,3,2)
       al(i,1,3)=e11*b(i,1,3)+e12*b(i,2,3)+e13*b(i,3,3)
       al(i,2,3)=e21*b(i,1,3)+e22*b(i,2,3)+e23*b(i,3,3)
       al(i,3,3)=e31*b(i,1,3)+e32*b(i,2,3)+e33*b(i,3,3)
       bl(i,1)=e11*r1+e12*r2+e13*r3
       bl(i,2)=e21*r1+e22*r2+e23*r3
       bl(i,3)=e31*r1+e32*r2+e33*r3
      else
       y(n,1)=e11*r1+e12*r2+e13*r3
       y(n,2)=e21*r1+e22*r2+e23*r3
       y(n,3)=e31*r1+e32*r2+e33*r3
      endif
  1   continue
      do 2 i=1,n-1
      l=n-i
      y(l,1)=al(l,1,1)*y(l+1,1)+al(l,1,2)*y(l+1,2)+al(l,1,3)*y(l+1,3)
     . +bl(l,1)
      y(l,2)=al(l,2,1)*y(l+1,1)+al(l,2,2)*y(l+1,2)+al(l,2,3)*y(l+1,3)
     . +bl(l,2)
      y(l,3)=al(l,3,1)*y(l+1,1)+al(l,3,2)*y(l+1,2)+al(l,3,3)*y(l+1,3)
     . +bl(l,3)
  2   continue
      return
      end

