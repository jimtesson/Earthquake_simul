      program coupling
c


!!!!!!!!!! Equations importantes !!!!!!!!!!
!
!!!!!Equation pour passer dans le repère xy que l'on visualise (x horizontal et y vertical)
!   cosi =  sin(degre*orient(i))
!   sini = -cos(degre*orient(i))
!
!!!!!Equation aller:
!   xmi =  cosi*xmt   + sini*ymt
!   ymi = -sini*xmt   + cosi*ymt
!
!!!!!Equation retour:
!   x=cosi*xi-sini*yi
!   y=sini*xi+cosi*yi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


c---- calcul de la perturbation de contrainte induite par une faille
c     rectangulaire sur un autre plan de faille
c
      parameter (nfx=10)
c
      implicit real*8(a-h,o-z)
c
      character char3*3 !,name*5
      !character*6 filin,filout,filplot,filerr,filallvar
!      character*6
      character (len=500) name,filename,
     .        filin,filout,filplot,filerr,filallvar
     .        ,filplotcfs, filcross, filslip, filmatrix
c
      dimension x    (nfx), y     (nfx), exten(nfx),
     .          depth1(nfx), depth2(nfx),orient(nfx), dip  (nfx),
     .          al1  (nfx), al2   (nfx), aw1  (nfx), aw2(nfx),
     .          disl1(nfx), disl2 (nfx), disl3(nfx),
     .          strike(nfx), rake(nfx),
     .          xslipvec(nfx),yslipvec(nfx),zslipvec(nfx)
      dimension couple(nfx,nfx)
      dimension sigma(3,3),dnj(3),dthj(3),dtvj(3),tension(3),
     .          slipvec(3),slipvecij(3)
      dimension s0(6),sn(6)
c
c---- constantes
c
      ndime = 3
      pi = acos(-1.0)
      radian = 180. / pi
      degre  = pi / 180.
cc      write(*,*)'pi =',pi
cc      write(*,*) sin(0.5*pi)
cc      write(*,*) cos(0.5*pi)
cc      write(*,*) sin(pi)
cc      write(*,*) cos(pi)
c
c---- lecture
c
      iin  = 10
      iout = 11
      iplot= 12
      ierr = 13
      iallvar = 70
      iplotcfs = 71

      icross = 72
c 
      write(*,*) ' nom de l''essai ?'
      read(*,'(a5)') name
c      name = 'test'
c
      !filin  (1:1) = 'i'
      !filin  (2:6) = name
      !filout (1:1) = 'o'
      !filout (2:6) = name
      !filplot(1:1) = 'p'
      !filplot(2:6) = name
      !filcross(1:1) = 's'
      !filcross(2:6) = name
      !filerr (1:1) = 'e'
      !filerr (2:6) = name
      !filallvar(1:1) = 'v'
      !filallvar(2:6) = name
      !filplotcfs(1:1) = 'c'
      !filplotcfs(2:6) = name
      !filslip(1:1) = 'u'
      !filslip(2:6) = name



        ! input file
        name=trim(name)
        filin = trim('../input/i_')//trim(name)
        filin = trim(filin)
        ! out file
        filout = trim('../output/o_')//trim(name)
        filout = trim(filout)
        ! plot file
        filplot = trim('../output/p_')//trim(name)
        filplot = trim(filplot)
        ! err file
        filerr = trim('../output/e_')//trim(name)
        filerr = trim(filerr)
        ! cross file
        filplotcfs = trim('../output/c_')//trim(name)
        filplotcfs = trim(filplotcfs)
        ! displacement file
        filslip = trim('../output/u_')//trim(name)
        filslip = trim(filslip)
       ! var file
        filallvar = trim('../output/v_')//trim(name)
        filallvar = trim(filallvar)
       ! var file
        filcross = trim('../output/s_')//trim(name)
        filcross = trim(filcross)
       ! matrix file
        filmatrix = trim('../output/m_')//trim(name)
        filmatrix = trim(filmatrix)


c------------------------------------------------
c---- Lecture du fichier input
c------------------------------------------------
      open(iin  ,file=filin,status='old')
      open(iout ,file=filout)
      open(iplot,file=filplot)
      open(iplotcfs,file=filplotcfs)
      open(iallvar,file=filallvar)
      open(ierr ,file=filerr)
      open(iplotcross ,file=filcross)
      open(iplotslip ,file=filslip)
      open(imatrix ,file=filmatrix)

c
      !open(iin,file=filin)
      read(iin,*) iecho

      read(iin,'(A3)') char3 !skip line
      read(iin,*) imap
      read(iin,*) ivar,ivarlog
      read(iin,*) xmap0, ymap0, zmap0
      read(iin,*) nmapa, nmapb, nmapc
      read(iin,*) dmapa, dmapb, dmapc
      read(iin,*) orimap, dipmap

      read(iin,'(A3)') char3 !skip line
      read(iin,*) icross
      read(iin,*) xcross0, ycross0, xcross1,
     .           ycross1, ncrossxy
      read(iin,*) zcross0, zcross1, ncrossz

      read(iin,'(A3)') char3 !skip line
      read(iin,*) icfsmap, zcfs
      read(iin,*) iviewsurf
      read(iin,'(A3)') char3 !skip line
      read(iin,*) cutoff
      read(iin,*) e
      read(iin,*) poisson
      read(iin,*) friction
      read(iin,*) ipress
      read(iin,*) nfault
      read(iin,'(A3)') char3 !skip line

      do i=1,nfault
         read(iin,'(A3)') char3
         if (char3 .ne. 'NEW') then
            write(*,*) ' erreur de lecture '
            write(*,*) ' char3 = '
            write(*,'(A3)') char3
            stop
         endif
         read(iin,*) x(i),y(i)
         read(iin,*) exten(i)
         read(iin,*) depth1(i),depth2(i)
         read(iin,*) strike(i),dip(i),rake(i)
         read(iin,*) disl1(i),disl2(i),disl3(i)

c----    geometrical parameters
         orient(i) = 180.0 - strike(i)
         sindip   = sin(degre*dip(i))


         x(i)     = 1000. * x(i)
         y(i)     = 1000. * y(i)
         exten(i) = 1000. * exten(i)
         al1(i)   = 0.0
         al2(i)   = exten(i)
         depth1(i) = 1000. * depth1(i)
         aw1(i)   = 0.0
         depth2(i) = 1000. * depth2(i)
         aw2(i)   = (depth2(i)-depth1(i)) / sindip


c-----  Determine if dip fault or lateral fault
        ierror=0
        write(*,*)"Faille",i
        if(rake(i).eq.180.0) then
            write(*,*)"  right-lateral fault"
            if(disl1(i).gt.0) ierror=1
        elseif(rake(i).eq.0.0) then
            write(*,*)"  left-lateral fault"
            if(disl1(i).lt.0) ierror=1
        elseif(rake(i).eq.-90.0) then
            write(*,*)"  normal fault"
            if(disl1(i).gt.0) ierror=1
        elseif(rake(i).eq.90.0) then
            write(*,*)"  reverse fault"
            if(disl1(i).lt.0) ierror=1
        endif

        if(ierror.eq.1) then
            write(*,*) ' erreur Deplacements-rake '
            stop
        endif

      enddo
      read(iin,'(A3)') char3
      if (char3 .ne. 'END') then
         write(*,*) ' erreur de lecture '
         write(*,*) ' char3 = '
         write(*,'(A3)') char3
         stop
      endif
c
      close(iin)
c
      write(*,*) ' fin de la lecture... '
c
c---- parametres elastiques
c
      xlamb = poisson*e / ( (1.+poisson) * (1.-2.*poisson) )
      xmu   = e / ( 2. * (1.+poisson) )
      alpha = (xlamb + xmu) / (xlamb + 2.*xmu)
      if (ipress .eq. 1) then
         cpress = 1.0
      else
         cpress = 0.0
      endif
      if (iecho .ge. 1) then
         write(iout,*) 'xlamb, xmu, alpha, cpress'
         write(iout,*)  xlamb, xmu, alpha, cpress
      endif
c

        couple(:,:)=.0


c------------------------------------------------
c---- calcul des variable pour tracer la carte
c------------------------------------------------

      xmap0     = 1000. * xmap0
      ymap0     = 1000. * ymap0
      zmap0     = 1000. * zmap0
      dmapa     = 1000. * dmapa
      dmapb     = 1000. * dmapb
      dmapc     = 1000. * dmapc

         if (nmapa .ge. 2) then
            dmapa = dmapa / (nmapa - 1)
         else
            dmapa = 0.0
         endif
         if (nmapb .ge. 2) then
            dmapb = dmapb / (nmapb - 1)
         else
            dmapb = 0.0
         endif
         if (nmapc .ge. 2) then
            dmapc = dmapc / (nmapc - 1)
         else
            dmapc = 0.0
         endif
         cosmapa =  cos(degre*orimap)
         sinmapa =  sin(degre*orimap)
         cosmapb =  cos(degre*dipmap)
         sinmapb = -sin(degre*dipmap)
         dxmapa  = dmapa * cosmapa
         dymapa  = dmapa * sinmapa
         dzmapa  = 0.0
         dchecka = sqrt(dxmapa*dxmapa+dymapa*dymapa)
         dxmapb  = -dmapb * sinmapa * cosmapb
         dymapb  =  dmapb * cosmapa * cosmapb
         dzmapb  =  dmapb           * sinmapb
         dcheckb = sqrt(dxmapb*dxmapb+dymapb*dymapb
     .           +      dzmapb*dzmapb)

c 
         if (iecho .ge. 1) then
            write(iout,*) 'orimap,dipmap,degre'
            write(iout,*)  orimap,dipmap,degre
            write(iout,*) 'cosmapa,sinmapa'
            write(iout,*)  cosmapa,sinmapa
            write(iout,*) 'cosmapb,sinmapb'
            write(iout,*)  cosmapb,sinmapb
            write(iout,*) 'dxmapa,dymapa,dzmapa'
            write(iout,*)  dxmapa,dymapa,dzmapa
            write(iout,*)  
            write(iout,*) 'dxmapb,dymapb,dzmapb'
            write(iout,*)  dxmapb,dymapb,dzmapb
            write(iout,*) 'dchecka,dcheckb'
            write(iout,*)  dchecka,dcheckb
         endif

c------------------------------------------------
c---- calcul des variable pour tracer la coupe
c------------------------------------------------


      xcross1     = 1000. * xcross1
      xcross0     = 1000. * xcross0
      ycross1     = 1000. * ycross1
      ycross0     = 1000. * ycross0
      zcross1     = 1000. * zcross1
      zcross0     = 1000. * zcross0

         if (ncrossxy .ge. 2) then
            dxcross = (xcross1-xcross0) / (ncrossxy - 1)
            dycross = (ycross1-ycross0) / (ncrossxy - 1)
         else
            dxcross = 0.0
            dycross = 0.0
         endif
         if (ncrossz .ge. 2) then
            dzcross = (zcross1-zcross0) / (ncrossz - 1)
         else
            dzcross = 0.0
         endif


c
c---- calcul de l'interaction de la faille i sur la faille j.
c     pour cela on procede a une transformation de coordonnees pour
c     se placer dans le repere de la faille i
c
      ipass = 0
      do i=1,nfault
         disloc = sqrt(disl1(i)*disl1(i)+disl2(i)*disl2(i)) ! distance to the fault
         smoment0 = 1.e7 * xmu * al2(i)*aw2(i) * disloc ! moment sismique
         smagms   = (log10(smoment0) - 16.1) / 1.5 ! magnitude
         if (iecho .ge. 2) then
            write(iout,*) ' ************************* '
            write(iout,*) ' calcul pour la faille ',i
            write(iout,*) ' Dislocation ', disloc
            write(iout,*) ' Surface     ', al2(i)*aw2(i)
            write(iout,*) ' Moment M0   ', smoment0
            write(iout,*) ' Magnitude Ms', smagms
            write(*,*) ' ************************* '
            write(*,*) ' calcul pour la faille ',i
            write(*,*) ' Dislocation ', disloc
            write(*,*) ' Surface     ', al2(i)*aw2(i)
            write(*,*) ' Moment M0   ', smoment0
            write(*,*) ' Magnitude Ms', smagms
         endif

            cosi =  sin(degre*orient(i))
            sini = -cos(degre*orient(i))
            tandipi=tan(degre*dip(i))

            !half fault(i) length
            dlxi = cosi * 0.5 * exten(i)
            dlyi = sini * 0.5 * exten(i)


            !fault i starting and ending point
            xs = x(i) - dlxi
            ys = y(i) - dlyi
            xf = x(i) + dlxi
            yf = y(i) + dlyi

         do j=1,nfault
            ipass = ipass + 1

!           Parameter of the fault j
            if(dip(j).EQ.0.0) then
                dipj = 0.000001
            elseif(dip(j).EQ.90.0) then
                dipj = 89.999999
            else
                dipj = dip(j)
            endif

!           shift to calculate the position of j
                dxdepthj = -zcfs*1000./tan(degre*dipj)
     .              *cos(-degre*strike(j)) ! in the global referentiel
                dydepthj = -zcfs*1000./tan(degre*dipj)
     .              *sin(-degre*strike(j))

!           depth of reference for the okada routine
                    depth = (depth1(i)+depth2(i))/2.0
!           top/bottom of the fault plane
                    top = depth1(i)
                    bottom =depth2(i)

!           !!!!Calcule de l'interaction i-j
            if ((imap.eq.0) .and. (j .ne. i)) then
               if (iecho .ge. 2) then
                  write(iout,*) ' interaction avec la faille ',j
                  write(iout,*) ' ************************* '
                  write(*,*) ' -> interaction avec la faille ',j
               endif


                !position at depth
                xj = x(j)-dxdepthj
                yj = y(j)-dydepthj


                !Conversion de la position de j dans le referentiel i
                top = depth1(i)
                bottom =depth2(i)
                call coord_conversion (xj,yj,xs,ys,xf,yf,
     .                       top,bottom,dip(i),xn,yn,al,aw)

                !distance par rapport au centre de i
                dist = sqrt(xn*xn+yn*yn)
                !depth of calcul
                zjc = -zcfs*1000.
c
c----          calcul du deplacement et de son gradient sur la faille j
c                   dans le referentiel local de la faille i

                  call dc3d (alpha   ,xn   ,yn     ,  zjc   ,
     .                       depth,dip(i),-al  ,al  ,
     .                       -aw  ,aw,disl1(i),disl2(i),
     .                       disl3(i),ux    ,uy      ,uz      ,
     .                       uxx     ,uyx   ,uzx     ,uxy     ,
     .                       uyy     ,uzy   ,uxz     ,uyz     ,
     .                       uzz     ,flag                    )
               write(iout,*) 'flag =',flag


c
c----          calcule inutiles pour avoir le vecteur
c
               if (iecho .ge. 3) then
               cosj =  cos(degre*orient(j))
               sinj =  sin(degre*orient(j))

               vx  =  cosi*cosj + sini*sinj
               vy  = -sini*cosj + cosi*sinj

               xji =  cosi*xj   + sini*yj
               yji = -sini*xj   + cosi*yj
               zji = -zcfs*1000.

               dlxj =  vy * 0.5 * exten(j)
               dlyj = -vx * 0.5 * exten(j)
               xjc  = xji 
               yjc  = yji 
               zjc = -zcfs*1000.
c
               distji = sqrt(xji*xji+yji*yji)
               distjc = sqrt(xjc*xjc+yjc*yjc)
               orientj = radian*atan2(vy,vx)
               azimutj = radian*atan2(yji,xji)

               dipdeg = degre*dip(j)
               sindip = sin(dipdeg)
               cosdip = cos(dipdeg)
c

                  write(iout,*)'   '
                  write(iout,*)' x(i),y(i) '
                  write(iout,*)  x(i),y(i)  
                  write(iout,*)' xi,yi '
                  write(iout,*)  xi,yi  
                  write(iout,*)'  xj,yj ' 
                  write(iout,*)  x(j),y(j)
                  write(iout,*)' orient(i), orient(j) '
                  write(iout,*)  orient(i), orient(j) 
                  write(iout,*)'  '
                  write(iout,*)' base de la faille i (strike vector)'
                  write(iout,*)' cosi,sini ', cosi,sini
                  write(iout,*)'  '
                  write(iout,*)' projection de la faille j (normal v.)'
                  write(iout,*)' cosj,sinj ', cosj,sinj
                  write(iout,*)'  '
                  write(iout,*)' projection j tournee'
                  write(iout,*)' vx,vy ', vx,vy
                  write(iout,*)'  '
                  write(iout,*)' position tournee '
                  write(iout,*)' xji,yji,distji ', xji,yji,distji
                  write(iout,*)' xjc,yjc,distjc ', xjc,yjc,distjc
                  write(iout,*)' dist ',dist
                  write(iout,*)'  '
                  write(iout,*)' azimutj', azimutj
                  write(iout,*)' orientj', orientj
                  write(iout,*)'  '
                  write(iout,*)
     .            ' alpha,xji,yji,zji,depth1(i),depth2(i),dip(i)'
                  write(iout,*)  
     .              alpha,xji,yji,zji,depth1(i),depth2(i),dip(i)
                  write(iout,*)' al1(i),al2(i),aw1(i),aw2(i) '
                  write(iout,*)  al1(i),al2(i),aw1(i),aw2(i)
                  write(iout,*)' disl1(i),disl2(i),disl3(i) '
                  write(iout,*)  disl1(i),disl2(i),disl3(i) 
               endif




c  
c----          calcul des deformations et des contraintes dans le referentiel faille i
c
               exx = uxx
               eyy = uyy
               ezz = uzz
               exy = 0.5 * (uxy + uyx)
               exz = 0.5 * (uxz + uzx)
               eyz = 0.5 * (uyz + uzy)
               trstr = exx + eyy + ezz
               sigma(1,1) = cpress*xlamb*trstr + 2.* xmu * exx
               sigma(2,2) = cpress*xlamb*trstr + 2.* xmu * eyy
               sigma(3,3) = cpress*xlamb*trstr + 2.* xmu * ezz
               sigma(1,2) =                      2.* xmu * exy
               sigma(1,3) =                      2.* xmu * exz
               sigma(2,3) =                      2.* xmu * eyz
               sigma(2,1) = sigma(1,2)
               sigma(3,1) = sigma(1,3)
               sigma(3,2) = sigma(2,3)


        ! conversion des déplcaments dans le systeme global
                UXG = ux*cosi-uy*sini   ! global referentiel
                UYG = ux*sini+uy*cosi   ! global referentiel
                UZG = uz                ! global referentiel

                !write(*,*)'UXG,UYG,UZG',UXG,UYG,UZG
        ! Calcul du vecteur contrainte dans le referentiel global
        !s0 = [ssxx; ssyy; sszz; ssyz; ssxz; ssxy];
        s0(1)=sigma(1,1)
        s0(2)=sigma(2,2)
        s0(3)=sigma(3,3)
        s0(4)=sigma(2,3)
        s0(5)=sigma(1,3)
        s0(6)=sigma(1,2)

        call tensor_trans(sini,cosi,s0,sn)
                    sigma(1,1) = sn(1)
                    sigma(1,2) = sn(6)
                    sigma(1,3) = sn(5)
                    sigma(2,1) = sn(6)
                    sigma(2,2) = sn(2)
                    sigma(2,3) = sn(4)
                    sigma(3,1) = sn(5)
                    sigma(3,2) = sn(4)
                    sigma(3,3) = sn(3)

        ! projection du tenseur de contrainte sur le plan de faille
        a_calc = strike(j)
        b_calc = dip(j)
        c_calc = rake(j)
        call calc_coulomb (a_calc,b_calc,c_calc,friction,
     .                            sn,shear,normal,coulomb)

                couple(i,j) = coulomb/100000.0


c
               if (iecho .ge. 2) then
                write(*,*)'normal,shear,CFS:',normal,shear,coulomb
               endif

c
c----          calcul des invariants du tenseur des contraintes
c
        call INVAR (sigma,VARJ12d,VARJ22d,VARJ32d,ndime-1)
        call INVAR (sigma,VARJ1  ,VARJ2  ,VARJ3  ,ndime)
               xVARJ22d = sqrt(3.0*VARJ22d)
               xVARJ2 = sqrt(3.0*VARJ2)
cc               xvarj3 = (0.5*27*VARJ3)**(0.333333333333333)
        if (iecho .ge. 2) then
            write(iout,100) x(j),y(j)
            write(iout,101) xjc,yjc,zji
            write(iout,102) disl1(i),disl2(i),disl3(i)
            write(iout,110) UXG,UYG,UZG
            write(iout,130) sigma
            write(iout,131) VARJ1,xVARJ2
            write(iout,132) VARJ12d,xVARJ22d
        endif
c


               
c               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c               !!!!!!!!!!!! CFS map  !!!!!!!!!!!!
c               !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               if(icfsmap.eq.1) then
               write(*,*)'Caluclate CFS map'
               
               do ia=1,nmapa
                  do ib=1,nmapb
                    	!do ic=1,nmapc
                     xma = float(ia-1) * dmapa
                     xmb = float(ib-1) * dmapb
                     xm  = xmap0 + (ia-1)*dxmapa + (ib-1)*dxmapb
                     ym  = ymap0 + (ia-1)*dymapa + (ib-1)*dymapb
                     !zm  = zmap0 + (ic-1)*dmapc

                     if(iviewsurf.eq.1) then ! to view the CFS map considering the fault position at surface and the fault diping
                        xmt = xm -dxdepthj
                        ymt = ym -dydepthj
                     else
                        xmt = xm
                        ymt = ym
                     endif

                !Conversion de la position de j dans le referentiel i
                call coord_conversion (xmt,ymt,xs,ys,xf,yf,
     .                       top,bottom,dip(i),xmi,ymi,al,aw)

                zmi = -zcfs*1000. !depth of calcul

c
c----          calcul du deplacement et de son gradient sur la faille j
c                   dans le referentiel local de la faille i


                  call dc3d (alpha   ,xmi   ,ymi     ,  zmi   ,
     .                       depth,dip(i),-al  ,al  ,
     .                       -aw  ,aw,disl1(i),disl2(i),
     .                       disl3(i),ux    ,uy      ,uz      ,
     .                       uxx     ,uyx   ,uzx     ,uxy     ,
     .                       uyy     ,uzy   ,uxz     ,uyz     ,
     .                       uzz     ,flag                    )


	         !calcul des deformations et des contraintes referentiel local (faille i)

               exx = uxx
               eyy = uyy
               ezz = uzz
               exy = 0.5 * (uxy + uyx)
               exz = 0.5 * (uxz + uzx)
               eyz = 0.5 * (uyz + uzy)
               trstr = exx + eyy + ezz
               sigma(1,1) = cpress*xlamb*trstr + 2.* xmu * exx
               sigma(2,2) = cpress*xlamb*trstr + 2.* xmu * eyy
               sigma(3,3) = cpress*xlamb*trstr + 2.* xmu * ezz
               sigma(1,2) =                      2.* xmu * exy
               sigma(1,3) =                      2.* xmu * exz
               sigma(2,3) =                      2.* xmu * eyz
               sigma(2,1) = sigma(1,2)
               sigma(3,1) = sigma(1,3)
               sigma(3,2) = sigma(2,3)
                    

                ! Conversion des déformation dans le référentiel global ------------

                UXG = ux*cosi-uy*sini   ! global referentiel
                UYG = ux*sini+uy*cosi   ! global referentiel
                UZG = uz                ! global referentiel

                !transformation du vecteur de contrainte dans le referentiel global
                s0(1)=sigma(1,1)
                s0(2)=sigma(2,2)
                s0(3)=sigma(3,3)
                s0(4)=sigma(2,3)
                s0(5)=sigma(1,3)
                s0(6)=sigma(1,2)

                call tensor_trans(sini,cosi,s0,sn)

                ! calcul de la contrainte normale, tengentielle et
                ! du CFS en fonction du strike, dip et rake de la
                ! faille j dans le referentiel global
                    sigma(1,1) = sn(1)
                    sigma(1,2) = sn(6)
                    sigma(1,3) = sn(5)
                    sigma(2,1) = sn(6)
                    sigma(2,2) = sn(2)
                    sigma(2,3) = sn(4)
                    sigma(3,1) = sn(5)
                    sigma(3,2) = sn(4)
                    sigma(3,3) = sn(3)

                    a_calc = strike(j)
                    b_calc = dip(j)
                    c_calc = rake(j)
                    call calc_coulomb (a_calc,b_calc,c_calc,
     .                  friction,sn,shear,normal,coulomb)

                cutoffcfs =100
     		   if (cfsh .gt. cutoffcfs) cfsh = cutoffcfs
               if (cfsh .lt.-ccutoffcfs) cfsh =-cutoffcfs
     		   if (cfsv .gt. cutoffcfs) cfsv = cutoffcfs
               if (cfsv .lt.-cutoffcfs) cfsv =-cutoffcfs
     		   
     		   !write in the file
     			write(iplotcfs,270) i,j,0.001*xm,0.001*ym,0.001*zmi,
     .        normal/100000.0,shear/100000.0,coulomb/100000.0

     		   
               	   		!enddo
               	    enddo
               enddo
               endif !!!!!!!!!!!!!!!!!!!!!!!!! END OF CFS MAP !!!!!!!!!!!!!!!!!!


c-------------------------------------------------------------------------------
c          Map of displacements and all variables from okada (stress, strain)
c-----                                                --------------------------
            
            elseif (imap.eq.1 .and. j.eq.i) then !.and. ipass.eq.1) then
               write(*,*) ' Calcul de la carte '
               do ia=1,nmapa
                  do ib=1,nmapb
                  do ic=1,nmapc




                     xma = float(ia-1) * dmapa
                     xmb = float(ib-1) * dmapb
                     xm  = xmap0 + (ia-1)*dxmapa + (ib-1)*dxmapb
                     ym  = ymap0 + (ia-1)*dymapa + (ib-1)*dymapb
                     zm  = zmap0 + (ic-1)*dmapc

                    if(iviewsurf.eq.1) then ! to view the CFS map considering the fault position at surface and the fault diping
                        xmt = xm -dxdepthj
                        ymt = ym -dydepthj
                     else
                        xmt = xm
                        ymt = ym
                     endif

                !Conversion de la position de j dans le referentiel i
                call coord_conversion (xmt,ymt,xs,ys,xf,yf,
     .                       top,bottom,dip(i),xmi,ymi,al,aw)

                zmi = -zcfs*1000. !depth of calcul

c
c----          calcul du deplacement et de son gradient sur la faille j
c                   dans le referentiel local de la faille i


                  call dc3d (alpha   ,xmi   ,ymi     ,  zmi   ,
     .                       depth,dip(i),-al  ,al  ,
     .                       -aw  ,aw,disl1(i),disl2(i),
     .                       disl3(i),ux    ,uy      ,uz      ,
     .                       uxx     ,uyx   ,uzx     ,uxy     ,
     .                       uyy     ,uzy   ,uxz     ,uyz     ,
     .                       uzz     ,flag                    )

c  
c----                calcul des deformations et des contraintes
c


                     exx = uxx
                     eyy = uyy
                     ezz = uzz
                     exy = 0.5 * (uxy + uyx)
                     exz = 0.5 * (uxz + uzx)
                     eyz = 0.5 * (uyz + uzy)
                     trstr = exx + eyy + ezz
                     sigma(1,1) = cpress*xlamb*trstr + 2.* xmu * exx
                     sigma(2,2) = cpress*xlamb*trstr + 2.* xmu * eyy
                     sigma(3,3) = cpress*xlamb*trstr + 2.* xmu * ezz
                     sigma(1,2) =                      2.* xmu * exy
                     sigma(1,3) =                      2.* xmu * exz
                     sigma(2,3) =                      2.* xmu * eyz
                     sigma(2,1) = sigma(1,2)
                     sigma(3,1) = sigma(1,3)
                     sigma(3,2) = sigma(2,3)

!               ! Conversion des déformation dans le référentiel global ------------

                UXG = ux*cosi-uy*sini   ! global referentiel
                UYG = ux*sini+uy*cosi   ! global referentiel
                UZG = uz                ! global referentiel

                !transformation du vecteur de contrainte dans le referentiel global
                s0(1)=sigma(1,1)
                s0(2)=sigma(2,2)
                s0(3)=sigma(3,3)
                s0(4)=sigma(2,3)
                s0(5)=sigma(1,3)
                s0(6)=sigma(1,2)

                call tensor_trans(sini,cosi,s0,sn)

                ! calcul de la contrainte normale, tengentielle et
                ! du CFS en fonction du strike, dip et rake de la
                ! faille j dans le referentiel global
                    sigma(1,1) = sn(1)
                    sigma(1,2) = sn(6)
                    sigma(1,3) = sn(5)
                    sigma(2,1) = sn(6)
                    sigma(2,2) = sn(2)
                    sigma(2,3) = sn(4)
                    sigma(3,1) = sn(5)
                    sigma(3,2) = sn(4)
                    sigma(3,3) = sn(3)


                a_calc = strike(j)
                b_calc = dip(j)
                c_calc = rake(j)
            call calc_coulomb (a_calc,b_calc,c_calc,friction,
     .                            sn,shear,normal,coulomb)

c----                calcul des invariants du tenseur des contraintes
c
                     call INVAR (sigma,VARJ1  ,VARJ2  ,VARJ3  ,ndime)
                     xVARJ2 = sqrt(3.0*VARJ2)

c					 !!!! var1
                     if (ivar.eq.1) var = UXG
                        var1 = UXG
                     !!!! var2
                     if (ivar.eq.2) var = UYG
                     !var2 = -sini*ux + cosi*uy
                        var2 = UYG
                     !!!! var12
                     if (ivar.eq.12) then
                        var = sqrt(UXG*UXG+UYG*UYG)
                     endif
                     var12 = sqrt(UXG*UXG+UYG*UYG)
                     !!!! var3
                     if (ivar.eq.3) var = UZG
                     var3 = UZG
                     !!!! var4
                     if (ivar.eq.4) var = cpress*xlamb*trstr
                     var4 = cpress*xlamb*trstr
                     !!!! var5
                     if (ivar.eq.5) then
                        var = 1.0e-5 * sigma(1,2)
                        if (var.gt.0.001) then
                           if(ivarlog.eq.1) var = log(var)
                        endif
                     endif
                     
                     var5 = 1.0e-5 * sigma(1,2)
                     if (var5.gt.0.001) then
                           if(ivarlog.eq.1) var5 = log(var5)
                        endif
                        
                     ! var6
                     if (ivar.eq.6) var = VARJ1
                     var6 = VARJ1
                     ! var7
                     if (ivar.eq.7) var = xVARJ2
                     var7 = xVARJ2
                     ! var8
                     if (ivar.eq.8) var = sigma(1,1)
                     var8 = sigma(1,1)
                     ! var9
                     if (ivar.eq.9) var = sigma(2,2)
                     var9 = sigma(2,2)
                     !cutoff
                     if (var1 .gt. cutoff) var1 = cutoff
                     if (var1 .lt.-cutoff) var1 =-cutoff
                     
                     if (var2 .gt. cutoff) var2 = cutoff
                     if (var2 .lt.-cutoff) var2 =-cutoff
                     
                     if (var3 .gt. cutoff) var3 = cutoff
                     if (var3 .lt.-cutoff) var3 =-cutoff
                     
                     if (var4 .gt. cutoff) var4 = cutoff
                     if (var4 .lt.-cutoff) var4 =-cutoff
                     
                     if (var5 .gt. cutoff) var5 = cutoff
                     if (var5 .lt.-cutoff) var5 =-cutoff
                     
                     if (var6 .gt. cutoff) var6 = cutoff
                     if (var6 .lt.-cutoff) var6 =-cutoff
                     
                     if (var7 .gt. cutoff) var7 = cutoff
                     if (var7 .lt.-cutoff) var7 =-cutoff
                     
                     if (var8 .gt. cutoff) var8 = cutoff
                     if (var8 .lt.-cutoff) var8 =-cutoff
                     
                     if (var9 .gt. cutoff) var9 = cutoff
                     if (var9 .lt.-cutoff) var9 =-cutoff
                     
                     if (var12 .gt. cutoff) var12 = cutoff
                     if (var12 .lt.-cutoff) var12 =-cutoff
                     

                     ymkm = 0.001*ym
                     if (ymkm .gt. 0.001) then
                        if (ivarlog.eq. 1) ymkm = log(ymkm)
                     else
                        ymkm = 0.0
                     endif
                     
                     !write output
                     write(iplot,250) 0.001*xm,0.001*ym,0.001*zm,var
                     write(iallvar,260) 0.001*xm,0.001*ym,0.001*zm,
     .                                  var1,var2,var3,var4,var5,
     .                                  var6,var7,var8,var9,var12
                     write(iplotslip,290)i,0.001*xm,0.001*ym,0.001*zm,
     .                                  var1,var2,var3,var12



                  enddo
                  enddo
               enddo
               endif


c-------------------------------------------------------------------------------
c                Cross section of displacement
c-----                                                --------------------------

              if(icross.eq.1 .and. ipass.eq.1) then
               write(*,*) ' -> Cross section of vertical displacements'

                !write(*,*)ncrossxy,ncrossz
                !write(*,*)xcross0,ycross0,dxcross
                !write(*,*)zcross0,dzcross
               do ia=1,ncrossxy

                  do ic=1,ncrossz

                     xm  = xcross0 + (ia-1)*dxcross
                     ym  = ycross0 + (ia-1)*dycross
                     zm  = zcross0 + (ic-1)*dzcross
                     xydist = ((xm-xcross0)*(xm-xcross0)
     .                          +(ym-ycross0)*(ym-ycross0))**0.5

                    
                     xmt = xm
                     ymt = ym


                !Conversion de la position de j dans le referentiel i
                call coord_conversion (xmt,ymt,xs,ys,xf,yf,
     .                       top,bottom,dip(i),xmi,ymi,al,aw)

                     zmi = -zm

c
c----          calcul du deplacement et de son gradient sur la faille j
c                   dans le referentiel local de la faille i


                   call dc3d (alpha   ,xmi   ,ymi     ,  zmi   ,
     .                       depth,dip(i),-al  ,al  ,
     .                       -aw  ,aw,disl1(i),disl2(i),
     .                       disl3(i),ux    ,uy      ,uz      ,
     .                       uxx     ,uyx   ,uzx     ,uxy     ,
     .                       uyy     ,uzy   ,uxz     ,uyz     ,
     .                       uzz     ,flag                    )

c  
c----                calcul des deformations et des contraintes
c
                     exx = uxx
                     eyy = uyy
                     ezz = uzz
                     exy = 0.5 * (uxy + uyx)
                     exz = 0.5 * (uxz + uzx)
                     eyz = 0.5 * (uyz + uzy)
                     trstr = exx + eyy + ezz
                     sigma(1,1) = cpress*xlamb*trstr + 2.* xmu * exx
                     sigma(2,2) = cpress*xlamb*trstr + 2.* xmu * eyy
                     sigma(3,3) = cpress*xlamb*trstr + 2.* xmu * ezz
                     sigma(1,2) =                      2.* xmu * exy
                     sigma(1,3) =                      2.* xmu * exz
                     sigma(2,3) =                      2.* xmu * eyz
                     sigma(2,1) = sigma(1,2)
                     sigma(3,1) = sigma(1,3)
                     sigma(3,2) = sigma(2,3)
!               ! Conversion des déformation dans le référentiel global ------------

                UXG = ux*cosi-uy*sini   ! global referentiel
                UYG = ux*sini+uy*cosi   ! global referentiel
                UZG = uz                ! global referentiel

                !transformation du vecteur de contrainte dans le referentiel global
                s0(1)=sigma(1,1)
                s0(2)=sigma(2,2)
                s0(3)=sigma(3,3)
                s0(4)=sigma(2,3)
                s0(5)=sigma(1,3)
                s0(6)=sigma(1,2)

                call tensor_trans(sini,cosi,s0,sn)

                ! calcul de la contrainte normale, tengentielle et
                ! du CFS en fonction du strike, dip et rake de la
                ! faille j dans le referentiel global
                    sigma(1,1) = sn(1)
                    sigma(1,2) = sn(6)
                    sigma(1,3) = sn(5)
                    sigma(2,1) = sn(6)
                    sigma(2,2) = sn(2)
                    sigma(2,3) = sn(4)
                    sigma(3,1) = sn(5)
                    sigma(3,2) = sn(4)
                    sigma(3,3) = sn(3)


                a_calc = strike(j)
                b_calc = dip(j)
                c_calc = rake(j)
            call calc_coulomb (a_calc,b_calc,c_calc,friction,
     .                            sn,shear,normal,coulomb)
                     
c
c----                calcul des invariants du tenseur des contraintes
c
                     call INVAR (sigma,VARJ1  ,VARJ2  ,VARJ3  ,ndime)
                     xVARJ2 = sqrt(3.0*VARJ2)

c					 !!!! var1
                        var1 =  UXG
                     !!!! var2
                        var2 = UYG
                     !!!! var3
                        var3 = UZG
                     !!!! var4
                        var4 = coulomb/100000.0
                     !cutoff
                     if (var1 .gt. cutoff) var1 = cutoff
                     if (var1 .lt.-cutoff) var1 =-cutoff
                     
                     if (var2 .gt. cutoff) var2 = cutoff
                     if (var2 .lt.-cutoff) var2 =-cutoff
                     
                     if (var3 .gt. cutoff) var3 = cutoff
                     if (var3 .lt.-cutoff) var3 =-cutoff

                     if (var4 .gt. 60.0) var4 = 60.0
                     if (var4 .lt.-60.0) var4 =-60.0

                     !write output
                write(iplotcross,280) 0.001*xm,0.001*ym,0.001*zm,
     .                     xydist, var1,var2,var3,var4
!                write(iplotcross,280) 0.001*xmi,0.001*ymi,0.001*zm,
!     .                                  xydist, ux,uy,uz


                  enddo
                  enddo
               endif
         enddo
      enddo
c
      write(*,*) '  '
      write(*,*) ' fin du calcul '
      
      write(*,*) ' Matrice (bar) '
c
      if (iecho .ge. 1) then
         do i=1,nfault
            write(iout,200)(couple(i,j),j=1,nfault)
            write(imatrix,200)(couple(i,j),j=1,nfault)
            write(*,200)(couple(i,j),j=1,nfault)
         enddo
      endif
c
      close(iout)
      close(imatrix)
      close(iplot)
      close(iplotcfs)
      close(iallvar)
      close(ierr)
      close(icross)
c
      stop
c
100   format (' Position (O) :',3f15.2)            
101   format (' Position (R) :',3f15.2)            
102   format (' Dislocation  :',3f15.2)            
110   format (' Deplacements :',3f15.6,/)     
120   format (' Gradients    :',/,3(15x,3f15.10,/))     
130   format (' Stress       :',/,3(15x,3f15.2,/)) 
131   format (' Invariants   :',3f15.2,/) 
132   format (' Invariants-2D:',3f15.2,/) 
135   format (' vecteur norm.:',3f15.10)            
136   format (' vecteur tver.:',3f15.10)            
137   format (' vecteur thor.:',3f15.10)            
140   format (' Tension      :',3f15.2)            
150   format (' Tension n    :',f15.2)            
160   format (' Tension tv   :',f15.2)            
170   format (' Tension th   :',f15.2)
171   format (' CFS   :',f15.4,/)
200   format (10f15.4)
250   format (11e15.4)
260   format (13e15.4)
270   format (2i2.0,7e15.4)
280   format (13e15.4)
290   format (1i2.0,8e15.4)
c     
      end
c
c*****************************************************************
c
      subroutine matvec (xmat,dir,vecout,ndime)
c
c---- projection d'un tenseur dans la direction donnee par dir
c
      implicit real*8(a-h,o-z)
c
      dimension xmat(ndime,ndime),
     .          dir (ndime),vecout(ndime)
c
      do i=1,ndime
         vecout(i) = 0.0
         do j=1,ndime
            vecout(i) = vecout(i) + xmat(i,j) * dir(j)
         enddo
      enddo
c
      return
      end
c
c*****************************************************************
c
      subroutine vecsca (vec,dir,scalar,ndime)
c
      implicit real*8(a-h,o-z)
c
c---- projection d'un vecteur dans la direction donnee par dir
c
      dimension vec (ndime), dir(ndime)
c
      scalar = 0.0
      do i=1,ndime
         scalar = scalar + vec(i) * dir(i)
      enddo
c
      return
      end
C
C**********************************************************************
C
      SUBROUTINE INVAR (sigma,VARJ1,VARJ2,VARJ3,NDIME)
C
C---- PROGRAM TO COMPUTE MAIN INVARIANTS OF A SECOND RANK TENSOR
C
C     NOTICE :
C     ------
C               S            : SYMMETRIC SECOND RANK TENSOR STORED AS
C                              A VECTOR.
C
C               DEVIA        : DEVIATORIC PART OF TENSOR S AS A VECTOR
C
C               NDIME        : SPATIAL DIMENSION OF THE PROBLEM
C
C      IF NDIME .EQ. 2 :
C
C                         S(1) ...... : SIGMA-XX
C                         S(2) ...... : SIGMA-YY
C                         S(3) ...... : SIGMA-XY
C                         S(4) ...... : SIGMA-ZZ
C
C      IF NDIME .EQ. 3 :
C
C                         S(1) ...... : SIGMA-XX
C                         S(2) ...... : SIGMA-YY
C                         S(3) ...... : SIGMA-ZZ
C                         S(4) ...... : SIGMA-XY
C                         S(5) ...... : SIGMA-XZ
C                         S(6) ...... : SIGMA-YZ
C
C----
C
      implicit real*8(a-h,o-z)
C
      DIMENSION sigma(3,3)
      DIMENSION S(6),DEVIA(6)
C
      DATA HALF,TWO,THREE/0.5D0,2.0D0,3.0D0/
C
C---- CHECK SPATIAL DIMENSION OF THE PROBLEM
C
      IF (NDIME .EQ. 2) then
C
C----    T W O   D I M E N S I O N A L   P R O B L E M
C
         s(1) = sigma(1,1)
         s(2) = sigma(2,2)
         s(3) = sigma(1,2)
         s(4) = sigma(3,3)
C
C----    COMPUTE MEAN VALUE
C
         VARJ1 = (S(1) + S(2) + S(4)) / THREE
C
C----    COMPUTE DEVIATORIC TENSOR AS A VECTOR
C
         DEVIA(1) = S (1) - VARJ1
         DEVIA(2) = S (2) - VARJ1
         DEVIA(4) = S (4) - VARJ1
         DEVIA(3) = S (3)
C
C----    COMPUTE SECOND INVARIANT
C
         VARJ2 = DEVIA(3)*DEVIA(3)
     .         + HALF*(DEVIA(1)*DEVIA(1)+DEVIA(2)*DEVIA(2)
     .                                  +DEVIA(4)*DEVIA(4))
C
C----    COMPUTE THIRD INVARIANT
C
         VARJ3 = DEVIA(4)*(DEVIA(4)*DEVIA(4)-VARJ2)
C
C
C----         T H R E E   D I M E N S I O N A L   P R O B L E M
C
      elseIF (NDIME .EQ. 3) then
         s(1) = sigma(1,1)
         s(2) = sigma(2,2)
         s(3) = sigma(3,3)
         s(4) = sigma(1,2)
         s(5) = sigma(1,3)
         s(6) = sigma(2,3)
C
C----    COMPUTE MEAN VALUE
C
         VARJ1 = (S(1) + S(2) + S(3)) / THREE
C
C----    COMPUTE DEVIATORIC TENSOR
C
         DEVIA(1) = S (1) - VARJ1
         DEVIA(2) = S (2) - VARJ1
         DEVIA(3) = S (3) - VARJ1
         DEVIA(4) = S (4)
         DEVIA(5) = S (5)
         DEVIA(6) = S (6)
C
C----    COMPUTE SECOND INVARIANT
C
         VARJ2 =  DEVIA(4)*DEVIA(4)+DEVIA(5)*DEVIA(5)+DEVIA(6)*DEVIA(6)
     .         +  HALF*(DEVIA(1)*DEVIA(1) + DEVIA(2)*DEVIA(2) +
     .            DEVIA(3)*DEVIA(3))
C
C----    COMPUTE THE THIRD INVARIANT
C
         VARJ3 =DEVIA(1)*DEVIA(2)*DEVIA(3) - DEVIA(1)*DEVIA(4)*DEVIA(4)
     .        - DEVIA(2)*DEVIA(5)*DEVIA(5) - DEVIA(3)*DEVIA(6)*DEVIA(6)
     .        + TWO * (DEVIA(4)*DEVIA(5)*DEVIA(5))
      endif
C
      return
      END

c*******************************************************************************
c*****************    coord_conversion  ****************************************
c*******************************************************************************
c** subroutine to convert the position from global to local (fault i referentiel)
c**                     coordinate system
c*******************************************************************************
c INPUT: xgg, ygg (global coordinate)
c        xs,ys,xf,yf,top,bottom,dip (fault parameters)
c           xgg, ygg are matrix files. The others are scalar.
c
c OUTPUT: xn, yn (position for the fault, 0,0 should be at the center
c                 of the fault)
c           all are matrix files.

        subroutine coord_conversion (xgg,ygg,xs,ys,xf,yf,
     .                            top,bottom,dip,xn,yn,al,aw)
c


        implicit real*8(a-h,o-z)
c


        dimension ss(6),sn(6),rr(3,3),sn9(3,3),
     .              sn10(3,3),t(6,6)

        pi = acos(-1.0)
        radian = 180. / pi
        degre  = pi / 180.


        cx = (xf+xs)/2.0
        cy = (yf+ys)/2.0
        h = (bottom-top)/2.0

        ak = tan(degre*dip)
        if(ak.eq.0) ak = 0.000001

        d = h/ak
        b = atan((yf-ys)/(xf-xs))

        ydipshift = abs(d * cos(b))
        xdipshift = abs(d * sin(b))


        !here cx, cy is the center position of the fault projection
        !on the global coordinate
        if(xf.gt.xs) then
            if(yf.gt.ys) then
        cx = cx + xdipshift
        cy = cy - ydipshift
            else
        cx = cx - xdipshift
        cy = cy - ydipshift
            endif
        else
            if(yf.gt.ys) then
        cx = cx + xdipshift
        cy = cy + ydipshift
            else
        cx = cx - xdipshift
        cy = cy + ydipshift
            endif
        endif
        ! converting global coordinate to Okada-fault coordinate
        ! xn and yn are the x, y position on the Okada coordinate
        xn = (xgg-cx)*cos(b)+(ygg-cy)*sin(b)
        yn =-(xgg-cx)*sin(b)+(ygg-cy)*cos(b)
        if(xf-xs.lt.0.0) then
            xn = -xn
            yn = -yn
        endif


        ! al is fault length, aw is fault width
        al = sqrt((xf-xs)*(xf-xs)+(yf-ys)*(yf-ys))/2.0
        aw = ((bottom-top)/2.0)/(sin(degre*dip))


        return
        end



c*****************************************************************
c*****************    calc_coulomb  *********************************
c*****************************************************************
c** subroutine to calculate the slip vector from a azimuth, dip and rake measure

        subroutine calc_coulomb (strike_m,dip_m,rake_m,friction_m,
     .                            ss,shear,normal,coulomb)
c

        implicit real*8(a-h,o-z)
c


        dimension ss(6),sn(6),rr(3,3),sn9(3,3),
     .              sn10(3,3),t(6,6)

        !write(*,*)'calc coulomb'

        pi = acos(-1.0)
        radian = 180. / pi
        degre  = pi / 180.

![shear,normal,coulomb] = calc_coulomb(strike_m,dip_m,rake_m,friction_m...
!,ss)
!% For calculating shear, normal and Coulomb stresses in a given
!% fault strike, dip, and rake
!%
!% INPUT: strike_m,dip_m,rake_m,friction_m,SXX,SYY,SZZ,SXY,SXZ,SYZ
!%
!% OUTPUT: shear,normal,coulomb
!%

!% "_m" means matrix. So, for "if" condition, we can only use scalar.
!% So here we convert it into scalar since there are the all same numbers in
!% a matrix.

!n = size(strike_m,1);

!strike = zeros(n,1);
!dip    = zeros(n,1);
!rake   = zeros(n,1);
!friction = friction_m(1,1);

        friction = friction_m
!% adjustment for our coordinate system from Aki & Richards convension
!c1 = strike_m >= 180.0;
c2 = strike_m < 180.0;
        c1 = .0
        c2 = .0
        if(strike_m .ge. 180.0) c1 = 1.0
        if(strike_m .lt. 180.0) c2 = 1.0

!strike = (strike_m - 180.0) .* c1 + strike_m .* c2;
!dip    = (-1.0) * dip_m .* c1 + dip_m .* c2;
!rake_m   = rake_m - 90.0;
        strike = (strike_m - 180.0) * c1 +
     .              strike_m * c2;
        dip    = (-1.0) * dip_m * c1 + dip_m * c2
        rake_m   = rake_m - 90.0

!c1 = rake_m <= -180.0; c2 = rake_m > -180.0;
        c1 = .0
        c2 = .0
        if(rake_m .le. -180.0) c1 = 1.0
        if(rake_m .gt. -180.0) c2 = 1.0
!rake = (360.0 + rake_m) .* c1 + rake_m .* c2;
        rake = (360.0 + rake_m) * c1
     .                  + rake_m * c2

!% CAUTION.....................................
!strike = deg2rad(strike);
!dip = deg2rad(dip);
!rake = deg2rad(rake);
        strike = degre*strike
        dip = degre*dip
        rake = degre*rake
!        write(*,*)'strike,dip,rake,friction',
!     .          strike,dip,rake,friction
!% % for rake rotation (this was fixed by Zhang ZQ)
!for i=1:n
!rsc = -rake(i,1); % flipped
!rr = makehgtform('xrotate',rsc);
!mtran(1:3,1:3,i) = rr(1:3,1:3);
!end
        rsc = -rake
        ! rotation matrix
        rr(1,1) = 1.0
        rr(1,2) = .0
        rr(1,3) = .0

        rr(2,1) = .0
        rr(2,2) = cos(rake)
        rr(2,3) = -sin(rake)

        rr(3,1) = .0
        rr(3,2) = sin(rake)
        rr(3,3) = cos(rake)

!        write(*,*)'rr1', rr(1,:)
!        write(*,*)'rr2', rr(2,:)
!        write(*,*)'rr3', rr(3,:)
!% Now corrected scalar value is to matrix (n x 1) as "...m"
!% strikem = zeros(n,1) + strike;
!% dipm = zeros(n,1) + dip;
        rakem = rake

        sn  = .0
        sn9 = .0

        ver = pi/2.0

!c1 = strike>=0.0;  c2 = strike<0.0; c3 = strike<=ver; c4 = strike>ver;
        c1 = .0
        c2 = .0
        c3 = .0
        c4 = .0
        if(strike .ge. 0.0) c1 = 1.0
        if(strike .lt. 0.0) c2 = 1.0
        if(strike .le. ver) c3 = 1.0
        if(strike .gt. ver) c4 = 1.0

!c24 = c2 + c4; cc24 = c24 > 0;
        c24 = c2 + c4
        cc24 = .0
        if(c24 .gt. 0.0) cc24 = 1.0

!d1 = dip>=0.0; d2 = dip<0.0;
        d1 = .0
        d2 = .0
        if(dip .ge. .0) d1 = 1.0
        if(dip .lt. .0) d2 = 1.0

!xbeta = (-1.0)*strike .* d1 + (pi - strike) .* d2;
        xbeta = (-1.0) * strike * d1
     .              + (pi - strike) * d2
!ybeta = (pi-strike).*d1 + (-1.0)*strike.*d2;
        ybeta = (pi-strike) * d1
     .          + (-1.0) * strike *d2
!zbeta = (ver-strike).*d1 + ((-1.0)*ver-strike).*d2.*c1.*c3 + (pi+ver-strike).*d2.*cc24;
        zbeta = (ver-strike) * d1
     .      + ((-1.0)*ver-strike)* d2 * c1 *c3
     .          + (pi+ver-strike) *d2 *cc24
!xdel = ver - abs(dip);
        xdel = ver - abs(dip)
!ydel = abs(dip);
        ydel = abs(dip)
!zdel = 0.0;
        zdel = .0

!% scalar to matrix (n x 1)
        xbetam = xbeta
        ybetam = ybeta
        zbetam = zbeta
        xdelm  = xdel
        ydelm  = ydel
        zdelm  = zdel

!        write(*,*)'xbetam',xbetam
!        write(*,*)'ybetam',ybetam
!        write(*,*)'zbetam',zbetam
!        write(*,*)'xdelm',xdelm
!        write(*,*)'ydelm',ydelm
!        write(*,*)'zdelm',zdelm
!xl = cos(xdelm) .* cos(xbetam);
!xm = cos(xdelm) .* sin(xbetam);
!xn = sin(xdelm);
!yl = cos(ydelm) .* cos(ybetam);
!ym = cos(ydelm) .* sin(ybetam);
!yn = sin(ydelm);
!zl = cos(zdelm) .* cos(zbetam);
!zm = cos(zdelm) .* sin(zbetam);
!zn = sin(zdelm);

        xl = cos(xdelm) * cos(xbetam)
        xm = cos(xdelm) * sin(xbetam)
        xn = sin(xdelm)
        yl = cos(ydelm) * cos(ybetam)
        ym = cos(ydelm) * sin(ybetam)
        yn = sin(ydelm)
        zl = cos(zdelm) * cos(zbetam)
        zm = cos(zdelm) * sin(zbetam)
        zn = sin(zdelm)
!        write(*,*)'xl',xl
!        write(*,*)'xm',xm
!        write(*,*)'xn',xn
!        write(*,*)'yl',yl
!        write(*,*)'ym',ym
!        write(*,*)'yn',yn
!        write(*,*)'zl',zl
!        write(*,*)'zm',zm
!        write(*,*)'zn',zn

        t(1,1) = xl * xl
        t(1,2) = xm * xm
        t(1,3) = xn * xn
        t(1,4) = 2.0 * xm * xn
        t(1,5) = 2.0 * xn * xl
        t(1,6) = 2.0 * xl * xm
        t(2,1) = yl * yl
        t(2,2) = ym * ym
        t(2,3) = yn * yn
        t(2,4) = 2.0 * ym * yn
        t(2,5) = 2.0 * yn * yl
        t(2,6) = 2.0 * yl * ym
        t(3,1) = zl * zl
        t(3,2) = zm * zm
        t(3,3) = zn * zn
        t(3,4) = 2.0 * zm * zn
        t(3,5) = 2.0 * zn * zl
        t(3,6) = 2.0 * zl * zm
        t(4,1) = yl * zl
        t(4,2) = ym * zm
        t(4,3) = yn * zn
        t(4,4) = ym * zn + zm * yn
        t(4,5) = yn * zl + zn * yl
        t(4,6) = yl * zm + zl * ym
        t(5,1) = zl * xl
        t(5,2) = zm * xm
        t(5,3) = zn * xn
        t(5,4) = xm * zn + zm * xn
        t(5,5) = xn * zl + zn * xl
        t(5,6) = xl * zm + zl * xm
        t(6,1) = xl * yl
        t(6,2) = xm * ym
        t(6,3) = xn * yn
        t(6,4) = xm * yn + ym * xn
        t(6,5) = xn * yl + yn * xl
        t(6,6) = xl * ym + yl * xm
!        write(*,*)'t1',t(1,:)
!        write(*,*)'t2',t(2,:)
!        write(*,*)'t3',t(3,:)
!        write(*,*)'t4',t(4,:)
!        write(*,*)'t5',t(5,:)
!        write(*,*)'t6',t(6,:)

!        write(*,*)'ss',ss(:)

        do i=1,6
            do j=1,6
        sn(i) = sn(i) + t(i,j)*ss(j)
            enddo
        enddo

!        write(*,*)'sn',sn(:)
!for k = 1:n
!sn(:,k) = t(:,:,k) * ss(:,k);
!sn9(1,1,k) = sn(1,k);
        sn9(1,1) = sn(1)
!sn9(1,2,k) = sn(6,k);
        sn9(1,2) = sn(6)
!sn9(1,3,k) = sn(5,k);
        sn9(1,3) = sn(5)
!sn9(2,1,k) = sn(6,k);
        sn9(2,1) = sn(6)
!sn9(2,2,k) = sn(2,k);
       sn9(2,2) = sn(2)
!sn9(2,3,k) = sn(4,k);
       sn9(2,3) = sn(4)
!sn9(3,1,k) = sn(5,k);
       sn9(3,1) = sn(5)
!sn9(3,2,k) = sn(4,k);
       sn9(3,2) = sn(4)
!sn9(3,3,k) = sn(3,k);
       sn9(3,3) = sn(3)
!sn9(:,:,k) = sn9(:,:,k) * mtran(:,:,k);

      DO I=1,3
         DO J=1,3
            sn10(I,J) = sn9(I,1)*rr(1,J)
     .                 + sn9(I,2)*rr(2,J)
     .                 + sn9(I,3)*rr(3,J)
         END DO
      END DO

!end
        shear = sn10(1,2)
        normal = sn10(1,1)
        coulomb = shear + friction * normal
!        write(*,*)'shear,normal,coulomb',shear
!     .              ,normal,coulomb
!shear   = reshape(sn9(1,2,:),n,1);
!normal  = reshape(sn9(1,1,:),n,1);
!coulomb = shear + friction .* normal;
        return
        end




c*****************************************************************
c*****************************************************************
c*****************    tensor_trans  *********************************
c*****************************************************************
c** subroutine to put the stress tensor in the global referentiel
!% Tensor transformation
!%
!%  INPUT:  sinb,so,sn,flag
!%            so(1x6) matix: original stress tensor
!%               --> sxx,syy,szz,syz,sxz,sxy
!%  OUTPUT:   sn(1x6) matix: newly calculated stress tensor with
!%                         horizontal rotation (not plunge change)
!%

        subroutine tensor_trans(sinb,cosb,s0,sn)

        implicit real*8(a-h,o-z)
        dimension   t(6,6), s0(6), sn(6)

        radian = 180. / pi
        degre  = pi / 180.
        pi = acos(-1.0)
        ver = pi/2.0

        bt = asin(sinb)

        if(cosb.gt.0.0) then
            xbeta = -bt
            xdel = 0.0
            ybeta = -bt + ver
            ydel = 0.0
            zbeta = -bt - ver
            zdel = ver
        else
            xbeta = bt - pi
            xdel = 0.0
            ybeta = bt - ver
            ydel = 0.0
            zbeta = bt - ver
            zdel = ver
        endif

        xl = cos(xdel) * cos(xbeta)
        xm = cos(xdel) * sin(xbeta)
        xn = sin(xdel)

        yl = cos(ydel) * cos(ybeta)
        ym = cos(ydel) * sin(ybeta)
        yn = sin(ydel)

        zl = cos(zdel) * cos(zbeta)
        zm = cos(zdel) * sin(zbeta)
        zn = sin(zdel)

        t(1,1) = xl * xl
        t(1,2) = xm * xm
        t(1,3) = xn * xn
        t(1,4) = 2.0 * xm * xn
        t(1,5) = 2.0 * xn * xl
        t(1,6) = 2.0 * xl * xm
        t(2,1) = yl * yl
        t(2,2) = ym * ym
        t(2,3) = yn * yn
        t(2,4) = 2.0 * ym * yn
        t(2,5) = 2.0 * yn * yl
        t(2,6) = 2.0 * yl * ym
        t(3,1) = zl * zl
        t(3,2) = zm * zm
        t(3,3) = zn * zn
        t(3,4) = 2.0 * zm * zn
        t(3,5) = 2.0 * zn * zl
        t(3,6) = 2.0 * zl * zm
        t(4,1) = yl * zl
        t(4,2) = ym * zm
        t(4,3) = yn * zn
        t(4,4) = ym * zn + zm * yn
        t(4,5) = yn * zl + zn * yl
        t(4,6) = yl * zm + zl * ym
        t(5,1) = zl * xl
        t(5,2) = zm * xm
        t(5,3) = zn * xn
        t(5,4) = xm * zn + zm * xn
        t(5,5) = xn * zl + zn * xl
        t(5,6) = xl * zm + zl * xm
        t(6,1) = xl * yl
        t(6,2) = xm * ym
        t(6,3) = xn * yn
        t(6,4) = xm * yn + ym * xn
        t(6,5) = xn * yl + yn * xl
        t(6,6) = xl * ym + yl * xm
        !write(*,*)'sinb,cosb',sinb,cosb
        !write(*,*)'t',t(1,:)
        ! write(*,*)'t',t(2,:)
        ! write(*,*)'t',t(3,:)
        ! write(*,*)'t',t(4,:)
        ! write(*,*)'t',t(5,:)
        ! write(*,*)'t',t(6,:)

        do i=1,6
        sn(i) = t(i,1)*s0(1)+t(i,2)*s0(2)
     .          +t(i,3)*s0(3) +t(i,4)*s0(4)
     .          +t(i,5)*s0(5) +t(i,6)*s0(6)
        enddo


        return
        end



