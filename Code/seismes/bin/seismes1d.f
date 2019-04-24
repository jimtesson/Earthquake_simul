      program seismes
c
c---- modele 1D de generation de seismes dans la croute faisant intervenir
c     pour chaque faille:
c
c     - une force de chargement externe (se) e pour external
c     - une loi de friction a deux coefficients (sc) c pour contact
c     - une force de couplage avec la croute moyenne (ss) s pour short range
c     - des forces de couplage interfailles (sl) l pour long range
c     Ces forces sont normalisees par une epaisseur et les calculs sont
c     effectues pour une epaisseur unite (Pa)
c
c     Le nombre de failles est nf
c
c     Les principaux parametres physiques utilises sont :
c
c     de : distance caracteristique du chargement externe
c     ds : distance caracteristique du couplage short range
c     cc0 : matrice de couplage cosismique long range (Pa/m)
c     cc1 : matrice de couplage postsismique long range (Pa/m)
c     frics : seuil tangentiel statique
c     fricd : seuil tangentiel dynamique
c     bruits : bruit sur le seuil statique
c     bruitd : bruit sur le seuil dynamique
c     e  : module d'Young
c     visco : viscosite crustale
c     taui : inverse du temps de relaxation de la croute moyenne
c
c     Les indices 0 referent a une valeur initiale
c
      implicit real*8 (a-h,o-z)
c
      real*4 rand,randomness
c
      parameter (nfx=10,nclassx=50)
      parameter (iin=10,iout=11,iplot=12,ierr=13)
      parameter (pi=3.141592)
c
      dimension se (nfx)    , sc (nfx), ss (nfx), 
     .          sl(nfx,nfx),dsl(nfx,nfx),
     .          se0(nfx), ss0(nfx)
      dimension de (nfx), ds (nfx), ucos(nfx)
      dimension xm (nfx), sep(nfx), slong(nfx), slongo(nfx)
      dimension frics(nfx),fricd(nfx),visco(nfx)
      dimension coefs(nfx),coefd(nfx),fricdr(nfx),fricsr(nfx)
c
c---- ajout 10 Aout 2000
c
      dimension fricdr0(nfx),fricsr0(nfx),ucos0(nfx),ucosnorm(nfx)
      dimension taui(nfx), tquake(nfx), tquake0(nfx)
      dimension quakes(nfx)
      dimension cc0(nfx,nfx),cc1(nfx,nfx)
      dimension kquakes(nfx)
      dimension ktrecur(nclassx)
c
      character char3*3!name*5,
      !character*6 filin,filout,filplot,filerr
      character (len=500) name,filename,filin,
     .              filout,filplot,filerr
c
      write(*,*) ' nom de l''essai ?'
      read(*,*) name
c
      !filin  (1:1) = 'i'
      !filin  (2:6) = name
      !filout (1:1) = 'o'
      !filout (2:6) = name
      !filplot(1:1) = 'p'
      !filplot(2:6) = name
      !filerr (1:1) = 'e'
      !filerr (2:6) = name

        ! input file
        name=trim(name)
        filin = trim('../input/i_')//trim(name)
        filin = trim(filin)
        ! out file
        name=trim(name)
        filout = trim('../output/o_')//trim(name)
        filout = trim(filout)
        ! plot file
        name=trim(name)
        filplot = trim('../output/p_')//trim(name)
        filplot = trim(filplot)
        ! err file
        name=trim(name)
        filerr = trim('../output/e_')//trim(name)
        filerr = trim(filerr)

        write(*,*)filin
        write(*,*)filout
        write(*,*)filplot
        write(*,*)filerr
c
      open(iin  ,file=filin)
      open(iout ,file=filout)
      open(iplot,file=filplot)
      open(ierr ,file=filerr)
      open(14 ,file='../output/quakes')
      open(16 ,file='../output/cosismique')
      open(17 ,file='../output/stress_c')
      open(18 ,file='../output/stress_l')
      open(19 ,file='../output/recurr')
c
      read (iin,*) iecho
      read (iin,*) tfin
      read (iin,*) dtime
      read (iin,*) tmin
      read (iin,*) tmax
      read (iin,*) nquakemax
      read (iin,*) istressini
      read (iin,*) tploti
      read (iin,*) nplotc
      read (iin,*) e
      read (iin,*) press
      read (iin,*) iscover
      read (iin,*) irelax
      read (iin,*) bruits
      read (iin,*) bruitd
      read (iin,*) nf
      do j=1,nf
        read (iin,*) sep(j),de(j),ds(j),xm(j),
     .               coefs(j),coefd(j),visco(j)
      enddo

        !read(*,*)char3 !skip line

        write(*,*)'Coseismic matrix:'
      do j=1,nf
         read (iin,*) (cc0(k,j),k=1,nf)
        write(*,*)(cc0(k,j),k=1,nf)
      enddo

        !read(*,*)char3 !skip line

         write(*,*)'Relaxed matrix:'
      do j=1,nf
         read (iin,*) (cc1(k,j),k=1,nf)
        write(*,*)(cc1(k,j),k=1,nf)
      enddo

        !read(*,*)char3 !skip line

      read (iin,*) (se0(j),j=1,nf)
      read (iin,*) (ss0(j),j=1,nf)
      read (iin,'(a3)') char3
      if (char3 .ne. 'END') then
         write(*,*)' erreur de lecture '
         write(*,'(a3)') char3
         stop
      endif
c
c---- initialisation
c
      do if=1,nfx
         kquakes(if) = 0
      enddo
      do in=1,nclassx
         ktrecur(in) = 0
      enddo
      do if=1,nf
         ucos    (if) = 0.0
         ucosnorm(if) = 0.0
         quakes(if) = 0.0
         tquake0(if) = 0.0
         de(if) = 1000 * de(if)
         ds(if) = 1000 * ds(if)
         frics(if)  = coefs(if) * press
         fricd(if)  = coefd(if) * press
         taui(if) = e / visco(if)
      enddo
c
      randomness = 0.0
      randoms = 0.5
      randomd = 0.5

      kbrand = 0
      tolerfric = 1.0e-6
      tolerstress = 0.1
      year   = 3600 * 24 * 365
      yeari  = 1.0 / year
      tfin   = tfin  * year
      tmin   = tmin  * year
      tmax   = tmax  * year
      dtime  = dtime * year
      grand = 1.0e33
      ucosmin = 1.e30
      ucosmax = 0.0
      time = 0.0
      ntime = tfin / dtime 
c
      if (iecho .ge. 1) then
         write(*,*) 'tfin  = ', yeari*tfin ,' years'
         write(*,*) 'dtime = ', yeari*dtime,' years'
      endif
c
c---- calcul des caracteristiques de la premiere faille isolee
c
      if = 1
c
      geom = de(if) / (ds(if) + de(if))
      deltaf = frics(if)-fricd(if)
      rigid = e * (1.0/de(if) + 1.0/ds(if))
      ucosref  = 2.0 * deltaf / rigid
      tcosref  = pi * sqrt(xm(if)/rigid )
      trecurr = 2.0 * deltaf / sep(if)
     .        * ds(if) / (ds(if) + de(if))
      if (iecho .ge. 0) then
         write(*,*) ' frics(1) MPa :', 1.e-6*frics(1)
         write(*,*) ' fricd(1)     :', 1.e-6*fricd(1)
         write(*,*) ' deltaf       :', 1.e-6*deltaf
         write(*,*) ' rigid       :', rigid
         write(*,*) ' facteur geometrique de la faille :', geom
         write(*,*) ' temps de relaxation :', 1./taui(if),
     .   '    (',1./taui(if)*yeari,' ans)'
         write(*,*) ' temps de recurrence (isole) :', trecurr,
     .   '    (',trecurr*yeari,' ans)'
         write(*,*) 'ucosref  :', ucosref
         write(*,*) 'tcosref  :', tcosref
         write(*,*) 'time ratio :', trecurr*taui(if)
      endif
c
      alpha   = 1.0 / (1.0 - exp(-trecurr*taui(if)))
      sigmas1 =        -2.0 * deltaf *  geom*alpha
      sigmae1 = frics(if) + 2.0 * deltaf * (geom*alpha - 1.0)
c
      sigmas0 = sigmas1 + 2.0 * deltaf * geom
      sigmae0 = sigmae1 + 2.0 * deltaf * geom * ds(if)/de(if)
c
      if (iecho .ge. 1) then
         write(*,*) ' contrainte s-range avant seisme :', 1.e-6*sigmas0
         write(*,*) ' contrainte e-range avant seisme :', 1.e-6*sigmae0
         write(*,*) ' contrainte s-range apres seisme :', 1.e-6*sigmas1
         write(*,*) ' contrainte e-range apres seisme :', 1.e-6*sigmae1
      endif
c
      param = 2.0 * (frics(if)-fricd(if))/ frics(if) 
     .      * de(if)/(ds(if) + de(if))
c
      tcrit = -1.0/taui(if) * log(1.0-param)
      tprem = frics(if) / sep(if)
      v500 =  ucosref / (500*year)

      if (iecho .ge. 0) then
         write(*,*) ' temps du premier seisme :',yeari*tprem
         write(*,*) ' vitesse de chargement :',sep(if)
         write(*,*) 
     .   ' chargement externe par an :',1.e-6*year*sep(if)
         write(*,*) 
     .   ' chargement externe par seisme :',1.e-6*trecurr*sep(if),' MPa'
         write(*,*) 'v500 :',v500
      endif
c
c---- initialisation des contraintes
c
      if (istressini .eq. 1) then
         do if=1,nf
            se(if) = se0(if)
            ss(if) = ss0(if)           
         enddo
      else
         do if=1,nf
            se(if) = 0.0
            ss(if) = 0.0
         enddo
      endif
      do if=1,nf
         slong(if) = 0.0
         do jf=1,nf
            sl (jf,if) = 0.0
         enddo
      enddo
      ucosmin = 1.e30
      ucosmax = 0.0
      iquake = 0
      tlastquake = 0.0
      nquake = 0
c
c---- ajout 10 Aout 2000: fricdr0 et fricsr0
c
      do if=1,nf
         ucos (if) = 0.0
         ucos0(if) = 0.0
         fricdr0(if) = press * coefd(if)
         fricsr0(if) = press * coefs(if)
         fricdr (if) = fricdr0(if)
         fricsr (if) = fricsr0(if)
      enddo

c
c---- iterations en temps
c
      do itime=1,ntime
         if (iecho .ge. 3) then
            write(*,*) ' '
            write(*,*) '*** itime,time',itime,time*yeari
         endif
c
c----    boucle sur les failles
c
        if (time.ge.tmin .and. time.le.tmax) then
        do if=1,nf
        sc(if) = - se(if) - ss(if) - slong(if)
        enddo
        write(17,'(10f15.3)') time*yeari,(-1.e-5*sc(if),if=1,nf)
        endif

         do if=1,nf
            if (iecho .ge. 4) then
               write(*,*) ' '
               write(*,*) 'fault number:',if
            endif
c
c----       calcul du statut cosismique ou intersismique
c
            if (irelax .eq. 1) then
               ss(if) = 0.0
            endif
cc            sltot = 0.0
cc            do jf=1,nf
cc               sltot = sltot + sl(jf,if)
cc            enddo
            sc(if) = - se(if) - ss(if) - slong(if)


            if (-sc(if) .ge. fricsr(if)) then
               iquake = 1
               se(if) = se(if) + slong(if)
            else
               iquake = 0
            endif
c
c----       cas cosismique
c           ==============
c
            if (iquake .eq. 1) then
               tquake(if) = time
               nquake = nquake + 1
               varrec = tquake(if) - tquake0(if) - trecurr*yeari
c
c----          ecritures...
c
               if (mod(nquake-1,100) .eq. 0) then
                  write(*,*) 'nquake = ', nquake
               endif
               if (iecho .ge. 3) then
                  write(*,*) 'varrec',if,tquake(if)-tquake0(if),
     .                       tquake(if)-tquake0(if)-trecurr*yeari,
     .                       tquake(2)-tquake(1)
               endif
c
cc               if (time.ge.tmin .and. time.le.tmax) then
                  write(19,'(f15.3,i5)') 
     .          (time-tlastquake)*yeari,if
cc               endif
               tlastquake = time
c
cc               if (time.ge.tmin .and. time.le.tmax) then
                  quakes(if) = float(if)
                  write(14,'(10f12.2)')
     .             time*yeari,(quakes(j),j=1,nf)
     			quakes(if) = 0.0
c                  quakes(if) = float(if) + 0.9
c                 write(14,'(10f12.2)')
c     .             time*yeari,(quakes(j),j=1,nf)
c                   quakes(if) = float(if)
c                  write(14,'(10f12.2)')
c     .             time*yeari,(quakes(j),j=1,nf)
cc               endif
c
c----          nouvelle friction dynamique
c
               krand = 0
               randomd = rand(krand)
               fricdr(if) = press * (coefd(if) + bruitd*(0.5-randomd))
c
               tinter = tquake(if) - tquake0(if)
               tquake0(if) = tquake(if)
               rigid = e * (1.0/de(if) + 1.0/ds(if))
               ucos (if) = 2.0 * (fricsr (if)-fricdr (if)) / rigid
               ucos0(if) = 2.0 * (fricsr0(if)-fricdr0(if)) / rigid
               if (ucos0(if) .ge. 0.01) then
                  ucosnorm(if) = ucos(if) / ucos0(if)
               endif
cc               write(*,*)'cos',if,  ucos (if),ucos0 (if)
               tiner =  sqrt(xm(if)/rigid)
               tineri = 1.0 / tiner
               tcos = pi * tiner
c
cc               if (time.ge.tmin .and. time.le.tmax) then
                  if (ucos(if) .gt. ucosmax) ucosmax = ucos(if)
                  if (ucos(if) .lt. ucosmin) ucosmin = ucos(if)
                  write(16,'(f15.3,2i10)') ucos(if),nquake,if
cc               endif
c
               if (iecho .ge. 1) then
                  write(*,*)' '
                  write(*,*)'FAILLE  ',if
                  write(*,*)'FRICTIONS',
     .            fricsr(if)/press,fricdr(if)/press
                  write(*,*)'COSISMIQUE at ',yeari*tquake(if),' years'
                  write(*,*)'SLIP ',ucos(if),' m ,',ucosnorm(if),' su'
               endif
c
c----          ecriture des contraintes durant la phase cosismique
c
               do jp=1,nplotc
                  dtplot = (jp-1) * tcos / max(1,nplotc-1)
                  uplot = (fricsr(if)-fricdr(if)) / rigid * 
     .                 (1.0-cos(dtplot*tineri))
                  sepp = se(if) - e / de(if) * uplot
                  sspp = ss(if) - e / ds(if) * uplot
                  sapp = (fricsr(if)-fricdr(if)) * cos(dtplot*tineri)
                  scpp = - sepp - sspp - sapp
                  write(iplot,'(i4,10e12.4)')
     .            if,time+dtplot,sepp,sspp,slpp,scpp,sapp,uplot
               enddo
c
c----          verification de la solution sur la friction
c
               sctest = sc(if) + 2.0 * (fricsr(if)-fricdr(if))
c
c----          modification des contraintes locales durant la phase cosismique
c
               se(if)    = se(if) - e / de(if) * ucos(if)
               ss(if)    = ss(if) - e / ds(if) * ucos(if)
               ss0(if)   = ss(if)
               sc(if)    = - se(if) - ss(if)
c
               accur =  abs(sctest-sc(if)) / ( abs(sctest)+abs(sc(if)) )
c
               if (accur .ge. tolerfric) then
                  write(*,*) 'erreur :  accur .ge. tolerfric'
                  write(*,*) 'sc(if) =', sc(if)
                  write(*,*) 'sctest =', sctest
                  stop
               endif
               if (iecho .ge. 3) then
                  write(*,*) 'cosref',ucos(if),ucos(if)-ucosref
               endif
c
c----          nouvelle friction statique
c
               krand = 0
               randoms = rand(krand)
               fricsr(if) = press * (coefs(if) + bruits*(0.5-randoms))
            else
c
c----          cas intersismique
c              =================
c
c----          modification des contraintes locales
c
               se(if) = se(if) + sep(if) * dtime
               if (irelax .eq. 0) then
                  tinter = time - tquake(if)
                  ss(if) = ss0(if) * exp(-tinter*taui(if))
               else
                  ss(if) = 0.0
               endif
               sc(if) = - se(if) - ss(if)
            endif
c
c-----      contraintes d'interaction provenant des autres failles
c           ======================================================
c

            if (iquake .eq. 1) then
c
c----          reset apres un cosismique
c
               do jf=1,nf
                  sl(if,jf) = 0.0
               enddo
            endif

            enddo
            do if=1,nf !!!! JIM
            do jf=1,nf
c
c----             modif 10 Aout 2000 : deplacement normalise ucos0
c
               sl0 = cc0(if,jf) * ucosnorm(jf)
               sl1 = cc1(if,jf) * ucosnorm(jf)
               tpost  = time  - tquake(jf)
               tposto = tpost - dtime
               if (tpost .lt. 0.01) then
                  dsl(if,jf) = sl0
           
               else
                  expo   =  exp(-tpost *taui(jf))
                  expoo  =  exp(-tposto*taui(jf))
                  dsl(if,jf) = (expoo-expo) * (sl1 - sl0)
               endif
               if (iecho .ge. 4) then
                  write(*,*) 'iquake :',iquake
                  write(*,*) 'jf,sl0,sl1,tpost,tposto'
                  write(*,*)  jf,1.e-5*sl0,1.e-5*sl1,yeari*tpost,
     .                        yeari*tposto
                  write(*,*) 'expo,dsl(jf,if)'
                  write(*,*)  expo,1.e-5*dsl(jf,if)
                  write(*,*) 'ucosnorm',ucosnorm
               endif                 
            enddo


c
c----       contrainte d'interaction
c
            slong(if) = 0.0
            do jf=1,nf
               sl(if,jf) = sl(if,jf) + dsl(if,jf)
               slong(if) = slong(if) + sl (if,jf)
            enddo
c
c----       ecritures...
c           ============
c
            if (iecho .ge. 4) then
                  write(*,*) 'STRESS at ',time*yeari, ' years'
                  write(*,*) 'if , se(if),ss(if),slong(if),sc(if)'
                  do ifw=1,nf
                     write(*,'(i4,10e17.8)') ifw,
     .                    1.e-5*se(ifw)   ,1.e-5*ss(ifw),
     .                    1.e-5*slong(ifw),1.e-5*sc(ifw)
                  enddo
                  write(*,*) 'sl'
                  do ifw=1,nf
                     write(*,'(i4,10e17.8)') ifw,
     .                    (1.e-5*sl(j,ifw),j=1,nf)
                  enddo
                  write(*,*) 'dsl'
                  do ifw=1,nf
                     write(*,'(i4,10e17.8)') ifw,
     .                    (1.e-5*dsl(j,ifw),j=1,nf)
                  enddo
            endif

c
cc            if (time.ge.tmin .and. time.le.tmax) then
cc               write(19,'(i5,10f15.3)') 
cc     .         if,tinter*yeari
cc               alpha =  tinter/trecurr*float(nclassx)+0.5
cc               ialpha = alpha
cc               iclass = min(ialpha,nclassx)
cc               iclass = max(iclass,1)
ccc               write(*,*) '**',tinter,trecurr,alpha,ialpha,iclass
cc               ktrecur(iclass) = ktrecur(iclass) + 1
cc            endif
         enddo    
c
c----    incrementation du temps et ecriture
c
         time = time + dtime
c
         if (iecho .ge. 3) then
            write(*,*) '  '
            write(*,*) 'time  = ',time*yeari, ' years'
            write(*,*) 'if , se(if),ss(if),slong(if),sc(if)'
            do if=1,nf
               write(*,'(i4,10e17.8)') if,
     .              1.e-5*se(if)   ,1.e-5*ss(if),
     .              1.e-5*slong(if),1.e-5*sc(if)
            enddo
         endif
         if (time.ge.tmin .and. time.le.tmax) then
            !write(17,'(10f15.3)') time*yeari,(-1.e-5*sc(if),if=1,nf)
            write(18,'(10f15.3)') time*yeari,(1.e-5*slong(if),if=1,nf)
         endif
c
         if (nquake .eq. nquakemax) goto 1234
      enddo
1234  continue
c
      close(iin)
      close(iout)
      close(iplot)
      close(ierr)
      close(14)
      close(16)
      close(17)
      close(18)
      close(19)
c
      write(*,*) '  '
      write(*,*) nquake ,' SEISMES'
      write(*,*) 'Umin ',ucosmin
      write(*,*) 'Umax ',ucosmax
      write(*,*) '  '
      write(*,*) ' TERMINAISON NORMALE '
      write(*,*) '  '
c
      stop
      end
c
c**************************************************************
c
      FUNCTION randx (R)
C
C***BEGIN PROLOGUE  RAND                                                        
C***PURPOSE  Generate a uniformly distributed random number.                    
C***LIBRARY   SLATEC (FNLIB)                                                    
C***CATEGORY  L6A21                                                             
C***TYPE      SINGLE PRECISION (RAND-S)                                         
C***KEYWORDS  FNLIB, RANDOM NUMBER, SPECIAL FUNCTIONS, UNIFORM                  
C***AUTHOR  Fullerton, W., (LANL)                                               
C***DESCRIPTION                                                                 
C                                                                               
C      This pseudo-random number generator is portable among a wide             
C variety of computers.  RAND(R) undoubtedly is not as good as many             
C readily available installation dependent versions, and so this                
C routine is not recommended for widespread usage.  Its redeeming               
C feature is that the exact same random numbers (to within final round-         
C off error) can be generated from machine to machine.  Thus, programs          
C that make use of random numbers can be easily transported to and              
C checked in a new environment.                                                 
C                                                                               
C      The random numbers are generated by the linear congruential              
C method described, e.g., by Knuth in Seminumerical Methods (p.9),              
C Addison-Wesley, 1969.  Given the I-th number of a pseudo-random               
C sequence, the I+1 -st number is generated from                                
C             X(I+1) = (A*X(I) + C) MOD M,                                      
C where here M = 2**22 = 4194304, C = 1731 and several suitable values          
C of the multiplier A are discussed below.  Both the multiplier A and           
C random number X are represented in double precision as two 11-bit             
C words.  The constants are chosen so that the period is the maximum            
C possible, 4194304.                                                            
C                                                                               
C      In order that the same numbers be generated from machine to              
C machine, it is necessary that 23-bit integers be reducible modulo             
C 2**11 exactly, that 23-bit integers be added exactly, and that 11-bit         
C integers be multiplied exactly.  Furthermore, if the restart option           
C is used (where R is between 0 and 1), then the product R*2**22 =              
C R*4194304 must be correct to the nearest integer.                             
C                                                                               
C      The first four random numbers should be .0004127026,                     
C .6750836372, .1614754200, and .9086198807.  The tenth random number           
C is .5527787209, and the hundredth is .3600893021 .  The thousandth            
C number should be .2176990509 .                                                
C                                                                               
C      In order to generate several effectively independent sequences           
C with the same generator, it is necessary to know the random number            
C for several widely spaced calls.  The I-th random number times 2**22,         
C where I=K*P/8 and P is the period of the sequence (P = 2**22), is             
C still of the form L*P/8.  In particular we find the I-th random               
C number multiplied by 2**22 is given by                                        
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8              
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0                  
C Thus the 4*P/8 = 2097152 random number is 2097152/2**22.                      
C                                                                               
C      Several multipliers have been subjected to the spectral test             
C (see Knuth, p. 82).  Four suitable multipliers roughly in order of            
C goodness according to the spectral test are                                   
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5                     
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5                             
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5                      
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1                               
C                                                                               
C      In the table below LOG10(NU(I)) gives roughly the number of              
C random decimal digits in the random numbers considered I at a time.           
C C is the primary measure of goodness.  In both cases bigger is better.        
C                                                                               
C                   LOG10 NU(I)              C(I)                               
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5                        
C                                                                               
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6                        
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7                        
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4                        
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6                        
C   Best                                                                        
C    Possible   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9                       
C                                                                               
C             Input Argument --                                                 
C R      If R=0., the next random number of the sequence is generated.          
C        If R .LT. 0., the last generated number will be returned for           
C          possible use in a restart procedure.                                 
C        If R .GT. 0., the sequence of random numbers will start with           
C          the seed R mod 1.  This seed is also returned as the value of        
C          RAND provided the arithmetic is done exactly.                        
C                                                                               
C             Output Value --                                                   
C RAND   a pseudo-random number between 0. and 1.                               
C                                                                               
C***REFERENCES  (NONE)                                                          
C***ROUTINES CALLED  (NONE)                                                     
C***REVISION HISTORY  (YYMMDD)                                                  
C   770401  DATE WRITTEN                                                        
C   890531  Changed all specific intrinsics to generic.  (WRB)                  
C   890531  REVISION DATE from Version 3.2                                      
C   891214  Prologue converted to Version 4.0 format.  (BAB)                    
C***END PROLOGUE  RAND                                                          
      SAVE IA1, IA0, IA1MA0, IC, IX1, IX0                                       
      DATA IA1, IA0, IA1MA0 /1536, 1029, 507/                                   
      DATA IC /1731/                                                            
      DATA IX1, IX0 /0, 0/                                                      
C***FIRST EXECUTABLE STATEMENT  RAND                                            
      IF (R.LT.0.) GO TO 10                                                     
      IF (R.GT.0.) GO TO 20                                                     
C                                                                               
C           A*X = 2**22*IA1*IX1 + 2**11*(IA1*IX1 + (IA1-IA0)*(IX0-IX1)          
C                   + IA0*IX0) + IA0*IX0                                        
C                                                                               
      IY0 = IA0*IX0                                                             
      IY1 = IA1*IX1 + IA1MA0*(IX0-IX1) + IY0                                    
      IY0 = IY0 + IC                                                            
      IX0 = MOD (IY0, 2048)                                                     
      IY1 = IY1 + (IY0-IX0)/2048                                                
      IX1 = MOD (IY1, 2048)                                                     
C                                                                               
 10   RAND = IX1*2048 + IX0                                                     
      RAND = RAND / 4194304.                                                    
      RETURN                                                                    
C                                                                               
 20   IX1 = MOD(R,1.)*4194304. + 0.5                                            
      IX0 = MOD (IX1, 2048)                                                     
      IX1 = (IX1-IX0)/2048                                                      
      GO TO 10                                                                  
C                                                                               
      END                                                                       
