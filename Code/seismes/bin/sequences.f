      program sequence
c
c---- recherche des sequences de seismes dans le fichier "recurr"
c
      dimension t(100000),num(100000),ktype(12),jtype(12),
     .          tabs(100000)
c
      open(10,file='recurr')
      tcour = 0
      do i=1,100000
         read(10,*,end=1000) t(i),num(i)
         tcour = tcour + t(i)
         tabs(i) = tcour
         imax = i
      enddo
1000  continue

      close(10)
c
      write(*,*)'imax =',imax
      iseqmax = imax / 3
      tmax = 100
c
      iseq = 0
      do k=1,12
         ktype(k) = 0
      enddo
      jtype(1) = 123
      jtype(2) = 132
      jtype(3) = 231
      jtype(4) = 213
      jtype(5) = 312
      jtype(6) = 321
      jtype(7) = 12
      jtype(8) = 13
      jtype(9) = 21
      jtype(10)= 23
      jtype(11)= 31
      jtype(12)= 32
      iseq3 = 0
      iseq2 = 0
      do i=2,imax-1
         t3 = t(i) + t(i+1)
         t2 = t(i)
         if (t3 .le. tmax) then
            iseq3 = iseq3 + 1
            icode3 = 100 * num(i-1) + 10 * num(i) + num(i+1)
            do k=1,6
               if (icode3 .eq. jtype(k)) then
                  ktype(k) = ktype(k) + 1
               endif
            enddo
            write(*,'(i5,3x,3I2,2x,f10.0,10x,2f5.0)') 
     .      iseq3,num(i-1),num(i),num(i+1),tabs(i),t(i),t(i)+t(i+1)
         endif
         if (t2 .le. tmax) then
            iseq2 = iseq2 + 1
            icode2 =  10 * num(i-1) + num(i)
            do k=7,12
               if (icode2 .eq. jtype(k)) then
                  ktype(k) = ktype(k) + 1
               endif
            enddo
            write(*,'(i5,3x,2I2,4x,f10.0,10x,2f5.0)') 
     .      iseq,num(i-1),num(i),tabs(i),t(i)
         endif
      enddo
c
      pcseq3 = 100.0 * iseq3 / iseqmax
      pcseq2 = 100.0 * iseq2 / iseqmax
      write(*,*)' nombre max de sequences        : ',iseqmax
      write(*,*)' nombre de sequences effectives (3): ',iseq3
      write(*,*)' nombre de sequences effectives (2): ',iseq2
      write(*,*)' pourcentage de sequences       : ',pcseq3
      write(*,*)' pourcentage de sequences       : ',pcseq2

      do k=1,12
         write(*,*)' type: ',jtype(k),'  nombre: ',ktype(k)
      enddo
c
      stop
      end
