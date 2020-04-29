       program program_julian_date
        implicit none
        integer op, A, M, D, N
        real*8 H, JD
        write (6,*) 'Selecione opcao'
        write (6,*) '1. Calendario -> JD'
        write (6,*) '2. JD -> Calendario'
        read (5,*) op
         if (op.eq.1) then
           write (6,*) 'Introduza ano, mes, dia, hora'
           read (5,*) A, M, D, H
           call calc_greg_to_jd (A, M, D, H, JD)
	         write (6,*) 'Data Juliada', JD
        else
           write (6,*) 'Introduza a Data Juliana'
           read (5,*) JD
           call calc_jd_to_greg (JD, A, M, D, N)
           write (6,*) 'Data Gregoriana', A, M, D, N
         endif
        end

        subroutine calc_greg_to_jd (A, M, D, UT1, JD)
        implicit none
        integer A, M, D, y, x
        real*8 JD, UT1
         if (m.le.2) then
           y=A-1
           x=M+12
         else
           y=A
           x=M
         endif
        JD=int(365.25*y)+int(30.6001*(x+1))+D+UT1/24+1720981.5
c        esta formula nao distingue entre as 12h e as 00h
c        JD=367*A-7*(A+(M+9)/12)/4+275*M/9+D+1721014
c        esta formula nao distingue entre as 12h e as 00h
c        JD=367*A-7*(A+(M+9)/12)/4-3*((A+(M-9)/7)/100+1)/4+275*M/9
c     .     +D+1721029
      end

        subroutine calc_jd_to_greg (JD, A, M, D, N)
        implicit none
        integer A, M, D, N, o, p, q, r, s
        real*8 JD, t
        o=INT(JD+0.5)
        p=o+1537
        q=INT((p-122.1)/365.25)
        r=INT(365.25*q)
        s=INT((p-r)/30.6001)
        t=(JD+0.5)-INT(JD+0.5)
        D=p-r-INT(30.6001*s)+t
        M=s-1-12*INT(s/14)
        A=q-4715-INT((7+M)/10)
        N=MOD(INT(JD+0.5),7)
        end
