PROGRAM findminimum
IMPLICIT NONE
REAL*8,ALLOCATABLE :: cv1(:,:), cv2(:,:), fes(:,:)
CHARACTER(50) ::filename1, filename2

REAL*8 :: dummy, minimum, min_cv, dummy2
REAL*8 :: x, y, min_cv1, min_cv2
REAL*8 :: Q_cv1, Q_cv2, Q_fes, P_cv1, P_cv2, P_fes, V_min
REAL*8 :: T_cv1, T_cv2, T_fes, S_cv1, S_cv2, S_fes, V_fes
REAL*8 :: R_cv1, R_cv2, R_fes, U_cv1, U_cv2, U_fes, V_cv1
REAL*8 :: S_min, T_min, U_min, P_min, Q_min, R_min, V_cv2

INTEGER :: i, nbin, nbin1, nbin2, ios, j, k, nsteps

!print*, "Enter INPUT File Name :  "
!read(*,*) filename1

filename1 = 'ANN_PBTASS_FES_phi1_phi2_50ns_100grid'

open(12,file=trim(filename1),IOSTAT=ios,STATUS="OLD")

open(13,file="Free_energy_wrt_0",IOSTAT=ios,STATUS="unknown")

CALL get_steps(12,nsteps)

PRINT*, 
PRINT*,"STEPS=",nsteps
PRINT*, 

nbin1 =100
nbin2 =100

ALLOCATE(cv1(nbin1,nbin2),cv2(nbin1,nbin2),fes(nbin1,nbin2))

DO i=1,nbin1
   DO j=1,nbin2
   read(12,*) cv1(i,j),cv2(i,j),fes(i,j)
   ENDDO
read(12,*)
ENDDO

!------------
! Global Minimum Q
minimum=fes(1,1)
min_cv1=cv1(1,1)
min_cv2=cv2(1,1)

! Minimum P
P_min=fes(1,1)
P_cv1=cv1(1,1)
P_cv2=cv2(1,1)

! Minimum R
R_min=fes(1,1)
R_cv1=cv1(1,1)
R_cv2=cv2(1,1)

! Minimum S
S_min=fes(1,1)
S_cv1=cv1(1,1)
S_cv2=cv2(1,1)

! Minimum T
T_min=fes(1,1)
T_cv1=cv1(1,1)
T_cv2=cv2(1,1)

! Minimum U
U_min=fes(1,1)
U_cv1=cv1(1,1)
U_cv2=cv2(1,1)

! Minimum V
V_min=fes(1,1)
V_cv1=cv1(1,1)
V_cv2=cv2(1,1)
!--------------------

DO i=1,nbin1
   DO j=1,nbin2

      ! Searching for minimum Q
      if(fes(i,j).lt.minimum) then
         minimum=fes(i,j)
         min_cv1=cv1(i,j)
         min_cv2=cv2(i,j)
      endif

     ! Searching for minima S
      if (((-1.70d0 <= cv1(i,j)).and.(cv1(i,j) <= -0.50d0)) .and. ((-3.140d0 <= cv2(i,j)).and.(cv2(i,j) <= -2.0d0))) then
        if (fes(i,j).lt.S_min)then
           S_min=fes(i,j)
           S_cv1=cv1(i,j)
           S_cv2=cv2(i,j)
        endif
      endif

! Searching for minimum U
      if (((-3.14d0 <= cv1(i,j)).and.(cv1(i,j) <= -2.0d0)) .and. ((-3.14d0 <= cv2(i,j)).and.(cv2(i,j) <= -2.0d0))) then
        if (fes(i,j).lt.U_min)then
           U_min=fes(i,j)
           U_cv1=cv1(i,j)
           U_cv2=cv2(i,j)
        endif
      endif
   
! Searching for minimum T
      if (((-3.14d0 <= cv1(i,j)).and.(cv1(i,j) <= -2.1d0)) .and. ((-2.0d0 <= cv2(i,j)).and.(cv2(i,j) <= -0.50d0))) then
        if (fes(i,j).lt.T_min)then
           T_min=fes(i,j)
           T_cv1=cv1(i,j)
           T_cv2=cv2(i,j)
        endif
      endif

! Searching for minimum V
      if (((0.50d0 <= cv1(i,j)).and.(cv1(i,j) <= 1.5d0)) .and. ((0.5d0 <= cv2(i,j)).and.(cv2(i,j) <= 1.50d0))) then
        if (fes(i,j).lt.V_min)then
           V_min=fes(i,j)
           V_cv1=cv1(i,j)
           V_cv2=cv2(i,j)
        endif
      endif

! Searching for minimum P
      if (((-1.7d0 <= cv1(i,j)).and.(cv1(i,j) <= -0.70d0)) .and. ((0.30d0 <= cv2(i,j)).and.(cv2(i,j) <= 1.30d0))) then
        if (fes(i,j).lt.P_min)then
           P_min=fes(i,j)
           P_cv1=cv1(i,j)
           P_cv2=cv2(i,j)
        endif
      endif

! Searching for minimum R
      if (((0.50d0 <= cv1(i,j)).and.(cv1(i,j) <= 1.3d0)) .and. ((-2.0d0 <= cv2(i,j)).and.(cv2(i,j) <= -1.0d0))) then
        if (fes(i,j).lt.R_min)then
           R_min=fes(i,j)
           R_cv1=cv1(i,j)
           R_cv2=cv2(i,j)
        endif
      endif
   END DO
ENDDO
!-------------------------------------------------!
 PRINT*, " Extracting All Minima :"
 PRINT*, " -----------------------"
  WRITE(*,1000) (min_cv1), (min_cv2), minimum-minimum 
  WRITE(*,2000) (P_cv1),   (P_cv2),   P_min-minimum
  WRITE(*,3000) (R_cv1),   (R_cv2),   R_min-minimum
  WRITE(*,4000) (S_cv1),   (S_cv2),   S_min-minimum
  WRITE(*,5000) (T_cv1),   (T_cv2),   T_min-minimum
  WRITE(*,6000) (U_cv1),   (U_cv2),   U_min-minimum
  WRITE(*,7000) (V_cv1),   (V_cv2),   V_min-minimum

1000 FORMAT (" Global Minimum [ Q ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
2000 FORMAT (" Local  Minimum [ P ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
3000 FORMAT (" Local  Minimum [ R ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
4000 FORMAT (" Local  Minimum [ S ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
5000 FORMAT (" Local  Minimum [ T ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
6000 FORMAT (" Local  Minimum [ U ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
7000 FORMAT (" Local  Minimum [ V ] : ",5X,"phi1:",F10.1,5X," phi2:",F10.1,5X," Free Energy:",F10.1)
  print*,  

!--------------------------------------------------!
! Rewrite by substracting by minimum
DO i=1,nbin1
   DO j=1,nbin2
   WRITE(13,*) cv1(i,j),cv2(i,j),fes(i,j)-minimum
   ENDDO
   WRITE(13,*)
ENDDO

 
close(12)
close(13)
DEALLOCATE (cv1,cv2,fes)

END PROGRAM


!-------------------------------------------!
SUBROUTINE get_steps(iunt,nsteps)
IMPLICIT NONE
INTEGER,INTENT(IN) ::iunt
INTEGER,INTENT(OUT) ::nsteps
INTEGER ::ios
nsteps=0
REWIND(iunt)
DO
READ(iunt,*,IOSTAT=ios)
IF(ios /= 0) EXIT
nsteps=nsteps+1
ENDDO
REWIND(iunt)
END SUBROUTINE get_steps


