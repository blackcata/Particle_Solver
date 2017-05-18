!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : Particle_read.f90                                                !
!                                                                              !
!   PURPOSE : Reading datas from the DNS data for turbulent channel flow.      !
!                                                                              !
!                                                             2017.05.17 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        SUBROUTINE READ_DNS
          USE mesh
          USE field
          USE fileout

          IMPLICIT NONE

          INTEGER :: i,j,k
          REAL(KIND=8) :: tmp_x, tmp_y, tmp_z, time_sta, time_end
          CHARACTER(20) :: header

          WRITE(*,*) '----------------------------------------------------'
          WRITE(*,*) '              READING PROCESS STARTED               '
          CALL CPU_TIME(time_sta)

          dirname  = 'Data'
          filename = 'INSU_XYZ.plt'
          pathname = TRIM(dirname)//'/'//TRIM(filename)

          OPEN(100,FILE=pathname,FORM='FORMATTED',STATUS='OLD')
          READ(100,*) header
          READ(100,*) header

          !--------------------------------------------------------------------!
          !                   Main loop of reading DNS datas                   !
          !--------------------------------------------------------------------!
          DO k = 0,Nz
            DO j = 0,Ny
              DO i = 0,Nx
                READ(100,*) tmp_x, tmp_y, tmp_z,                              &
                            U(1,i,j,k), U(2,i,j,k), U(3,i,j,k)

                IF (j==1 .AND. k==1) x1(i)      = tmp_x
                IF (i==1 .AND. k==1) x2(j)      = tmp_y
                IF (i==1 .AND. j==1) x3(k)      = tmp_z
              END DO
            END DO
          END DO

          CLOSE(100)

          CALL CPU_TIME(time_end)
          WRITE(*,*) '            READING PROCESS IS COMPLETED            '
          WRITE(*,*) '  Total Reading time : ',time_end - time_sta,' s'
          WRITE(*,*) '----------------------------------------------------'
          WRITE(*,*) ''

        END SUBROUTINE READ_DNS
