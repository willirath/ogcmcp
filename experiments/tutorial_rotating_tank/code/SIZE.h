CBOP
C    !ROUTINE: SIZE.h
C    !INTERFACE:
C    include SIZE.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | SIZE.h Declare size of underlying computational grid.
C     *==========================================================*
C     | Modified from tutorial_rotating_tank for nPx=2 (2 MPI processes).
C     | Nx = sNx*nSx*nPx = 30*2*2 = 120 (unchanged)
C     | Ny = sNy*nSy*nPy = 23*1*1 = 23  (unchanged)
C     \ev
CEOP
      INTEGER sNx
      INTEGER sNy
      INTEGER OLx
      INTEGER OLy
      INTEGER nSx
      INTEGER nSy
      INTEGER nPx
      INTEGER nPy
      INTEGER Nx
      INTEGER Ny
      INTEGER Nr
      PARAMETER (
     &           sNx =  30,
     &           sNy =  23,
     &           OLx =   3,
     &           OLy =   3,
     &           nSx =   2,
     &           nSy =   1,
     &           nPx =   2,
     &           nPy =   1,
     &           Nx  = sNx*nSx*nPx,
     &           Ny  = sNy*nSy*nPy,
     &           Nr  =  29)
      INTEGER MAX_OLX
      INTEGER MAX_OLY
      PARAMETER ( MAX_OLX = OLx,
     &            MAX_OLY = OLy )
