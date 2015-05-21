module geometry
  !! Provides routines for basic geometrical and trigonometric functions in 2D and 3D.
  
  implicit none

  public :: distance
  
contains

  pure real function distance(x0, y0, z0, x1, y1, z1) result(d)

    implicit none

    real, intent(in) :: x0, x1, y0, y1
    real, intent(in), optional :: z0, z1
    
    if (present(z0) .and. present(z1)) then
       d = sqrt((x1-x0)**2 + (y1-y0)**2 + (z1-z0)**2)
    else
       d = sqrt((x1-x0)**2 + (y1-y0)**2)
    end if
    
  end function distance
end module geometry
