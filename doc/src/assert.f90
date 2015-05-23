module assert
  !! Development tool used to ensure that predicates are as expected before proceeding. If an assertion fails, the error message will be logged and the program terminated.

  use error
  
  implicit none

  public

  interface assert_x_is_ge_y
     module procedure assert_x_is_ge_y_real, assert_x_is_ge_y_int
  end interface assert_x_is_ge_y
  
  
contains

  subroutine assert_x_is_ge_y_real(x,y)
    !! [Real] Asserts that input argument /(x/) is greater than or equal to input argument /(y/)
    real, intent(in) :: x, y
    character(len=256) :: err_s

    if (x < y) then
       err_s = "Assertion error - x is not greater than or equal to y."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if

  end subroutine assert_x_is_ge_y_real

  subroutine assert_x_is_ge_y_int(x,y)
    !! [Integer] Asserts that input argument /(x/) is greater than or equal to input argument /(y/)
    integer, intent(in) :: x, y
    character(len=256) :: err_s

    if (x < y) then
       err_s = "Assertion error - x is not greater than or equal to y."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if

  end subroutine assert_x_is_ge_y_int
  
  subroutine assert_x_is_gt_y(x,y)
    !! Asserts that input argument /(x/) is greater than input argument /(y/)
    real, intent(in) :: x, y
    character(len=256) :: err_s
    
    if (x <= y) then
       err_s = "Assertion error - x is not greater than y."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if

  end subroutine assert_x_is_gt_y
  
  subroutine assert_same_bounds(x,y)
    !! Asserts that input vector /(/mathbf{x}/) has the same lower and upper bounds as input vector /(/mathbf{y}/)
    real, allocatable, intent(in) :: x(:), y(:)
    character(len=256) :: err_s

    if (lbound(x,1) /= lbound(y,1) .or. ubound(x,1) /= ubound(y,1)) then
       err_s = "Assertion error - bounds do not begin or end at the same position."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if

  end subroutine assert_same_bounds
  
  subroutine assert_same_rank(x,y)
    !! Asserts that input vector /(/mathbf{x}/) has the same rank as input vector /(/mathbf{y}/)
    real, allocatable, intent(in) :: x(:), y(:)

    character(len=256) :: err_s

    if (size(x) /= size(y)) then
       err_s = "Assertion error - ranks are not equivalent."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if
    
  end subroutine assert_same_rank

end module assert
