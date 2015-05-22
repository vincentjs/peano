module assert
  !! Development tool used to ensure that predicates are as expected before proceeding. If an assertion fails, the error message will be logged and the program terminated.

  use error
  
  implicit none

  private
  
contains

  subroutine assert_same_rank(x,y)

    real, allocatable, intent(in) :: x(:), y(:)
    logical :: isSameRank

    character(len=:) :: err_s

    isSameRank = is_same_rank(x,y)

    if (.not.isSameRank) then
       err_s = "Assertion error - ranks are not equivalent."
       call add_error_message(err_s)
       call print_error_list_to_shell()
       call terminate_with_failure()
    end if
    
  end subroutine assert_same_rank
  
  pure logical function is_same_rank(x,y) result(res)

    real, allocatable, intent(in) :: x(:), y(:)

    if (size(x) == size(y)) then
       res = .true.
    else
       res = .false.
    
  end function assert_same_rank
  

end module assert
