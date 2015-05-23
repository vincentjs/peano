module error
  !! Development tool used to add and print error messages, as well as handle logic flow in the event an error is thrown. 

  implicit none

  private

  character(len=1024) :: error_list(1:100)
  integer :: err_i = 0
  
contains

  subroutine add_error_message(err_s)

    character(len=*), intent(in) :: err_s

    err_i = err_i + 1
    error_list(err_i) = "Error: " // trim(err_s)

  end subroutine add_error_message

  subroutine print_error_list_to_shell()

    integer :: i, l, u

    l = lbound(error_list,1)
    u = ubound(error_list,1)

    do i = l, u
       write(6,*) error_list(i)
    end do

  end subroutine print_error_list_to_shell

  subroutine terminate_with_failure()
    error stop 1
  end subroutine terminate_with_failure
  
end module error
    
