! Self-checking execution tests for the AT edit descriptor with internal
! writes, FORMAT statements, and additional scenarios
! (Fortran 2023 Standard, 13.7.2).
!
! Each test compares actual formatted output against an expected string
! and prints PASS or FAIL. The reference output is just PASS lines.
program at_edit02
  implicit none
  character(10) :: s10
  character(5)  :: s5
  character(50) :: buffer
  character(80) :: got
  character(:), allocatable :: alloc_str
  character(kind=2, len=10) :: s2
  character(kind=4, len=10) :: s4
  integer :: npass, nfail

  npass = 0
  nfail = 0

  ! Test 1: AT with internal write
  s10 = "hello"
  s5 = "world"
  write(buffer, '(AT,1X,AT)') s10, s5
  call check("internal write", buffer, "hello world")

  ! Test 2: AT with FORMAT statement
  s10 = "fmt"
100 format("[",AT,"]")
  write(got, 100) s10
  call check("FORMAT stmt", got, "[fmt]")

  ! Test 3: 3AT repeat count with three items
  s10 = "xx"
  s5 = "yyy"
  write(got, '("[",3AT,"]")') s10, s5, "z   "
  call check("3AT repeat", got, "[xxyyyz]")

  ! Test 4: AT with tab position descriptor
  s10 = "after tab"
  write(got, '(T5,AT)') s10
  call check("AT with T5", got, "    after tab")

  ! Test 5: AT with character substring
  s10 = "abcdefghij"
  write(got, '("[",AT,"]")') s10(1:5)
  call check("substring", got, "[abcde]")

  ! Test 6: AT with allocatable character
  alloc_str = "allocated"
  write(got, '("[",AT,"]")') alloc_str
  call check("allocatable", got, "[allocated]")
  deallocate(alloc_str)

  ! Test 7: Internal write with all blanks
  s10 = "    "
  write(buffer, '("[",AT,"]")') s10
  call check("internal blanks", buffer, "[]")

  ! Test 8: AT with various literal lengths
  write(got, '("[",AT,"]")') "a"
  call check("literal len 1", got, "[a]")
  write(got, '("[",AT,"]")') "ab"
  call check("literal len 2", got, "[ab]")
  write(got, '("[",AT,"]")') "abc"
  call check("literal len 3", got, "[abc]")

  ! Test 9: AT with allocatable with trailing blanks
  alloc_str = "padded     "
  write(got, '("[",AT,"]")') alloc_str
  call check("alloc trailing blanks", got, "[padded]")
  deallocate(alloc_str)

  ! Test 10: AT in internal write then read back
  s10 = "round"
  write(buffer, '(AT)') s10
  call check("internal round-trip", buffer, "round")

  ! Test 11: AT with kind=2 character
  s2 = 2_"hello"
  write(got, '("[",AT,"]")') s2
  call check("kind=2 trim", got, "[hello]")

  ! Test 12: AT with kind=4 character
  s4 = 4_"hello"
  write(got, '("[",AT,"]")') s4
  call check("kind=4 trim", got, "[hello]")

  ! Test 13: AT with kind=4 all blanks
  s4 = 4_" "
  write(got, '("[",AT,"]")') s4
  call check("kind=4 blanks", got, "[]")

  ! Test 14: AT with kind=2 leading blanks preserved
  s2 = 2_"  hi"
  write(got, '("[",AT,"]")') s2
  call check("kind=2 leading blanks", got, "[  hi]")

  ! Test 15: AT with kind=4 internal blanks preserved
  s4 = 4_"a b c"
  write(got, '("[",AT,"]")') s4
  call check("kind=4 internal blanks", got, "[a b c]")

  ! Summary
  write(*,'(A,I0,A,I0,A)') "Result: ", npass, " passed, ", nfail, " failed"
  if (nfail > 0) stop 1

contains
  subroutine check(name, actual, expected)
    character(len=*), intent(in) :: name, actual, expected
    if (trim(actual) == trim(expected)) then
      write(*,'(A,A)') "PASS: ", name
      npass = npass + 1
    else
      write(*,'(A,A)') "FAIL: ", name
      write(*,'(A,A,A)') "  Expected: [", trim(expected), "]"
      write(*,'(A,A,A)') "  Got:      [", trim(actual), "]"
      nfail = nfail + 1
    end if
  end subroutine

end program at_edit02
