module trapezoidal_rule
  use iso_fortran_env, only : int32, int64, real32, real64
  implicit none

  abstract interface
    ! Абстрактный интерфейс для подинтегральной функции
    pure function f(x)
      double precision, intent(in) :: x
      double precision :: f
    end function f
  end interface

  private

  public :: trapezoidal

  contains
  !----------------------------------------------------------------
  ! Функция вычисляющая значение интеграла по формуле трапеции
  ! Функция func --- подынтегральная функция от одного аргумента
  ! Параметры a и b задают пределы интегрирования
  ! Параметр n --- число точек разбиения отрезка [a, b]
  ! Параметра threads_num --- число потоков, которые может использовать
  ! функция
  !----------------------------------------------------------------
  function trapezoidal(func, a, b, n, threads_num) result (res)
    implicit none
    procedure(f) :: func
    real(real64), intent(in) :: a, b
    integer(int64), intent(in) :: n
    integer(int64), intent(in) :: threads_num
    real(real64) :: res

    real(real64) :: h ! шаг интегрирования
    integer(int64) :: i
    ! Здесь должен быть ваш код
  end function trapezoidal

end module trapezoidal_rule
