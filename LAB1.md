# Лабораторная работа 1

Генералов Даниил, 1032212280

## Задание 1

В этой программе мы принимаем два значения: `half_queue_size` (соответствует значению N)
и `loop_counter` -- количество попыток.
Каждую попытку мы перемешиваем список, состоящий из `half_queue_size*2` элементов,
ровно половина из которых равна 1, а остальные равны -1.
Затем проверяем, что каждая сумма префикса не меньше 0:
если да, записываем эту попытку как успешную.
В конце вычисляем, сколько попыток были успешными,
и сравниваем это с тем, как предсказывает комбинаторная формула.

```f95
! NAME=lab1-1.f95
! CMDLINE=gfortran -o lab1-1.elf $FILE && ./lab1-1.elf

program monte_carlo_queue
    implicit none

    integer :: loop_counter
    integer :: half_queue_size
    integer :: success_counter
    integer :: current_score
    integer :: i
    integer :: j
    integer :: k
    integer, allocatable :: queue(:)

    write(*,*) "Enter N: "
    read(*,*) half_queue_size
    write(*,*) "Enter number of loops: "
    read(*,*) loop_counter

    ! Create the queue with half_queue_size elements equal to 1,
    ! and the rest equal to -1
    allocate(queue(half_queue_size*2))

    do i = 1, half_queue_size
        queue(i) = 1
    end do
    do i = half_queue_size + 1, half_queue_size*2
        queue(i) = -1
    end do

    success_counter = 0

    do i = 1, loop_counter
        ! Shuffle the queue
        do j = half_queue_size*2, 2, -1
            ! k is a random number between 1 and j
            k = int(rand() * j) + 1
            ! Swap the elements
            current_score = queue(j)
            queue(j) = queue(k)
            queue(k) = current_score
        end do

        current_score = 0

        ! Sum successive elements of the queue
        ! If the sum becomes negative, break
        ! If the sum remains non-negative, increment success_counter
        do j = 1, half_queue_size*2
            current_score = current_score + queue(j)
            if (current_score < 0) exit
        end do

        if (current_score >= 0) success_counter = success_counter + 1

    end do

    write(*,'(A, F15.10)') "Success rate: ", 1.0 * success_counter / loop_counter
    write(*,'(A, F15.10)') "Expected from math: ", 1.0 / (half_queue_size + 1.0)

end program monte_carlo_queue
```

(Чтобы запустить: `python3 run.py lab1-1.f95`)

## Задание 2

В этой программе принимается одно значение: `loop_counter` -- количество попыток.
Каждую попытку мы генерируем два случайных числа от 1 до 6 и складываем их.
Затем записываем, в каких случаях их сумма была равна 8,
в каких -- эта сумма была четной,
и в каких -- оба события произошли одновременно.
Чтобы посчитать вероятность, что выпало 8, если известно, что выпала четная сумма,
просто делим количество восьмерок на количество событий, когда сумма была четной.

```f95
! NAME=lab1-2.f95
! CMDLINE=gfortran -o lab1-2.elf $FILE && ./lab1-2.elf

program monte_carlo_dice
    implicit none

    integer :: loop_counter
    integer :: i
    integer :: j
    integer :: sum
    integer :: sum_is_eight_counter
    integer :: even_sum_counter
    integer :: both_events_counter
    integer :: eight_given_even_counter

    write(*,*) "Enter number of loops: "
    read(*,*) loop_counter

    sum_is_eight_counter = 0
    even_sum_counter = 0
    both_events_counter = 0
    eight_given_even_counter = 0

    do i = 1, loop_counter

        sum = 0
        do j = 1, 2
            sum = sum + int(rand() * 6) + 1
        end do

        if (sum == 8) sum_is_eight_counter = sum_is_eight_counter + 1
        if (mod(sum, 2) == 0) even_sum_counter = even_sum_counter + 1
        if (sum == 8 .and. mod(sum, 2) == 0) both_events_counter = both_events_counter + 1

    end do

    write(*,'(A, F15.10)') "Sum is 8: ", 1.0 * sum_is_eight_counter / loop_counter
    write(*,'(A, F15.10)') "Even sum: ", 1.0 * even_sum_counter / loop_counter
    write(*,'(A, F15.10)') "Both events: ", 1.0 * both_events_counter / loop_counter
    write(*,'(A, F15.10)') "Prob of 8 given even: ", 1.0 * sum_is_eight_counter / even_sum_counter

end program monte_carlo_dice
```

(Чтобы запустить: `python3 run.py lab1-2.f95`)

## Задание 3

В этой программе принимается одно значение: `loop_counter` -- количество попыток.
Каждую попытку перемешиваем колоду карт,
затем записываем, в каких случаях вторая карта была тузом,
и в каких она была тузом одновременно с первой.

```f95
! NAME=lab1-3.f95
! CMDLINE=gfortran -o lab1-3.elf $FILE && ./lab1-3.elf

program monte_carlo_card_shuffle
    implicit none

    character(len=5) :: deck(1:36) = [character(len=5) :: &
    "Т♣","К♣","Д♣","В♣","6♣","7♣","8♣","9♣","10♣",&
    "Т♠","К♠","Д♠","В♠","6♠","7♠","8♠","9♠","10♠",&
    "Т♦","К♦","Д♦","В♦","6♦","7♦","8♦","9♦","10♦",&
    "Т♥","К♥","Д♥","В♥","6♥","7♥","8♥","9♥","10♥" &
    ]
    integer :: loop_counter
    integer :: i
    integer :: j
    integer :: k
    character(len=5) :: card
    integer :: first_ace_counter
    integer :: second_ace_counter
    integer :: first_given_second_ace_counter
    logical :: first_ace
    logical :: second_ace

    write(*,*) "Enter number of loops: "
    read(*,*) loop_counter

    first_ace_counter = 0
    second_ace_counter = 0
    first_given_second_ace_counter = 0
    do i = 1, loop_counter

        do j = 36, 1, -1
            k = j + floor(rand() * (1 - j))
            card = deck(k)
            deck(k) = deck(j)
            deck(j) = card
        end do

        first_ace = .false.
        second_ace = .false.

        if (deck(1) == "Т♠") first_ace = .true.
        if (deck(1) == "Т♦") first_ace = .true.
        if (deck(1) == "Т♥") first_ace = .true.
        if (deck(1) == "Т♣") first_ace = .true.

        if (deck(2) == "Т♠") second_ace = .true.
        if (deck(2) == "Т♦") second_ace = .true.
        if (deck(2) == "Т♥") second_ace = .true.
        if (deck(2) == "Т♣") second_ace = .true.

        if (first_ace) first_ace_counter = first_ace_counter + 1
        if (second_ace) second_ace_counter = second_ace_counter + 1

        if (second_ace .and. first_ace) first_given_second_ace_counter = first_given_second_ace_counter + 1
    end do

    write(*,'(A, F15.10)') "First is ace: ", 1.0 * first_ace_counter / loop_counter
    write(*,'(A, F15.10)') "Second is ace: ", 1.0 * second_ace_counter / loop_counter
    write(*,'(A, F15.10)') "First given second ace: ", 1.0 * first_given_second_ace_counter / second_ace_counter
end program monte_carlo_card_shuffle
```

(Чтобы запустить: `python3 run.py lab1-3.f95`)

## Задание 4

```f95
! NAME=lab1-4-1.f95
! CMDLINE=gfortran -o lab1-4-1.elf $FILE -O3 -fopt-info-vec-all

program vectorize
    implicit none
    real, dimension(7) :: a, b
    a = [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
    b = [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0]

    print *, sum(a*b)
    print *, dot_product(a, b)
end program
```

При запуске `python3 run.py lab1-4-1.f95` можно посмотреть на сообщения компилятора,
которые сообщают о результатах векторизации -- 
процесса, когда компилятор замечает, что происходит какая-то одинаковая операция над несколькими элементами массива,
и заменяет операцию над каждым элементом на SIMD-инструкцию:
с помощью них можно, например, сложить два массива за гораздо меньшее количество инструкций,
что может снизить размер программы и повысить скорость вычисления.

В данном случае компилятор говорит, что он векторизовал два цикла в этой программе,
потому что цена выполнения этих операций векторно равна 24, а скалярно -- тоже 24,
поэтому векторизация не влияет на скорость вычисления и при этом дает более компактный машинный код.

Оптимизатор может понять, что эти операции можно векторизовать, даже если цикл написан явным образом:

```f95
! NAME=lab1-4-2.f95
! CMDLINE=gfortran -o lab1-4-2.elf $FILE -O3 -fopt-info-vec-all

program vectorize
    implicit none
    real, dimension(7) :: a, b
    real :: sum
    integer :: i
    a = [2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0]
    b = [4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0]

    sum = 0.0

    do i = 1, 7
        sum = sum + a(i)*b(i)
    end do
    print *, sum
end program
```

В этом случае компилятор дает такой же вывод о том, что он смог векторизовать этот цикл.

Более того, компилятор способен распознать векторную оптимизацию, даже если цикл производится по массиву неизвестного размера.
В этом случае компилятор работает над большей частью массива векторным способом, а если в конце остается несколько элементов,
то они, возможно, будут обработаны скалярным способом.

```f95
! NAME=lab1-4-3.f95
! CMDLINE=gfortran -o lab1-4-3.elf $FILE -O3 -fopt-info-vec-all

program vectorize
    implicit none
    real, allocatable :: a(:), b(:)
    real :: sum
    integer :: i
    integer :: j

    i = rand() * 100 + 1

    allocate(a(i))
    allocate(b(i))

    do j = 1, i
        a(j) = rand() * 10
        b(j) = rand() * 10
    end do

    sum = 0.0

    do j = 1, i
        sum = sum + a(j)*b(j)
    end do
    print *, sum
end program
```
