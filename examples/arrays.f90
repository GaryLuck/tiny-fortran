PROGRAM ARRAYS
    INTEGER :: ARR(10), I, SUM, N, TEMP

    N = 10

    ! Fill array with squares
    DO I = 1, N
        ARR(I) = I * I
    END DO

    ! Print array elements
    PRINT *, "Squares from 1 to 10:"
    DO I = 1, N
        PRINT *, ARR(I)
    END DO

    ! Sum the array
    SUM = 0
    DO I = 1, N
        SUM = SUM + ARR(I)
    END DO
    PRINT *, "Sum of squares:", SUM

    ! Reverse the array in-place
    DO I = 1, N / 2
        TEMP = ARR(I)
        ARR(I) = ARR(N - I + 1)
        ARR(N - I + 1) = TEMP
    END DO

    PRINT *, "Reversed:"
    DO I = 1, N
        PRINT *, ARR(I)
    END DO

    ! Use array element in expression
    PRINT *, "ARR(1) + ARR(10) =", ARR(1) + ARR(10)

    ! Use MOD with array element
    PRINT *, "MOD(ARR(3), 5) =", MOD(ARR(3), 5)

END PROGRAM
