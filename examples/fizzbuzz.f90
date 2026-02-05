PROGRAM FIZZBUZZ
  INTEGER :: I
  DO I = 1, 30
    IF (MOD(I, 15) == 0) THEN
      PRINT *, "FizzBuzz"
    ELSE IF (MOD(I, 3) == 0) THEN
      PRINT *, "Fizz"
    ELSE IF (MOD(I, 5) == 0) THEN
      PRINT *, "Buzz"
    ELSE
      PRINT *, I
    END IF
  END DO
END PROGRAM FIZZBUZZ
