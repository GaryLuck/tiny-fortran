INTEGER FUNCTION GCD(A, B)
  INTEGER, INTENT(IN) :: A, B
  INTEGER :: X, Y, TEMP
  X = ABS(A)
  Y = ABS(B)
  DO WHILE (Y /= 0)
    TEMP = Y
    Y = MOD(X, Y)
    X = TEMP
  END DO
  GCD = X
END FUNCTION GCD

PROGRAM GCD_DEMO
  INTEGER :: A, B, RESULT
  A = 48
  B = 18
  RESULT = GCD(A, B)
  PRINT *, "GCD(", A, ",", B, ") =", RESULT

  A = 100
  B = 75
  RESULT = GCD(A, B)
  PRINT *, "GCD(", A, ",", B, ") =", RESULT

  A = 17
  B = 13
  RESULT = GCD(A, B)
  PRINT *, "GCD(", A, ",", B, ") =", RESULT
END PROGRAM GCD_DEMO
