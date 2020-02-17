load ../../harness

@test "794bffc105c3" {
  check 'if (z     +   -1 <-3 -    Co)  then y     :=     0     +    y      else  z    :=    3     +    y    ' '⇒ z := (3+y), {}
⇒ skip, {z → 3}'
}
