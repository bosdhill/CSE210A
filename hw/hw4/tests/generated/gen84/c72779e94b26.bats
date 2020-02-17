load ../../harness

@test "c72779e94b26" {
  check 'if (x    +     x     <    0     +  WX   ∨  y     =   q*z)  then   skip     else skip ' '⇒ skip, {}'
}
