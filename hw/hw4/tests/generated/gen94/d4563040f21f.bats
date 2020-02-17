load ../../harness

@test "d4563040f21f" {
  check 'if (true ∨    x   +    -40169082442273333902908070117294718662 <     129911610439586058150531966667855883295 *     z) then skip else 
skip    ' '⇒ skip, {}'
}
