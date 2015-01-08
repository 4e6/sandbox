# Sandbox

Various stuff.

### Build Go

    make
    
Executables are at `$GOPATH/bin`

### Build Haskell

    cabal sandbox init
    cabal install
    
Executables are at `.cabal-sandbox/bin`

### Build Scala

    sbt compile test
