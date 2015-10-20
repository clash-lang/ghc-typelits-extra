#Hacking
Download sources:
```
git clone https://github.com/christiaanb/ghc-typelits-extra.git
git clone https://github.com/clash-lang/ghc-tcplugins-extra.git
git clone https://github.com/clash-lang/ghc-typelits-natnormalise.git
```

Go to ghc-typelits-extra dir:
```
cd ghc-typelits-extra
```

Run:
```
cabal sandbox init
cabal sandbox add-source ../ghc-typelits-extra
cabal sandbox add-source ../ghc-typelits-natnormalise
cabal install --dependencies-only --enable-tests
```

Configure the package with testing enabled:
```
cabal configure --enable-tests
```

Once you've finished hacking, build and test:
```
cabal build
cabal test
```
