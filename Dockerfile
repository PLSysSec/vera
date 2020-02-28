FROM archlinux:20200205 
RUN pacman -Syy && pacman --noconfirm -S python2 wget stack z3
RUN pacman --noconfirm -S base-devel
COPY ./ /lejit
WORKDIR /lejit
RUN stack test --no-run-tests -j16 --ghc-options -j8

