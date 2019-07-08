{ mkDerivation, base, containers, stdenv, X11, xmonad
, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-splintah";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers X11 xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.agpl3;
}
