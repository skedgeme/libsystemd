{ cabal ? (import <nixpkgs> {}).pkgs.haskellPackages_ghc763p7878.cabal
, systemd ? (import <nixpkgs> {}).pkgs.systemd
, unixBytestring ? (import <nixpkgs> {}).pkgs.haskellPackages_ghc763p7878.unixBytestring
}:

cabal.mkDerivation (self: {
  pname = "libsystemd";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ unixBytestring ];
  extraLibraries = [ systemd ];
  jailbreak = true;
  meta = {
    homepage = "http://github.com/dharaj/libsystemd";
    description = "Haskell bindings to libsystemd";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
