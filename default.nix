let

sources = import ./nix/sources.nix;
nixos-22-05 = import sources."nixos-22.05" {};
nixos-22-11 = import sources."nixos-22.11" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    lazy-async = ./lazy-async;
};

ghc."8.10" = nixos-22-05.haskell.packages.ghc8107.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
    ];
});

ghc."9.0" = nixos-22-11.haskell.packages.ghc90.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
    ];
});

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
    ];
});

ghc."9.4" = nixos-22-11.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            data-functor-logistic = new.callPackage ./nix/data-functor-logistic.nix {};
            rank2classes = new.callPackage ./nix/rank2classes-1.4.6.nix {};
        })
    ];
});

in

symlinkJoin {
    name = "lazy-async";
    paths = concatMap (x: [x.lazy-async]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
