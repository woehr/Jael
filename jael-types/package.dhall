let defs = ./../package-defaults.dhall

let deps = ./../package-dependencies.dhall

in      defs
    //  { name = "jael-types"
        , library =
            { dependencies = deps
            , source-dirs = "lib"
            }
        }
