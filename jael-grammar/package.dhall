let defs = ./../package-defaults.dhall

let deps = ./../package-dependencies.dhall

in      defs
    //  { name = "jael-grammar"
        , extra-source-files =
          [ "lib/Jael/Grammar/Lexer.x"
          , "lib/Jael/Grammar/Parser.y"
          , "golden/*"
          ]
        , library =
            { build-tools = [ "alex", "happy" ]
            , dependencies = [ "jael-types" ] # deps
            , source-dirs = "lib"
            }
        , executables =
            { jael-syntax =
                { dependencies = [ "jael-grammar" ] # deps
                , main = "Main.hs"
                , source-dirs = "src"
                }
            }
        , tests =
            { test =
                { build-tools = [ "hspec-discover" ]
                , dependencies = [ "jael-grammar" ] # deps
                , main = "Main.hs"
                , source-dirs = "test"
                }
            }
        }
