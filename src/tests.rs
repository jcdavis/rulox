#[cfg(test)]
mod tests {
    use crate::compiler;
    use crate::vm;

    fn interpet(src: &str) -> Vec<String> {
        let compiled = compiler::compile(src, false).unwrap();
        let mut vm = vm::VM::new(compiled, false, true);
        vm.run();
        vm.output
    }

    #[test]
    fn test_block() {
        let src = r#"
        var a = "outer";
            {
            var a = "inner";
            a = a + " added";
            print a;
            }
            print a;

        "#;
        assert_eq!(interpet(&src), vec!["\"inner added\"", "\"outer\""]);
    }

    #[test]
    fn test_loop() {
        let src = r#"
        for(var foo = 0; foo < 10; foo = foo+2) {
            if (foo != 4) {
              print foo;
            }
          }
        "#;
        assert_eq!(interpet(&src), vec!["0", "2", "6", "8"]);
    }

    #[test]
    fn test_function() {
        let src = r#"
            fun foo(a) {
                return bar(a) + 1;
            }

            fun bar(a) {
                return baz(a) + 2;
            }

            fun baz(a) {
                return a*2;
            }

            print foo(5);
        "#;
        assert_eq!(interpet(&src), vec!["13"]);
    }

    #[test]
    fn test_upvalue() {
        let src = r#"
        var globalSet;
        var globalGet;

        fun main() {
        var a = "initial";

        fun set() { a = "updated"; }
        fun get() { print a; }

        globalSet = set;
        globalGet = get;
        }

        main();
        globalSet();
        globalGet();
        "#;
        assert_eq!(interpet(&src), vec!["\"updated\""]);
    }

    #[test]
    fn class_fields() {
        let src = r#"
        class Pair {}

        var pair = Pair();
        pair.first = 1;
        pair.second = 2;
        print pair.first + pair.second; // 3.
        "#;
        assert_eq!(interpet(&src), vec!["3"]);
    }

    #[test]
    fn class_methods() {
        let src = r#"
        class Scone {
            topping(first, second) {
              print "scone with " + first + " and " + second;
            }
          }

          var scone = Scone();
          scone.topping("berries", "cream");
        "#;
        assert_eq!(interpet(&src), vec!["\"scone with berries and cream\""]);
    }

    #[test]
    fn test_this() {
        let  src = r#"
        class Nested {
            method() {
              fun function() {
                print this;
              }

              function();
            }
          }

          Nested().method();
        "#;
        assert_eq!(interpet(src), vec!["Nested instance"]);
    }

    #[test]
    fn test_init() {
        let src = r#"
        class CoffeeMaker {
            init(coffee) {
                this.coffee = coffee;
            }

            brew() {
                print "Enjoy your cup of " + this.coffee;

                // No reusing the grounds!
                this.coffee = nil;
            }
        }

        var maker = CoffeeMaker("coffee and chicory");
        maker.brew();
        "#;
        assert_eq!(interpet(src), vec!["\"Enjoy your cup of coffee and chicory\""]);
    }

    #[test]
    fn test_invoke_bug() {
        let src = r#"
        class Oops {
            init() {
              fun f() {
                print "not a method";
              }

              this.field = f;
            }
          }

        var oops = Oops();
        oops.field();
        "#;
        assert_eq!(interpet(src), vec!["\"not a method\""]);
    }

    #[test]
    fn test_super() {
        let src = r#"
        class Doughnut {
            cook() {
              print "Dunk in the fryer.";
              this.finish("sprinkles");
            }

            finish(ingredient) {
              print "Finish with " + ingredient;
            }
          }

          class Cruller < Doughnut {
            finish(ingredient) {
              // No sprinkles, always icing.
              super.finish("icing");
            }
          }

          Cruller().cook();
        "#;
        assert_eq!(interpet(src), vec!["\"Dunk in the fryer.\"", "\"Finish with icing\""]);
    }
}