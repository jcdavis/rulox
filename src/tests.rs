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
}