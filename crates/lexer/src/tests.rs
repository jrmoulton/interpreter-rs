#![cfg(test)]
use expect_test::expect_file;
use TokenKind::*;

use super::*;

mod results {
    use pretty_assertions::assert_eq;

    use super::*;
    #[test]
    fn single_expr() {
        let code: &'static str = r#"5 + 5"#;
        let correct = vec![Int(5), Plus, Int(5)];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_string() {
        let code: &'static str = r#" "foo bar" "#;
        let correct = vec![String("foo bar".into())];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn comment() {
        let code: &'static str = r#" //foo bar"#;
        let correct = vec![Comment("foo bar".into())];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn escaped_quote() {
        let code: &'static str = r#" "foo \" bar" "#;
        let correct = vec![String("foo \\\" bar".into())];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn let_ident_string() {
        let code: &'static str = r#" let x = "foo bar" "#;
        let correct = vec![Let, Ident("x".into()), Assign, String("foo bar".into())];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_let() {
        let code: &'static str = r#"let"#;
        let correct = vec![Let];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_assign() {
        let code: &'static str = r#"let five = 5;"#;
        let correct = vec![Let, Ident("five".into()), Assign, Int(5), Semicolon];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn bool_or() {
        let code: &'static str = r#"3 == 3 || 5 == 6"#;
        let correct = vec![Int(3), Eq, Int(3), Or, Int(5), Eq, Int(6)];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn bit_or() {
        let code: &'static str = r#"3 | 5"#;
        let correct = vec![Int(3), BitOr, Int(5)];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn bool_and() {
        let code: &'static str = r#"3 == 3 && 5 == 6"#;
        let correct = vec![Int(3), Eq, Int(3), And, Int(5), Eq, Int(6)];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn bit_and() {
        let code: &'static str = r#"3 & 5"#;
        let correct = vec![Int(3), BitAnd, Int(5)];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn double_assign() {
        let code: &'static str = r#"let five = 5;let ten = 10;"#;
        #[rustfmt::skip]
    let correct = vec![
        Let, Ident("five".into()), Assign, Int(5), Semicolon, Let, Ident("ten".into()), Assign, Int(10), Semicolon,
    ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_func() {
        let code: &'static str = r#"let add = fn(x, y) {
                x - y
            };"#;
        #[rustfmt::skip]
    let correct = vec![
        Let,
        Ident("add".into()), Assign, Func, LParen, Ident("x".into()), Comma, Ident("y".into()), RParen, LBrace, Ident("x".into()),
        Minus, Ident("y".into()), RBrace, Semicolon,
    ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn array() {
        let code: &'static str = r#"let x = [12, 15, 30];"#;
        #[rustfmt::skip]
        let correct = vec![
        Let, Ident("x".into()), Assign, LBracket, Int(12), Comma, Int(15), Comma, Int(30), RBracket, Semicolon,
        ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn let_ends_on_plus_space() {
        let code: &'static str = r#"let x = 10 + "#;
        #[rustfmt::skip]
        let correct = vec![
            Let, Ident("x".into()), Assign, Int(10), Plus
        ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn let_ends_on_plus() {
        let code: &'static str = r#"let x = 10 +"#;
        #[rustfmt::skip]
        let correct = vec![
            Let, Ident("x".into()), Assign, Int(10), Plus
        ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn comprehensive() {
        let code: &'static str = r#"
                let five = 5;
                let ten = 10;
                   let add = fn(x, y) {
                     x - y;
                };
                   let result = add(five, ten);
                   !-/*5;
                   5 < 10 > 5;
                   if (5 < 10) {
                       return true;
                   } else {
                       return false;
                }
                10 == 10; 10 != 9;"#;
        #[rustfmt::skip]
        let correct = vec![
            Let, Ident("five".into()), Assign, Int(5), Semicolon,
            Let, Ident("ten".into()), Assign, Int(10), Semicolon,
            Let, Ident("add".into()), Assign, Func, LParen, Ident("x".into()), Comma, Ident("y".into()), RParen, LBrace,
            Ident("x".into()), Minus, Ident("y".into()), Semicolon, 
            RBrace, Semicolon,
            Let, Ident("result".into()), Assign, Ident("add".into()), LParen, Ident("five".into()), Comma, Ident("ten".into()), RParen, Semicolon,
            Bang, Minus, Slash, Asterisk, Int(5), Semicolon,
            Int(5), Lt, Int(10), Gt, Int(5), Semicolon,
            If, LParen, Int(5), Lt, Int(10), RParen, LBrace,
            Return, True, Semicolon,
            RBrace, Else, LBrace, 
            Return, False, Semicolon,
            RBrace,
            Int(10), Eq, Int(10), Semicolon, Int(10), Ne, Int(9), Semicolon
        ];
        let lexer = Lexer::new(code.into());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.kind)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn comprehensive_expect_test() {
        let code: &'static str = r#"let five = 5;
                let ten = 10;
                   let add = fn(x, y) {
                     x - y;
                };
                   let result = add(five, ten);
                   !-/*5;
                   5 < 10 > 5;
                   if (5 < 10) {
                       return true;
                   } else {
                       return false;
                }
                3 | 6;
                2 & 2;
                1 == 2 && false;
                2 == 2 || false;
                10 == 10; 10 != 9;"#;
        let lexer = Lexer::new(code.into()).into_iter().collect::<Vec<_>>();
        let expected = expect_file!["./../tests/expect_test_results/comprehensive_expect_test.txt"];
        expected.assert_eq(&format!("{lexer:#?}"));
    }
}
mod errors {
    use super::*;

    #[test]
    fn bad_character() {
        let code: &'static str = r#"?"#;
        let lexer = Lexer::new(code.into()).into_iter().collect::<Vec<_>>();
        let expected = expect_file!["./../tests/expect_test_results/bad_character.txt"];
        expected.assert_eq(&format!("{lexer:#?}"));
    }
}
