use nom::branch::alt;
use nom::bytes::complete::{take_until, take_until1, take_while};
use nom::character::complete::{alphanumeric0, digit1};
use nom::combinator::map;
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::{pair, terminated, tuple};
use nom::{
    bytes::complete::{tag, tag_no_case},
    character::complete::{alpha1, alphanumeric1, space0, space1},
    sequence::delimited,
    IResult,
};
pub fn parse_separated_list(input: &str) -> IResult<&str, Vec<Expr>> {
    separated_list0(tag(","), parse_ident_or_value)(input)
}

pub fn parse_separated_list_in_parentheses(input: &str) -> IResult<&str, Vec<Expr>> {
    delimited(tag("("), parse_separated_list, tag(")"))(input)
}
fn parse_insert(input: &str) -> IResult<&str, Query> {
    let (input, _) = tag_no_case("insert")(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("into")(input)?;
    let (input, _) = space1(input)?;
    let (input, table) = alphanumeric1(input)?;
    let (input, _) = space1(input)?;
    let (input, columns) = parse_separated_list_in_parentheses(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("values")(input)?;
    let (input, values) = parse_separated_list_in_parentheses(input)?;
    let (input, _) = tag_no_case(";")(input)?;
    Ok((
        input,
        Query {
            body: Statement::Insert {
                table: table.to_string(),
                columns: columns,
                values: values,
            },
        },
    ))
}

fn parse_select(input: &str) -> IResult<&str, Query> {
    let (input, _) = tag_no_case("select")(input)?;
    let (input, _) = space1(input)?;
    let (input, columns) = parse_separated_list(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("from")(input)?;
    let (input, _) = space1(input)?;
    let (input, table) = alphanumeric1(input)?;
    let (input, exprs) = parse_exprs(input)?;
    Ok((
        input,
        Query {
            body: Statement::Select {
                table: table.to_string(),
                columns,
                exprs,
            },
        },
    ))
}

fn match_binary_op(op: &str) -> Result<BinaryOp, &str> {
    match op {
        "&" => Ok(BinaryOp::And),
        "|" => Ok(BinaryOp::Or),
        "!|" => Ok(BinaryOp::Xor),
        ">" => Ok(BinaryOp::Gt),
        ">=" => Ok(BinaryOp::GtEq),
        "<" => Ok(BinaryOp::Lt),
        "<=" => Ok(BinaryOp::LtEq),
        "=" => Ok(BinaryOp::Eq),
        "!=" => Ok(BinaryOp::NotEq),
        "%" => Ok(BinaryOp::Like),
        _ => Err("Неизвестный оператор"),
    }
}

fn parse_binary_op(input: &str) -> IResult<&str, &str> {
    let (remaining, _) = space0(input)?;
    alt((
        tag("&"),
        tag("|"),
        tag("!|"),
        tag(">"),
        tag(">="),
        tag("<"),
        tag("<="),
        tag("="),
        tag("!="),
        tag("%"),
    ))(remaining)
}

pub fn parse_ident_or_value(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = space0(input)?;
    alt((parse_function, parse_ident, parse_value))(remaining)
}
fn parse_word_start_from_alpha(input: &str) -> IResult<&str, String> {
    let (remaining, first) = alpha1(input)?;
    let (remaining, second) = alphanumeric0(remaining)?;
    let value = [first, second].concat();
    Ok((remaining, value))
}

fn parse_ident(input: &str) -> IResult<&str, Expr> {
    let (remaining, value) = parse_word_start_from_alpha(input)?;
    Ok((remaining, Expr::Identifier(Identifier { value })))
}

fn parse_function(input: &str) -> IResult<&str, Expr> {
    let (remaining, (function, args)) = tuple((
        parse_word_start_from_alpha,
        parse_separated_list_in_parentheses,
    ))(input)?;
    Ok((remaining, Expr::Function { function, args }))
}

fn parse_value(input: &str) -> IResult<&str, Expr> {
    let (remaining, value) = alt((
        delimited(
            alt((tag("\""), tag("\'"))),
            alphanumeric1,
            alt((tag("\""), tag("\'"))),
        ),
        digit1,
    ))(input)?;
    Ok((remaining, Expr::Value(value.to_string())))
}

fn parse_expr_in_parentheses<'a>(
    input: &'a str,
    expr: fn(&'a str) -> IResult<&'a str, Expr>,
) -> IResult<&'a str, Expr> {
    let (remaining, _) = space0(input)?;
    delimited(tag("("), expr, tag(")"))(remaining)
}

fn parse_filter_term(input: &str) -> IResult<&str, Expr> {
    let (remaining, first) = alt((parse_ident_or_value, |i| {
        parse_expr_in_parentheses(i, parse_filter)
    }))(input)?;
    let (remaining, rest) = many0(pair(
        parse_binary_op,
        alt((parse_ident_or_value, |i| {
            parse_expr_in_parentheses(i, parse_filter)
        })),
    ))(remaining)?;

    let mut result = first;
    for (op, term) in rest {
        let bop = match_binary_op(op).unwrap();
        result = Expr::BinaryOp {
            left: Box::new(result),
            op: bop,
            right: Box::new(term),
        };
    }
    Ok((remaining, result))
}

pub fn parse_filter(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = space0(input)?;
    let (remaining, first) = parse_filter_term(remaining)?;
    let (remaining, rest) = many0(pair(parse_binary_op, parse_filter_term))(remaining)?;

    let mut result = first;
    for (op, term) in rest {
        let bop = match_binary_op(op).unwrap();
        result = Expr::BinaryOp {
            left: Box::new(result),
            op: bop,
            right: Box::new(term),
        };
    }
    Ok((remaining, result))
}

pub fn parse_filter_expression(input: &str) -> IResult<&str, Expr> {
    let (remaining, _) = space1(input)?;
    let (remaining, _) = tag_no_case("filter")(remaining)?;
    let (remaining, _) = space1(remaining)?;
    parse_filter(remaining)
}

fn parse_group_by_expression(input: &str) -> IResult<&str, Expr> {
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("group by")(input)?;
    let (input, _) = space1(input)?;
    let (input, group_by) = parse_separated_list(input)?;
    Ok((input, Expr::GroupBy { columns: group_by }))
}

fn parse_order_by_expression(input: &str) -> IResult<&str, Expr> {
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("order by")(input)?;
    let (input, _) = space1(input)?;
    let (input, orders) = parse_orders_list(input)?;
    Ok((input, Expr::OrderBy { orders }))
}

fn parse_asc(input: &str) -> IResult<&str, SortingOrder> {
    let (input, _) = tag_no_case("asc")(input)?;
    Ok((input, SortingOrder::ASC))
}

fn parse_desc(input: &str) -> IResult<&str, SortingOrder> {
    let (input, _) = tag_no_case("desc")(input)?;
    Ok((input, SortingOrder::DESC))
}

fn parse_order(input: &str) -> IResult<&str, Order> {
    let (input, (_, ident, _, order)) =
        tuple((space0, parse_ident, space1, alt((parse_asc, parse_desc))))(input)?;
    Ok((input, Order { ident, order }))
}

fn parse_orders_list(input: &str) -> IResult<&str, Vec<Order>> {
    separated_list0(tag(","), parse_order)(input)
}

fn parse_exprs(input: &str) -> IResult<&str, Vec<Expr>> {
    many0(alt((
        parse_filter_expression,
        parse_group_by_expression,
        parse_order_by_expression,
    )))(input)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Insert {
        table: String,
        columns: Vec<Expr>,
        values: Vec<Expr>,
    },
    Select {
        table: String,
        columns: Vec<Expr>,
        exprs: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Or,
    And,
    Xor,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Eq,
    NotEq,
    Like,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Order {
    pub ident: Expr,
    pub order: SortingOrder,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SortingOrder {
    ASC,
    DESC,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// Identifier e.g. table name or column name
    Identifier(Identifier),
    Value(String),
    Function {
        function: String,
        args: Vec<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    GroupBy {
        columns: Vec<Expr>,
    },
    OrderBy {
        orders: Vec<Order>,
    },
    Subquery(Box<Query>),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Query {
    body: Statement,
}

impl Query {
    pub fn parse(input: &str) -> IResult<&str, Query> {
        alt((parse_select, parse_insert))(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_ident_works() {
        let (remainder, expr) = parse_ident_or_value("name").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            expr,
            Expr::Identifier(Identifier {
                value: "name".to_string(),
            })
        );
    }

    #[test]
    fn parse_value_works() {
        let (remainder, expr) = parse_ident_or_value("224").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(expr, Expr::Value("224".to_string()),);
    }

    #[test]
    fn parse_value_in_buckets_works() {
        let (remainder, expr) = parse_ident_or_value("\"kek\"").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(expr, Expr::Value("kek".to_string()),);
    }

    #[test]
    fn parse_function_works() {
        let (remainder, expr) = parse_ident_or_value("testFunc(a,b,\"25\")").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            expr,
            Expr::Function {
                function: "testFunc".to_string(),
                args: vec![
                    Expr::Identifier(Identifier {
                        value: "a".to_string(),
                    }),
                    Expr::Identifier(Identifier {
                        value: "b".to_string(),
                    }),
                    Expr::Value("25".to_string())
                ]
            },
        );
    }
    #[test]
    fn simple_select_works() {
        let (remainder, query) =
            Query::parse("select '15',function1(name),id from table1").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            query,
            Query {
                body: Statement::Select {
                    table: "table1".to_string(),
                    columns: vec![
                        Expr::Value("15".to_string()),
                        Expr::Function {
                            function: "function1".to_string(),
                            args: vec![Expr::Identifier(Identifier {
                                value: "name".to_string(),
                            })]
                        },
                        Expr::Identifier(Identifier {
                            value: "id".to_string()
                        })
                    ],
                    exprs: vec![]
                }
            }
        );
    }

    #[test]
    fn select_with_filter_works() {
        let (remainder, query) =
            Query::parse("select id,name from table1 filter (balance > 500) | (age > 18)").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            query,
            Query {
                body: Statement::Select {
                    table: "table1".to_string(),
                    columns: vec![
                        Expr::Identifier(Identifier {
                            value: "id".to_string()
                        }),
                        Expr::Identifier(Identifier {
                            value: "name".to_string()
                        }),
                    ],
                    exprs: vec![Expr::BinaryOp {
                        left: Box::new(Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: "balance".to_string()
                            })),
                            op: BinaryOp::Gt,
                            right: Box::new(Expr::Value("500".to_string())),
                        }),
                        op: BinaryOp::Or,
                        right: Box::new(Expr::BinaryOp {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: "age".to_string()
                            })),
                            op: BinaryOp::Gt,
                            right: Box::new(Expr::Value("18".to_string())),
                        }),
                    }]
                }
            }
        );
    }

    #[test]
    fn select_with_group_by_works() {
        let (remainder, query) =
            Query::parse("select id,name,class from table1 group by class").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            query,
            Query {
                body: Statement::Select {
                    table: "table1".to_string(),
                    columns: vec![
                        Expr::Identifier(Identifier {
                            value: "id".to_string()
                        }),
                        Expr::Identifier(Identifier {
                            value: "name".to_string()
                        }),
                        Expr::Identifier(Identifier {
                            value: "class".to_string()
                        }),
                    ],
                    exprs: vec![Expr::GroupBy {
                        columns: vec![Expr::Identifier(Identifier {
                            value: "class".to_string()
                        })],
                    }]
                }
            }
        );
    }
    #[test]
    fn select_with_order_by_works() {
        let (remainder, query) =
            Query::parse("select id,name,class from table1 order by class desc").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            query,
            Query {
                body: Statement::Select {
                    table: "table1".to_string(),
                    columns: vec![
                        Expr::Identifier(Identifier {
                            value: "id".to_string()
                        }),
                        Expr::Identifier(Identifier {
                            value: "name".to_string()
                        }),
                        Expr::Identifier(Identifier {
                            value: "class".to_string()
                        }),
                    ],
                    exprs: vec![Expr::OrderBy {
                        orders: vec![Order {
                            ident: Expr::Identifier(Identifier {
                                value: "class".to_string()
                            }),
                            order: SortingOrder::DESC
                        }]
                    }]
                }
            }
        );
    }
}
