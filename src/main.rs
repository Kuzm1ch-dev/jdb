use nom::branch::alt;
use nom::multi::many0;
use nom::{
    bytes::complete::{tag, tag_no_case},
    character::complete::{alphanumeric1, space1},
    multi::separated_list1,
    sequence::delimited,
    IResult,
};
fn parse_separated_list(input: &str) -> IResult<&str, Vec<&str>> {
    separated_list1(tag(","), alphanumeric1)(input)
}

fn parse_separated_list_in_parentheses(input: &str) -> IResult<&str, Vec<&str>> {
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
                columns: columns.iter().map(|s| s.to_string()).collect(),
                values: values.iter().map(|s| s.to_string()).collect(),
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
                columns: columns.iter().map(|s| s.to_string()).collect(),
                exprs: exprs,
            },
        },
    ))
}

fn parse_binary_op(input: &str) -> IResult<&str, BinaryOp> {
    alt((
        parse_binary_op_is_eq,
        parse_binary_op_is_not_eq,
        parse_binary_op_is_gt,
        parse_binary_op_is_gteq,
        parse_binary_op_is_lt,
        parse_binary_op_is_lteq,
        parse_binary_op_is_or,
        parse_binary_op_is_and,
    ))(input)
}

fn parse_binary_op_is_eq(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("=")(input)?;
    Ok((input, BinaryOp::Eq))
}

fn parse_binary_op_is_not_eq(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("!=")(input)?;
    Ok((input, BinaryOp::NotEq))
}

fn parse_binary_op_is_gt(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag(">")(input)?;
    Ok((input, BinaryOp::Gt))
}

fn parse_binary_op_is_gteq(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag(">=")(input)?;
    Ok((input, BinaryOp::GtEq))
}

fn parse_binary_op_is_lt(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("<")(input)?;
    Ok((input, BinaryOp::Lt))
}

fn parse_binary_op_is_lteq(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("<=")(input)?;
    Ok((input, BinaryOp::LtEq))
}

fn parse_binary_op_is_or(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("or")(input)?;
    Ok((input, BinaryOp::Or))
}

fn parse_binary_op_is_and(input: &str) -> IResult<&str, BinaryOp> {
    let (input, _) = tag("and")(input)?;
    Ok((input, BinaryOp::And))
}

fn parse_ident_or_value(input: &str) -> IResult<&str, Expr> {
    alt((parse_ident, parse_value))(input)
}

fn parse_ident(input: &str) -> IResult<&str, Expr> {
    let (input, val) = alphanumeric1(input)?;
    Ok((
        input,
        Expr::Identifier(Identifier {
            value: val.to_string(),
        }),
    ))
}

fn parse_value(input: &str) -> IResult<&str, Expr> {
    let (input, value) = delimited(
        alt((tag("\""), tag("\'"))),
        alphanumeric1,
        alt((tag("\""), tag("\'"))),
    )(input)?;
    Ok((input, Expr::Value(value.to_string())))
}

fn parse_filter_expr(input: &str) -> IResult<&str, Option<Expr>> {
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("filter")(input)?;
    let (input, _) = space1(input)?;
    let (input, left) = parse_ident_or_value(input)?;
    let (input, _) = space1(input)?;
    let (input, op) = parse_binary_op(input)?;
    let (input, _) = space1(input)?;
    let (input, right) = parse_ident_or_value(input)?;
    let expr = Expr::BinaryOp {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };
    match parse_filter_expr_next(Some(expr.clone()), input) {
        Err(_) => return Ok((input, Some(expr))),
        Ok((r, e)) => Ok((r, e)),
    }
}

fn parse_filter_expr_next(left: Option<Expr>, input: &str) -> IResult<&str, Option<Expr>> {
    match left {
        Some(left) => {
            let (input, _) = space1(input)?;
            let (input, op) = parse_binary_op(input)?;
            let (input, _) = space1(input)?;
            let (input, right) = parse_ident_or_value(input)?;
            let expr = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            match parse_filter_expr_next(Some(expr.clone()), input) {
                Err(_) => return Ok((input, Some(expr))),
                Ok((r, e)) => Ok((r, e)),
            }
        }
        None => panic!("Ошибка парсинга!"),
    }
}

fn parse_group_by_expr(input: &str) -> IResult<&str, Option<Expr>> {
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("group by")(input)?;
    let (input, _) = space1(input)?;
    let (input, group_by) = parse_separated_list(input)?;
    Ok((
        input,
        Some(Expr::GroupBy {
            columns: group_by.iter().map(|s| s.to_string()).collect(),
        }),
    ))
}

fn parse_exprs(input: &str) -> IResult<&str, Vec<Option<Expr>>> {
    many0(alt((parse_filter_expr, parse_group_by_expr)))(input)
}

fn parse_end(input: &str) -> IResult<&str, &str> {
    tag_no_case(";")(input)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Insert {
        table: String,
        columns: Vec<String>,
        values: Vec<String>,
    },
    Select {
        table: String,
        columns: Vec<String>,
        exprs: Vec<Option<Expr>>,
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    /// The value of the identifier without quotes.
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// Identifier e.g. table name or column name
    Identifier(Identifier),
    Value(String),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    GroupBy {
        columns: Vec<String>,
    },
    Subquery(Box<Query>),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Query {
    body: Statement,
}

//  col1 > 1 and col2 < 2
// (col 1 > 1 and col2 < 2) or (col 1 < 1 and col2 > 2)
// rem:  and co2 > 400  and co3 < 500, expr: {col1, >, 100}
fn main() {
    let (r, o) =
        parse_select("select id,name,title from table filter id > 15 and name = 'sex';").unwrap();
    println!("{:?}", r);
    println!("{:?}", o);
}

// #[cfg(test)]
// mod test {
//     use super::*;
//     #[test]
//     fn parse_select_works() {}
// }
