use nom::branch::{alt, permutation};
use nom::bytes::complete::{take_till, take_until, take_while};
use nom::character::complete::{alpha1, space0};
use nom::error::{Error, ErrorKind, ParseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::tuple;
use nom::{
    bytes::complete::{tag, tag_no_case},
    character::complete::{alphanumeric1, space1},
    sequence::delimited,
    IResult,
};
fn parse_separated_list(input: &str) -> IResult<&str, Vec<Expr>> {
    separated_list0(tag(","), parse_ident_or_value)(input)
}

fn parse_separated_list_in_parentheses(input: &str) -> IResult<&str, Vec<Expr>> {
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
fn parse_binary_op(input: &str) -> IResult<&str, BinaryOp> {
    match take_until(" ")(input)? {
        (remainder, "=") => Ok((remainder, BinaryOp::Eq)),
        (remainder, "!=") => Ok((remainder, BinaryOp::NotEq)),
        (remainder, ">") => Ok((remainder, BinaryOp::Gt)),
        (remainder, ">=") => Ok((remainder, BinaryOp::GtEq)),
        (remainder, "<") => Ok((remainder, BinaryOp::Lt)),
        (remainder, "<=") => Ok((remainder, BinaryOp::LtEq)),
        (remainder, "|") => Ok((remainder, BinaryOp::Or)),
        (remainder, "&") => Ok((remainder, BinaryOp::And)),
        _ => {
            let e: ErrorKind = ErrorKind::Tag;
            return Err(nom::Err::Error(Error::from_error_kind(input, e)));
        }
    }
}

fn parse_ident_or_value(input: &str) -> IResult<&str, Expr> {
    let (input, _) = space0(input)?;
    alt((parse_function, parse_ident, parse_value))(input)
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

fn parse_function(input: &str) -> IResult<&str, Expr> {
    let (input, (function, args)) =
        tuple((alphanumeric1, parse_separated_list_in_parentheses))(input)?;
    Ok((
        input,
        Expr::Function {
            function: function.to_string(),
            args,
        },
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
    let (input, (_, _, _, left, _, op, _, right)) = tuple((
        space1,
        tag_no_case("filter"),
        space1,
        parse_ident_or_value,
        space1,
        parse_binary_op,
        space1,
        parse_ident_or_value,
    ))(input)?;
    let expr = Expr::BinaryOp {
        left: Box::new(left),
        op,
        right: Box::new(right),
    };
    match parse_filter_expr_next(Some(expr.clone()), input) {
        Err(_) => {
            return Ok((input, Some(expr)));
        }
        Ok((remainder, expr)) => Ok((remainder, expr)),
    }
}

fn parse_filter_expr_next(left: Option<Expr>, input: &str) -> IResult<&str, Option<Expr>> {
    match left {
        Some(left) => {
            let (input, op, right) =
                match tuple((space1, parse_binary_op, space1, parse_ident_or_value))(input) {
                    Ok((input, (_, op, _, right))) => (input, op, right),
                    _ => {
                        let e: ErrorKind = ErrorKind::Tag;
                        return Err(nom::Err::Error(Error::from_error_kind(input, e)));
                    }
                };
            let expr = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            match parse_filter_expr_next(Some(expr.clone()), input) {
                Err(_) => {
                    return Ok((input, Some(expr)));
                }
                Ok((remainder, expr)) => Ok((remainder, expr)),
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
    Ok((input, Some(Expr::GroupBy { columns: group_by })))
}

fn parse_order_by_expr(input: &str) -> IResult<&str, Option<Expr>> {
    let (input, _) = space1(input)?;
    let (input, _) = tag_no_case("order by")(input)?;
    let (input, _) = space1(input)?;
    let (input, orders) = parse_orders_list(input)?;
    Ok((input, Some(Expr::OrderBy { orders })))
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

fn parse_exprs(input: &str) -> IResult<&str, Vec<Option<Expr>>> {
    many0(alt((
        parse_filter_expr,
        parse_group_by_expr,
        parse_order_by_expr,
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

//  col1 > 1 and col2 < 2
// (col 1 > 1 and col2 < 2) or (col 1 < 1 and col2 > 2)
// rem:  and co2 > 400  and co3 < 500, expr: {col1, >, 100}
fn main() {
    let (r, o) = parse_select(
        "select '15',function1(name),id,name,title from table filter id > 100 & id < 200 group by id,name order by id asc, sex desc",
    )
    .unwrap();
    println!("{:?}", r);
    println!("{:?}", o);
}

// #[cfg(test)]
// mod test {
//     use super::*;
//     #[test]
//     fn parse_select_works() {}
// }
