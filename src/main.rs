mod parser;
use parser::Query;
fn main() {
    let (remainder, query) = Query::parse(
        "select id,name,class from table1 group by class",
    )
    .unwrap();
    println!("{:?}", remainder);
    println!("{:?}", query);
}
