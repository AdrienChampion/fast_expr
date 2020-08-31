#[test]
fn simple_1() {}
pub mod simple_1 {
    pub type Op = ();
    pub type UnOp = ();
    pub type Id = ();
    pub type Val = ();
    fast_expr::fast_expr! {
        pub enum Expr {
            Cst(Val),
            Id(Id),
            UnApp { op: UnOp, arg: wrap::Box<Self> },
            App { op: Op, args: coll::Vec<Self> },
        }
    }
}
