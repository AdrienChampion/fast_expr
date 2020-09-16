//! Examples.

macro_rules! prelude {
    () => {
        use $crate::examples::prelude::*;
    };
}

pub mod book_subst_1;
pub mod simple;
pub mod test;

pub mod prelude {
    pub use std::{collections::BTreeMap, convert::TryInto, fmt};

    pub type Res<T> = Result<T, String>;

    pub use super::{cst, id, op};
}

pub mod cst {
    prelude! {}

    pub type BCst = bool;
    pub type ICst = isize;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Cst {
        B(BCst),
        I(ICst),
    }
    impl From<BCst> for Cst {
        fn from(b_cst: BCst) -> Self {
            Self::B(b_cst)
        }
    }
    impl From<ICst> for Cst {
        fn from(i_cst: ICst) -> Self {
            Self::I(i_cst)
        }
    }

    impl TryInto<BCst> for Cst {
        type Error = String;
        fn try_into(self) -> Res<BCst> {
            match self {
                Self::B(b_cst) => Ok(b_cst),
                Self::I(i_cst) => Err(format!("expected bool value, found integer `{}`", i_cst)),
            }
        }
    }
    impl TryInto<ICst> for Cst {
        type Error = String;
        fn try_into(self) -> Res<ICst> {
            match self {
                Self::B(b_cst) => Err(format!("expected int value, found boolean `{}`", b_cst)),
                Self::I(i_cst) => Ok(i_cst),
            }
        }
    }
}

pub mod id {
    prelude! {}

    use cst::*;

    pub type BId = String;
    pub type IId = String;
    pub enum Id {
        B(BId),
        I(IId),
    }
    impl Id {
        pub fn b_id(b_id: BId) -> Self {
            Self::B(b_id)
        }
        pub fn i_id(i_id: IId) -> Self {
            Self::I(i_id)
        }
        pub fn id(&self) -> &str {
            match self {
                Self::B(b_id) => b_id,
                Self::I(i_id) => i_id,
            }
        }
    }
    impl fmt::Display for Id {
        fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
            self.id().fmt(fmt)
        }
    }

    pub type BModel = BTreeMap<BId, BCst>;
    pub type IModel = BTreeMap<IId, ICst>;

    pub struct Model {
        pub b_model: BModel,
        pub i_model: IModel,
    }
    impl Model {
        pub fn get(&self, id: &Id) -> Option<Cst> {
            match id {
                Id::B(b_id) => self.b_model.get(b_id).cloned().map(Cst::from),
                Id::I(i_id) => self.i_model.get(i_id).cloned().map(Cst::from),
            }
        }
    }
}

pub mod op {
    prelude! {}

    #[derive(Debug, Clone, Copy)]
    pub enum Op {
        BOp(BOp),
        IRel(IRel),
        IOp(IOp),
    }
    impl From<BOp> for Op {
        fn from(op: BOp) -> Self {
            Self::BOp(op)
        }
    }
    impl From<IRel> for Op {
        fn from(op: IRel) -> Self {
            Self::IRel(op)
        }
    }
    impl From<IOp> for Op {
        fn from(op: IOp) -> Self {
            Self::IOp(op)
        }
    }
    impl Op {
        pub fn res_eval<I>(self, args: I) -> Res<cst::Cst>
        where
            I: IntoIterator<Item = Res<cst::Cst>>,
            I::IntoIter: std::iter::ExactSizeIterator,
        {
            let args = args.into_iter();
            match self {
                Self::BOp(b_op) => b_op
                    .res_eval(args.map(|cst_res| cst_res.and_then(cst::Cst::try_into)))
                    .map(cst::Cst::from),
                Self::IRel(i_rel) => i_rel
                    .res_eval(args.map(|cst_res| cst_res.and_then(cst::Cst::try_into)))
                    .map(cst::Cst::from),
                Self::IOp(i_op) => i_op
                    .res_eval(args.map(|cst_res| cst_res.and_then(cst::Cst::try_into)))
                    .map(cst::Cst::from),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum BOp {
        One(BOp1),
        Two(BOp2),
        N(BOpN),
        TwoN(BOp2N),
    }
    impl BOp {
        pub fn res_eval<I>(self, args: I) -> Res<cst::BCst>
        where
            I: IntoIterator<Item = Res<cst::BCst>>,
            I::IntoIter: std::iter::ExactSizeIterator,
        {
            let mut args = args.into_iter();
            let args_len = args.len();

            macro_rules! bail {
                ($expected:expr) => {
                    return Err(format!(
                        "expected {} argument(s), found {}",
                        $expected, args_len
                    ));
                };
            }

            match self {
                Self::One(one) => match (args.next(), args.next()) {
                    (Some(val_res), None) => one.res_eval(val_res),
                    _ => bail!("one"),
                },
                Self::Two(two) => match (args.next(), args.next(), args.next()) {
                    (Some(lft_res), Some(rgt_res), None) => two.res_eval(lft_res, rgt_res),
                    _ => bail!("two"),
                },
                Self::N(op_n) => op_n.res_eval(args),
                Self::TwoN(two_n) => match (args.next(), args.next()) {
                    (Some(fst_res), Some(snd_res)) => two_n.res_eval(fst_res, snd_res, args),
                    _ => bail!("two or more"),
                },
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum BOp1 {
        Not,
    }
    impl BOp1 {
        pub fn eval(self, val: cst::BCst) -> cst::BCst {
            match self {
                Self::Not => !val,
            }
        }
        pub fn res_eval(self, val: Res<cst::BCst>) -> Res<cst::BCst> {
            Ok(self.eval(val?))
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum BOp2 {
        Implies,
    }
    impl BOp2 {
        pub fn eval(self, lft: cst::BCst, rgt: cst::BCst) -> cst::BCst {
            match self {
                Self::Implies => !lft || rgt,
            }
        }
        pub fn res_eval(self, lft: Res<cst::BCst>, rgt: Res<cst::BCst>) -> Res<cst::BCst> {
            Ok(self.eval(lft?, rgt?))
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum BOpN {
        Conj,
        Disj,
    }
    impl BOpN {
        pub fn eval(self, args: impl IntoIterator<Item = cst::BCst>) -> cst::BCst {
            match self {
                Self::Conj => {
                    let mut res = true;
                    for arg in args {
                        if !arg {
                            res = false;
                            break;
                        }
                    }
                    res
                }
                Self::Disj => {
                    let mut res = false;
                    for arg in args {
                        if arg {
                            res = true;
                            break;
                        }
                    }
                    res
                }
            }
        }
        pub fn res_eval(self, args: impl IntoIterator<Item = Res<cst::BCst>>) -> Res<cst::BCst> {
            match self {
                Self::Conj => {
                    let mut res = true;
                    for arg in args {
                        if !arg? {
                            res = false;
                            break;
                        }
                    }
                    Ok(res)
                }
                Self::Disj => {
                    let mut res = false;
                    for arg in args {
                        if arg? {
                            res = true;
                            break;
                        }
                    }
                    Ok(res)
                }
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum BOp2N {
        Eq,
    }
    impl BOp2N {
        pub fn eval(
            self,
            fst: cst::BCst,
            snd: cst::BCst,
            tail: impl IntoIterator<Item = cst::BCst>,
        ) -> cst::BCst {
            match self {
                Self::Eq => {
                    if fst != snd {
                        return false;
                    }

                    tail.into_iter().all(|val| val == fst)
                }
            }
        }
        pub fn res_eval(
            self,
            fst: Res<cst::BCst>,
            snd: Res<cst::BCst>,
            tail: impl IntoIterator<Item = Res<cst::BCst>>,
        ) -> Res<cst::BCst> {
            let (fst, snd) = (fst?, snd?);
            match self {
                Self::Eq => {
                    if fst != snd {
                        return Ok(false);
                    }

                    for val in tail {
                        if val? != fst {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IRel {
        TwoN(IRel2N),
    }
    impl IRel {
        pub fn eval(
            self,
            mut args: impl Iterator<Item = cst::ICst> + ExactSizeIterator,
        ) -> Res<cst::BCst> {
            let args_len = args.len();
            macro_rules! error {
                ($expected:expr) => {
                    || format!("expected {} argument(s), found {}", $expected, args_len);
                };
            }
            Ok(match self {
                Self::TwoN(irel_2n) => irel_2n.eval(
                    args.next().ok_or_else(error!("two or more"))?,
                    args.next().ok_or_else(error!("two or more"))?,
                    args,
                ),
            })
        }
        pub fn res_eval(
            self,
            mut args: impl Iterator<Item = Res<cst::ICst>> + ExactSizeIterator,
        ) -> Res<cst::BCst> {
            let args_len = args.len();
            macro_rules! error {
                ($expected:expr) => {
                    || format!("expected {} argument(s), found {}", $expected, args_len);
                };
            }
            match self {
                Self::TwoN(irel_2n) => irel_2n.res_eval(
                    args.next().ok_or_else(error!("two or more"))?,
                    args.next().ok_or_else(error!("two or more"))?,
                    args,
                ),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IRel2N {
        Gt,
        Ge,
        Lt,
        Le,
        Eq,
    }
    impl IRel2N {
        pub fn eval_2(self, fst: cst::ICst, snd: cst::ICst) -> cst::BCst {
            match self {
                Self::Gt => fst > snd,
                Self::Ge => fst >= snd,
                Self::Lt => fst < snd,
                Self::Le => fst <= snd,
                Self::Eq => fst == snd,
            }
        }

        pub fn eval(
            self,
            fst: cst::ICst,
            snd: cst::ICst,
            tail: impl IntoIterator<Item = cst::ICst>,
        ) -> cst::BCst {
            if !self.eval_2(fst, snd) {
                return false;
            }

            let mut last = snd;
            for next in tail {
                if !self.eval_2(last, next) {
                    return false;
                }
                last = next
            }

            true
        }
        pub fn res_eval(
            self,
            fst: Res<cst::ICst>,
            snd: Res<cst::ICst>,
            tail: impl IntoIterator<Item = Res<cst::ICst>>,
        ) -> Res<cst::BCst> {
            let (fst, snd) = (fst?, snd?);
            if !self.eval_2(fst, snd) {
                return Ok(false);
            }

            let mut last = snd;
            for next in tail {
                let next = next?;
                if !self.eval_2(last, next) {
                    return Ok(false);
                }
                last = next
            }

            Ok(true)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IOp {
        One(IOp1),
        Two(IOp2),
        TwoN(IOp2N),
    }
    impl IOp {
        pub fn res_eval(
            self,
            args: impl IntoIterator<Item = Res<cst::ICst>> + std::iter::ExactSizeIterator,
        ) -> Res<cst::ICst> {
            let args_len = args.len();
            macro_rules! bail {
                ($expected:expr) => {
                    return Err(format!(
                        "expected {} argument(s), found {}",
                        $expected, args_len
                    ));
                };
            }

            let mut args = args.into_iter();

            match self {
                Self::One(one) => match (args.next(), args.next()) {
                    (Some(val_res), None) => one.res_eval(val_res),
                    _ => bail!("one"),
                },
                Self::Two(two) => match (args.next(), args.next(), args.next()) {
                    (Some(lft_res), Some(rgt_res), None) => two.res_eval(lft_res, rgt_res),
                    _ => bail!("two"),
                },
                Self::TwoN(two_n) => match (args.next(), args.next()) {
                    (Some(fst_res), Some(snd_res)) => two_n.res_eval(fst_res, snd_res, args),
                    _ => bail!("two or more"),
                },
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IOp1 {
        Minus,
    }
    impl IOp1 {
        pub fn eval(self, val: cst::ICst) -> cst::ICst {
            match self {
                Self::Minus => -val,
            }
        }
        pub fn res_eval(self, val: Res<cst::ICst>) -> Res<cst::ICst> {
            Ok(self.eval(val?))
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IOp2 {
        Mod,
        Div,
    }
    impl IOp2 {
        pub fn eval(self, fst: cst::ICst, snd: cst::ICst) -> cst::ICst {
            match self {
                Self::Mod => fst % snd,
                Self::Div => fst / snd,
            }
        }
        pub fn res_eval(self, fst: Res<cst::ICst>, snd: Res<cst::ICst>) -> Res<cst::ICst> {
            Ok(self.eval(fst?, snd?))
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum IOp2N {
        Add,
        Sub,
        Mul,
    }
    impl IOp2N {
        pub fn eval(
            self,
            fst: cst::ICst,
            snd: cst::ICst,
            tail: impl IntoIterator<Item = cst::ICst>,
        ) -> cst::ICst {
            match self {
                Self::Add => {
                    let tail: cst::ICst = tail.into_iter().sum();
                    fst + snd + tail
                }
                Self::Sub => {
                    let tail: cst::ICst = tail.into_iter().sum();
                    fst - snd - tail
                }
                Self::Mul => {
                    let res = fst * snd;
                    tail.into_iter().fold(res, |acc, next| acc * next)
                }
            }
        }

        pub fn res_eval(
            self,
            fst: Res<cst::ICst>,
            snd: Res<cst::ICst>,
            tail: impl IntoIterator<Item = Res<cst::ICst>>,
        ) -> Res<cst::ICst> {
            Ok(match self {
                Self::Add => {
                    let mut res = fst? + snd?;
                    for val in tail {
                        res += val?
                    }
                    res
                }
                Self::Sub => {
                    let mut res = fst? - snd?;
                    for val in tail {
                        res -= val?
                    }
                    res
                }
                Self::Mul => {
                    let mut res = fst? * snd?;
                    for val in tail {
                        res = res * val?
                    }
                    res
                }
            })
        }
    }
}
