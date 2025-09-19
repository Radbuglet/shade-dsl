use crate::{
    base::{
        Session,
        arena::{Obj, ObjInterner},
    },
    typeck::syntax::{FuncInstance, Ty, ValueInterner},
};

pub struct AnalyzeCx<'a> {
    pub session: &'a Session,
    pub value_interner: &'a mut ValueInterner,
    pub ty_interner: &'a mut ObjInterner<Ty>,
    pub fn_interner: &'a mut ObjInterner<FuncInstance>,
}

impl AnalyzeCx<'_> {
    pub fn eval(&mut self, func: Obj<FuncInstance>) {}
}
