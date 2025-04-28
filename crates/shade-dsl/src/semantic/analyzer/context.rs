use crate::base::Gcx;

#[derive(Debug)]
pub struct Analyzer<'gcx> {
    gcx: Gcx<'gcx>,
}
