use std::{fmt, hash::BuildHasher};

use hashbrown::{HashMap, hash_map};

use crate::{
    base::syntax::Symbol,
    typeck::{analysis::TyCtxt, syntax::ValuePlace},
    utils::hash::FxHashMap,
};

// === TyCtxt === //

impl TyCtxt {
    pub fn resolve_intrinsic(&self, id: Symbol) -> Option<ValuePlace> {
        let mut intrinsics = self.intrinsic_cache.borrow_mut();

        match intrinsics.entry(id) {
            hash_map::Entry::Occupied(entry) => Some(*entry.get()),
            hash_map::Entry::Vacant(entry) => {
                let value = self.intrinsic_resolver.resolve(
                    self,
                    &id.as_str(&self.session).split(".").collect::<Vec<_>>(),
                )?;
                entry.insert(value);
                Some(value)
            }
        }
    }
}

// === Traits === //

pub struct IntrinsicResolver {
    inner: Box<dyn IntrinsicResolverTrait>,
}

impl fmt::Debug for IntrinsicResolver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ErasedIntrinsicResolver")
            .finish_non_exhaustive()
    }
}

impl Clone for IntrinsicResolver {
    fn clone(&self) -> Self {
        self.clone_erased()
    }
}

impl IntrinsicResolver {
    pub fn new(resolver: impl IntrinsicResolverTrait) -> Self {
        Self {
            inner: Box::new(resolver),
        }
    }

    pub fn new_map(entries: impl IntoIterator<Item = (&'static str, IntrinsicResolver)>) -> Self {
        Self::new(entries.into_iter().collect::<FxHashMap<_, _>>())
    }

    pub fn new_terminal(f: fn(&TyCtxt) -> Option<ValuePlace>) -> Self {
        Self::new(f)
    }

    pub fn new_dynamic(f: fn(&TyCtxt, &str) -> Option<ValuePlace>) -> Self {
        Self::new(f)
    }
}

impl IntrinsicResolverTrait for IntrinsicResolver {
    fn resolve(&self, tcx: &TyCtxt, parts: &[&str]) -> Option<ValuePlace> {
        self.inner.resolve(tcx, parts)
    }

    fn clone_erased(&self) -> IntrinsicResolver {
        self.inner.clone_erased()
    }
}

pub trait IntrinsicResolverTrait: 'static {
    fn resolve(&self, tcx: &TyCtxt, parts: &[&str]) -> Option<ValuePlace>;

    fn clone_erased(&self) -> IntrinsicResolver;
}

// === Impls === //

impl<S> IntrinsicResolverTrait for HashMap<&'static str, IntrinsicResolver, S>
where
    S: 'static + BuildHasher + Clone,
{
    fn resolve(&self, tcx: &TyCtxt, parts: &[&str]) -> Option<ValuePlace> {
        let (first, rest) = parts.split_first()?;
        let child = self.get(first)?;
        child.resolve(tcx, rest)
    }

    fn clone_erased(&self) -> IntrinsicResolver {
        IntrinsicResolver::new(self.clone())
    }
}

impl IntrinsicResolverTrait for fn(&TyCtxt, &str) -> Option<ValuePlace> {
    fn resolve(&self, tcx: &TyCtxt, parts: &[&str]) -> Option<ValuePlace> {
        if parts.len() != 1 {
            return None;
        }

        self(tcx, parts[0])
    }

    fn clone_erased(&self) -> IntrinsicResolver {
        IntrinsicResolver::new(*self)
    }
}

impl IntrinsicResolverTrait for fn(&TyCtxt) -> Option<ValuePlace> {
    fn resolve(&self, tcx: &TyCtxt, parts: &[&str]) -> Option<ValuePlace> {
        if !parts.is_empty() {
            return None;
        }

        self(tcx)
    }

    fn clone_erased(&self) -> IntrinsicResolver {
        IntrinsicResolver::new(*self)
    }
}
