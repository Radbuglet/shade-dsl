use std::{cell::Cell, fmt, hash, ptr::NonNull, sync::OnceLock};

use crate::semantic::syntax::{BoundValue, Ty, TyAdtMember, TyKind, Value, ValueInner};

use super::{
    DiagCtxt, Intern, Interner, ListIntern, ListInterner, SourceMap, Symbol, SymbolInterner,
};

pub type Gcx<'gcx> = &'gcx GcxOwned<'gcx>;

pub struct GcxOwned<'gcx> {
    pub bump: bumpalo::Bump,
    pub dcx: DiagCtxt<'gcx>,
    pub symbols: SymbolInterner,
    pub source_map: SourceMap,
    pub interners: GcxInterners<'gcx>,
    pub pre_interned: OnceLock<PreInterned<'gcx>>,
}

pub struct PreInterned<'gcx> {
    pub unit: Ty<'gcx>,
    pub meta_fn: Ty<'gcx>,
    pub meta_ty: Ty<'gcx>,
    pub meta_producer_fn: Ty<'gcx>,
}

thread_local! {
    static CURR_GCX: Cell<Option<NonNull<()>>> = const { Cell::new(None) };
}

impl fmt::Debug for GcxOwned<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GcxOwned").finish_non_exhaustive()
    }
}

impl<'gcx> GcxOwned<'gcx> {
    pub fn init<R>(f: impl FnOnce(Gcx<'_>) -> R) -> R {
        let bump = bumpalo::Bump::new();
        let diag = DiagCtxt::new();
        let symbols = SymbolInterner::new();
        let spans = SourceMap::new();
        let interners = GcxInterners::default();

        let gcx = GcxOwned {
            bump,
            dcx: diag,
            symbols,
            source_map: spans,
            interners,
            pre_interned: OnceLock::new(),
        };

        gcx.dcx.bind_gcx(&gcx);

        gcx.provide_tls(f)
    }

    pub fn provide_tls<R>(&'gcx self, f: impl FnOnce(Gcx<'_>) -> R) -> R {
        let _guard = scopeguard::guard(CURR_GCX.take(), |old| {
            CURR_GCX.set(old);
        });

        CURR_GCX.set(Some(NonNull::from(self).cast()));
        f(self)
    }

    pub fn fetch_tls<R>(f: impl FnOnce(Gcx<'_>) -> R) -> R {
        f(unsafe {
            CURR_GCX
                .get()
                .expect("no gcx bound to the current thread")
                .cast::<GcxOwned<'_>>()
                .as_ref()
        })
    }

    pub fn alloc<T>(&'gcx self, val: T) -> &'gcx T {
        self.bump.alloc(val)
    }

    pub fn alloc_slice<T: Clone>(&'gcx self, val: &[T]) -> &'gcx [T] {
        self.bump.alloc_slice_clone(val)
    }

    pub fn intern<T: Internable<'gcx>>(&'gcx self, value: T) -> Intern<'gcx, T> {
        T::fetch(&self.interners).intern(&self.bump, value)
    }

    pub fn intern_slice<T: ListInternable<'gcx>>(&'gcx self, value: &[T]) -> ListIntern<'gcx, T> {
        T::fetch(&self.interners).intern(&self.bump, value)
    }

    pub fn intern_iter<T>(&'gcx self, iter: impl IntoIterator<Item = T>) -> ListIntern<'gcx, T>
    where
        T: ListInternable<'gcx>,
    {
        T::fetch(&self.interners).intern_iter(&self.bump, iter)
    }

    pub fn pre_interned(&'gcx self) -> &'gcx PreInterned<'gcx> {
        self.pre_interned.get_or_init(|| PreInterned {
            unit: self.intern(TyKind::Tuple(self.intern_slice(&[]))),
            meta_fn: self.intern(TyKind::MetaFunc),
            meta_ty: self.intern(TyKind::MetaType),
            meta_producer_fn: self.intern(TyKind::Func(
                self.intern_slice(&[]),
                self.intern(TyKind::MetaFunc),
            )),
        })
    }
}

// === Interner Bundles === //

pub trait Internable<'gcx>: Sized + hash::Hash + Eq {
    fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx Interner<'gcx, Self>;
}

pub trait ListInternable<'gcx>: Sized + hash::Hash + Eq + Clone {
    fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx ListInterner<'gcx, Self>;
}

macro_rules! define_interners {
    (
        single {
            $($single_name:ident: $single_ty:ty),*
            $(,)?
        }

        list {
            $($list_name:ident: $list_ty:ty),*
            $(,)?
        }
    ) => {
        #[derive(Default)]
        pub struct GcxInterners<'gcx> {
            $(pub $single_name: Interner<'gcx, $single_ty>,)*
            $(pub $list_name: ListInterner<'gcx, $list_ty>,)*
        }

        $(
            impl<'gcx> Internable<'gcx> for $single_ty {
                fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx Interner<'gcx, Self> {
                    &interners.$single_name
                }
            }
        )*

        $(
            impl<'gcx> ListInternable<'gcx> for $list_ty {
                fn fetch(interners: &'gcx GcxInterners<'gcx>) -> &'gcx ListInterner<'gcx, Self> {
                    &interners.$list_name
                }
            }
        )*
    };
}

define_interners! {
    single {
        type_interner: TyKind<'gcx>,
        value_interner: ValueInner<'gcx>,
    }

    list {
        type_list_interner: Ty<'gcx>,
        value_list_interner: Value<'gcx>,
        bound_value_list_interner: BoundValue<'gcx>,
        symbol_list_interner: Symbol,
        member_list_interner: TyAdtMember<'gcx>,
    }
}
