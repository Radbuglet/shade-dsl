use std::{
    fmt,
    num::NonZeroU32,
    sync::{OnceLock, RwLock},
};

use bumpalo::Bump;

// === Symbol === //

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Symbol(NonZeroU32);

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}#{}", self.as_str(), self.0)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Symbol {
    pub fn new(value: &str) -> Self {
        SymbolInterner::global().intern(value)
    }

    pub fn as_str(self) -> &'static str {
        SymbolInterner::global().lookup(self)
    }
}

// === SymbolInterner === //

#[derive(Default)]
pub struct SymbolInterner(RwLock<SymbolInternInner>);

#[derive(Default)]
struct SymbolInternInner {
    arena: Bump,
    symbols: Vec<*const str>,
    symbol_map: FxHashMap<(u64, *const str), Symbol>,
}

unsafe impl Send for SymbolInterner {}
unsafe impl Sync for SymbolInterner {}

impl fmt::Debug for SymbolInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SymbolInterner").finish_non_exhaustive()
    }
}

impl SymbolInterner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn global() -> &'static SymbolInterner {
        static INTERNER: OnceLock<SymbolInterner> = OnceLock::new();

        INTERNER.get_or_init(Self::new)
    }

    pub fn intern(&self, value: &str) -> Symbol {
        let hash = fx_hash_one(value);

        // Fast path: return existing symbol
        let inner = self.0.read().unwrap();

        if let Some((_key, sym)) =
            inner
                .symbol_map
                .raw_entry()
                .from_hash(hash, |&(candidate_hash, candidate)| {
                    candidate_hash == hash && unsafe { *candidate == *value }
                })
        {
            return *sym;
        }

        drop(inner);

        // Slow path: create a new symbol
        let mut inner = self.0.write().unwrap();
        let inner = &mut *inner;

        let entry =
            inner
                .symbol_map
                .raw_entry_mut()
                .from_hash(hash, |&(candidate_hash, candidate)| {
                    candidate_hash == hash && unsafe { *candidate == *value }
                });

        match entry {
            hash_map::RawEntryMut::Occupied(entry) => *entry.get(),
            hash_map::RawEntryMut::Vacant(entry) => {
                let str = inner.arena.alloc_str(value);
                let sym = Symbol(
                    u32::try_from(inner.symbols.len() + 1)
                        .ok()
                        .and_then(NonZeroU32::new)
                        .expect("too many symbols"),
                );
                inner.symbols.push(str);
                entry.insert_with_hasher(hash, (hash, str), sym, |&(hash, _)| hash);
                sym
            }
        }
    }

    pub fn lookup(&self, sym: Symbol) -> &str {
        let inner = self.0.read().unwrap();

        unsafe { &*inner.symbols[(sym.0.get() - 1) as usize] }
    }
}

// === `symbol` macro === //

#[doc(hidden)]
pub mod symbol_internals {
    use std::{
        num::NonZeroU32,
        sync::atomic::{AtomicU32, Ordering::*},
    };

    pub use {super::Symbol, std::primitive::str};

    pub use super::symbol;

    pub struct LateSymbol {
        id: AtomicU32,
    }

    impl LateSymbol {
        #[allow(clippy::new_without_default)] // (macro internal)
        pub const fn new() -> Self {
            Self {
                id: AtomicU32::new(0),
            }
        }

        pub fn get(&self, init: &str) -> Symbol {
            if let Some(sym) = NonZeroU32::new(self.id.load(Relaxed)) {
                return Symbol(sym);
            }

            let sym = Symbol::new(init);
            self.id.store(sym.0.get(), Relaxed);
            sym
        }
    }
}

#[macro_export]
macro_rules! symbol {
    (fn $text:expr) => {{
        fn _symbol_getter() -> $crate::base::symbol_internals::Symbol {
            static LATE: $crate::base::symbol_internals::LateSymbol =
                $crate::base::symbol_internals::LateSymbol::new();

            const INIT: &'static $crate::base::symbol_internals::str = $text;

            LATE.get(INIT)
        }

        _symbol_getter
    }};
    ($text:expr) => {{
        $crate::base::symbol_internals::symbol!(fn $text)()
    }};
}

use ctx2d_utils::hash::{FxHashMap, fx_hash_one, hash_map};
pub use symbol;
