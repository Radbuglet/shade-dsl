use std::{cell::RefCell, sync::Arc};

use late_struct::{LateField, LateInstance, late_struct};

#[derive(Debug, Clone, Default)]
pub struct Session(Arc<LateInstance<Self>>);

thread_local! {
    static SESSION: RefCell<Option<Session>> = const { RefCell::new(None) };
}

late_struct!(Session);

impl Session {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get<T: LateField<Session>>(&self) -> &T::Value {
        self.0.get::<T>()
    }

    #[must_use]
    pub fn bind(self) -> impl Drop {
        let old = SESSION.replace(Some(self));

        scopeguard::guard(old, move |old| {
            SESSION.set(old);
        })
    }

    pub fn fetch() -> Session {
        SESSION
            .with_borrow(|v| v.clone())
            .expect("no session was ever bound")
    }
}
