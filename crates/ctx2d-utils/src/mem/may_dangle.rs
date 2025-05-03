use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

// === Core === //

mod sealed {
    pub trait NeverImplemented {}
}

pub trait TypeWrapper: sealed::NeverImplemented {
    type Output;
}

pub struct NoDangle<D: ?Sized + TypeWrapper> {
    _ty: PhantomData<fn(D) -> D>,
    value: DropHelper,
}

struct DropHelper {
    inner: NonNull<()>,
    dtor: unsafe fn(NonNull<()>),
}

impl Drop for DropHelper {
    fn drop(&mut self) {
        unsafe { (self.dtor)(self.inner) }
    }
}

impl<D: ?Sized + TypeWrapper> NoDangle<D> {
    pub unsafe fn new(value: D::Output) -> Self {
        unsafe {
            let value = Box::into_raw(Box::new(value));
            let value = DropHelper {
                inner: NonNull::new_unchecked(value).cast(),
                dtor: |ptr| drop(Box::from_raw(ptr.cast::<D::Output>().as_ptr())),
            };

            Self {
                _ty: PhantomData,
                value,
            }
        }
    }
}

impl<D: ?Sized + TypeWrapper> Deref for NoDangle<D> {
    type Target = D::Output;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.inner.cast::<D::Output>().as_ref() }
    }
}

impl<D: ?Sized + TypeWrapper> DerefMut for NoDangle<D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.inner.cast::<D::Output>().as_mut() }
    }
}

#[macro_export]
macro_rules! no_dangle {
    ($ty:ty) => {
        $crate::mem::NoDangle<dyn $crate::mem::TypeWrapper<Output = $ty>>
    };
}

pub use no_dangle;

// === Tests === //

/// ```compile_fail,E0597
/// use hashbrown::HashSet;
///
/// use std::cell::RefCell;
///
/// struct Demo<'a> {
///     map: RefCell<HashSet<&'a ()>>,
/// }
///
/// fn entry() {
///     let demo = Demo {
///         map: Default::default(),
///     };
///     callee(&demo);
/// }
///
/// fn callee<'a>(_demo: &'a Demo<'a>) {}
/// ```
#[cfg(doctest)]
pub fn expect_fail() {}

/// ```
/// use hashbrown::HashSet;
///
/// use std::cell::RefCell;
///
/// use ctx2d_utils::mem::{no_dangle, NoDangle};
///
/// struct Demo<'a> {
///     map: RefCell<no_dangle!(HashSet<&'a ()>)>,
/// }
///
/// fn entry() {
///     let demo = Demo {
///         map: RefCell::new(unsafe { NoDangle::new(HashSet::default()) }),
///     };
///     callee(&demo);
/// }
///
/// fn callee<'a>(_demo: &'a Demo<'a>) {}
/// ```
#[cfg(doctest)]
pub fn expect_success() {}
