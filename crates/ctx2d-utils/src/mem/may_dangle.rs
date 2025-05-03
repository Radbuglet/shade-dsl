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

pub struct MayDangle<D: ?Sized + TypeWrapper> {
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

impl<D: ?Sized + TypeWrapper> MayDangle<D> {
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

impl<D: ?Sized + TypeWrapper> Deref for MayDangle<D> {
    type Target = D::Output;

    fn deref(&self) -> &Self::Target {
        unsafe { self.value.inner.cast::<D::Output>().as_ref() }
    }
}

impl<D: ?Sized + TypeWrapper> DerefMut for MayDangle<D> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.value.inner.cast::<D::Output>().as_mut() }
    }
}

#[macro_export]
macro_rules! my_may_dangle {
    ($ty:ty) => {
        $crate::mem::MayDangle<dyn $crate::mem::TypeWrapper<Output = $ty>>
    };
}

pub use my_may_dangle;

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
/// use ctx2d_utils::mem::{my_may_dangle, MayDangle};
///
/// struct Demo<'a> {
///     map: RefCell<my_may_dangle!(HashSet<&'a ()>)>,
/// }
///
/// fn entry() {
///     let demo = Demo {
///         map: RefCell::new(unsafe { MayDangle::new(HashSet::default()) }),
///     };
///     callee(&demo);
/// }
///
/// fn callee<'a>(_demo: &'a Demo<'a>) {}
/// ```
#[cfg(doctest)]
pub fn expect_success() {}
