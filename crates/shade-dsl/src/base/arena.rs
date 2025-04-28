pub trait Annotatable<V: ?Sized> {}

#[derive(Debug, Default)]
pub struct Arena {}

impl Arena {
    pub fn alloc<T>(&self, value: T) -> &T {
        todo!()
    }

    pub fn annotate<T, V>(&self, target: &T, value: &V)
    where
        T: Annotatable<V>,
        T: ?Sized,
        V: ?Sized,
    {
        todo!()
    }

    pub fn lookup<T, V>(&self, target: &T) -> &V
    where
        T: Annotatable<V>,
        T: ?Sized,
        V: ?Sized,
    {
        todo!()
    }
}
