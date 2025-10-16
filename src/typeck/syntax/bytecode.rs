//! The shade-DSL bytecode interpreter.
//!
//! This is a stack machine that stores two stacks of [`ValuePlace`]s:
//!
//! - A **local stack** storing owned locals, function arguments, and temporaries.
//! - A **place stack** storing the references to places produced by expressions.
//!
//! Popping from the local stack deallocates a value while deallocating from the place stack has
//! no side-effects.
//!
//! Unlike other bytecode interpreters, we use a split stack to simplify manual memory management.
//! This is necessary because our VM uses manual memory management.

use std::ops::{Add, AddAssign, Sub, SubAssign};

use crate::{
    base::{ErrorGuaranteed, arena::Obj},
    parse::ast::BinOpKind,
    typeck::{
        analysis::TyCoercion,
        syntax::{AnyFuncValue, FuncInstance, Ty, ValuePlace},
    },
    utils::lang::StackConsumer,
};

// === BycFunction === //

#[derive(Debug)]
pub struct BycFunction {
    pub instance: Obj<FuncInstance>,
    pub instructions: Vec<BycInstr>,
}

// === Bytecode === //

define_bytecode! {
    /// Allocates a new local.
    Allocate { ty: Obj<Ty> } => allocate() -> local;

    /// Frees a specified number of locals.
    Deallocate { count: u32 } => deallocate(to_free: pop local[*count]);

    /// Pushes a reference to a local to the place stack.
    Reference { at: u32 } => reference(_ignore: peek local[*at], target: peek local) -> place;

    /// Copies a reference on the place stack to the top of the place stack.
    Tee { at: u32 } => tee(_ignore: peek place[*at], target: peek place) -> place;

    // TODO
    Fold { count: u32 } => fold(top: pop place, _drop: pop place[*count]) -> place;

    /// Pops the specified number of places from the **place stack**.
    Forget { count: u32 } => forget(_ignore: pop place[*count]);

    // TODO
    Coerce { coercion: TyCoercion } => coerce(src: pop place, dst: peek place);

    /// Pops the top of the **place stack** and assigns a deep clone of a constant interned value.
    AssignConst { intern: ValuePlace } => assign_const(target: peek place);

    /// Pops the top of the **place stack** and assigns a deep clone of the constant interned value
    /// obtained by evaluating the specified function instance.
    AssignConstExpr { func: Obj<FuncInstance> } => assign_const_expr(target: peek place);

    // TODO
    AssignTupleType { fields: u32 } => assign_tuple_type(
        fields: pop place[*fields],
        assign_to: peek place,
    );

    /// Pops the top two elements of the **place stack** to use as an RHS and an LHS for an
    /// assignment respectively.
    AssignCopyRhsThenLhs { } => assign_copy_rhs_then_lhs(rhs: pop place, lhs: peek place);

    /// Pops the top two elements of the **place stack** to use as an RHS and an LHS for an
    /// assignment respectively.
    AssignCopyLhsThenRhs { } => assign_copy_lhs_then_rhs(lhs: pop place, rhs: peek place);

    // TODO
    CallFixed { callee: AnyFuncValue, args: u32 } => call_fixed(
        args: peek place[*args], callee: peek place, return_to: peek place
    );

    /// Calls the function with the specified number of arguments. It is the **caller**'s
    /// responsibility clean up the **place stack** after the callee returns by calling `Forget`.
    CallDyn { args: u32 } => call_dyn(args: peek place[*args], callee: peek place, return_to: peek place);

    /// Instantiates the function with the specified number of generics.
    Instantiate { args: u32 } => instantiate(
        args: pop place[*args],
        target: pop place,
        write_to: peek place,
    );

    /// Returns to the caller.
    Return { } => return_();

    /// Pops the place at the top of the **place stack** and pushes the specified field inside the
    /// aggregate ADT onto the stack.
    AdtAggregateIndex { idx: u32 } => adt_aggregate_index(target: pop place) -> place;

    /// Pops the place at the top of the **place stack** and pushes the specified field inside the
    /// tuple ADT onto the stack.
    TupleIndex { idx: u32 } => tuple_index(target: pop place) -> place;

    /// Pops the place at the top of the stack and pushes the value inside the variant ADT onto the
    /// stack.
    AdtVariantUnwrap { } => adt_variant_unwrap(target: pop place) -> place;

    /// Pops the top two elements of the **place stack** to use as an RHS, an LHS, and uses the top
    /// of the **place stack** as the target for a binary operation respectively.
    BinOp { op: BinOpKind } => bin_op(rhs: pop place, lhs: pop place, assign_to: peek place);

    /// Performs an unconditional jump.
    Jump { addr: usize } => jump();

    /// Pops the **place stack** expecting a boolean. If the boolean is false, jumps to the target
    /// instruction.
    JumpOtherwise { addr: usize } => jump_otherwise(scrutinee: pop place);
}

// === Bytecode infrastructure === //

fn u32_to_i32(v: u32) -> i32 {
    v as i32
}

fn u32_to_usize(v: u32) -> usize {
    v as usize
}

macro_rules! define_bytecode {
    (
		$(
			$(#[$($attr:tt)*])*
            $variant:ident {
                $($field:ident: $ty:ty),* $(,)?
            } => $method:ident(
                $( $arg:ident: $op:ident $kind:ident $([$count:expr])? ),* $(,)?
            ) $(-> $($out_kind:ident)*)?;
		)*
	) => {
        #[derive(Debug, Clone)]
        pub enum BycInstr {
            $($variant( byc_instr::$variant ),)*
        }

        impl BycInstr {
            pub fn depth_delta(&self) -> BycDepth {
                match self {
                    $(Self::$variant(kind) => kind.depth_delta(),)*
                }
            }

            pub fn invoke(
                &self,
                locals: &mut Vec<ValuePlace>,
                places: &mut Vec<ValuePlace>,
                handler: &mut impl BycInstrHandler,
            ) -> Result<(), ErrorGuaranteed> {
                match self {
                    $(Self::$variant(kind) => kind.invoke(locals, places, handler),)*
                }
            }
        }

        pub mod byc_instr {
            #[allow(unused)]
            use super::*;

            $(
                #[derive(Debug, Clone)]
                pub struct $variant {
                    $(pub $field: $ty),*
                }

                #[allow(unused, clippy::unused_unit, clippy::ptr_arg)]
                impl $variant {
                    pub fn depth_delta(&self) -> BycDepth {
                        let Self { $($field),* } = self;

                        BycDepth::ZERO $(- define_bytecode!(@get_depth $op $kind $([$count])?))*
                            $($(+ define_bytecode!(@get_res_depth $out_kind))*)?
                    }

                    pub fn read_stacks<'a>(
                        &self,
                        locals: &'a StackConsumer<ValuePlace>,
                        places: &'a StackConsumer<ValuePlace>,
                    ) -> ( $(define_bytecode!(@get_arg_type $([$count])?),)* ) {
                        let Self { $($field),* } = self;

                        (
                            $( define_bytecode!(@perform_op locals places => $op $kind $([$count])?), )*
                        )
                    }

                    pub fn push_stacks(
                        locals: &mut Vec<ValuePlace>,
                        places: &mut Vec<ValuePlace>,
                        outputs: define_bytecode!(@get_ret_type $($($out_kind)*)?),
                    ) {
                        let i = 0;

                        $($(
                            define_bytecode!(@push_ret locals places $out_kind outputs[i]);
                            let i = i + 1;
                        )*)?
                    }

                    pub fn invoke(
                        &self,
                        locals: &mut Vec<ValuePlace>,
                        places: &mut Vec<ValuePlace>,
                        handler: &mut impl BycInstrHandler,
                    ) -> Result<(), ErrorGuaranteed> {
                        let locals_in = StackConsumer::new(locals);
                        let places_in = StackConsumer::new(places);

                        let ($($arg,)*) = self.read_stacks(&locals_in, &places_in);

                        let res = handler.$method(self, $($arg,)*)?;

                        drop(locals_in);
                        drop(places_in);

                        Self::push_stacks(locals, places, res);

                        Ok(())
                    }
                }

                impl From<$variant> for BycInstr {
                    fn from(v: $variant) -> BycInstr {
                        BycInstr::$variant(v)
                    }
                }
            )*
        }

        pub trait BycInstrHandler {
            $(
                #[allow(clippy::needless_lifetimes)]
                fn $method<'a>(
                    &mut self,
                    instr: &'a byc_instr::$variant,
                    $($arg: define_bytecode!(@get_arg_type $([$count])?) ,)*
                ) -> Result<define_bytecode!(@get_ret_type $($($out_kind)*)?), ErrorGuaranteed>;
            )*
        }
    };

    // === `get_depth` === //

    (@get_depth peek $kind:ident $([$count:expr])?) => {
        BycDepth::ZERO
    };
    (@get_depth pop local) => {
        BycDepth::new_local(1)
    };
    (@get_depth pop place) => {
        BycDepth::new_place(1)
    };
    (@get_depth pop local [$count:expr]) => {
        BycDepth::new_local(u32_to_i32($count))
    };
    (@get_depth pop place [$count:expr]) => {
        BycDepth::new_place(u32_to_i32($count))
    };
    (@get_depth pop $kind:ident$([$count:expr])?) => {
        compile_error!(concat!("unknown kind `", stringify!($kind), "`"))
    };
    (@get_depth $op:ident $kind:ident$([$count:expr])?) => {
        compile_error!(concat!("unknown op `", stringify!($op), "`"))
    };

    // === `get_res_depth` === //

    (@get_res_depth local) => {
        BycDepth::new_local(1)
    };
    (@get_res_depth place) => {
        BycDepth::new_place(1)
    };

    // === `get_arg_type` === //

    (@get_arg_type) => { ValuePlace };
    (@get_arg_type [$count:expr]) => { &'a [ValuePlace] };

    // === `perform_op` === //

    (@perform_op $locals:ident $places:ident => peek local [$count:expr]) => {
        $locals.peek_many(u32_to_usize($count)).unwrap()
    };
    (@perform_op $locals:ident $places:ident => peek local) => {
        *$locals.peek().unwrap()
    };
    (@perform_op $locals:ident $places:ident => peek place [$count:expr]) => {
        $places.peek_many(u32_to_usize($count)).unwrap()
    };
    (@perform_op $locals:ident $places:ident => peek place) => {
        *$places.peek().unwrap()
    };
    (@perform_op $locals:ident $places:ident => pop local [$count:expr]) => {
        $locals.pop_many(u32_to_usize($count)).unwrap()
    };
    (@perform_op $locals:ident $places:ident => pop local) => {
        *$locals.pop().unwrap()
    };
    (@perform_op $locals:ident $places:ident => pop place [$count:expr]) => {
        $places.pop_many(u32_to_usize($count)).unwrap()
    };
    (@perform_op $locals:ident $places:ident => pop place) => {
        *$places.pop().unwrap()
    };

    // === `get_ret_type` === //

    (@get_ret_type $($kind:ident)*) => {
        [ValuePlace; 0 $(+ { let $kind = (); _ = $kind; 1 })*]
    };

    // === `push_ret` === //

    (@push_ret $locals:ident $places:ident local $output:expr) => {
        $locals.push($output);
    };
    (@push_ret $locals:ident $places:ident place $output:expr) => {
        $places.push($output);
    };
    (@push_ret $locals:ident $places:ident $unknown:ident $output:expr) => {
        compile_error!(concat!("unknown return position `", stringify!($unknown), "`"));
    };
}

use define_bytecode;

// === BycDepth === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Default)]
pub struct BycDepth {
    pub local: i32,
    pub place: i32,
}

impl BycDepth {
    pub const ZERO: Self = Self { local: 0, place: 0 };

    pub const fn new_local(v: i32) -> Self {
        Self { local: v, place: 0 }
    }

    pub const fn new_place(v: i32) -> Self {
        Self { local: 0, place: v }
    }
}

impl Add for BycDepth {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            local: self.local + rhs.local,
            place: self.place + rhs.place,
        }
    }
}

impl Sub for BycDepth {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            local: self.local - rhs.local,
            place: self.place - rhs.place,
        }
    }
}

impl AddAssign for BycDepth {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for BycDepth {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}
