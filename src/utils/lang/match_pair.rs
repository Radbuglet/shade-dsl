#[macro_export]
macro_rules! match_pair {
    ($tuple:expr => {
        $(($lhs:pat, $rhs:pat $(,)?) $(| ($lhs_alt:pat, $rhs_alt:pat $(,)?))* => { $($body:tt)* } $(,)? )*
        _ => { $($fallback:tt)* }
    }) => {{
        let tuple = $tuple;

        #[allow(unused)]
        if 0u32 != 0u32 {
            let (lhs, rhs) = tuple;

            match lhs {
                $($lhs $(| $lhs_alt)* => {})*
            }

            match rhs {
                $($rhs $(| $rhs_alt)* => {})*
            }

            loop {}
        } else {
            match tuple {
                $(($lhs, $rhs) $(| ($lhs_alt, $rhs_alt))* => { $($body)* },)*
                _ => { $($fallback)* },
            }
        }
    }};
}

pub use match_pair;
