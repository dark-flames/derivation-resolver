pub mod common;
mod eval_ml_3;
mod eval_ml_4;
mod poly_ml_4;

#[cfg(feature = "eval-ml-3")]
pub use eval_ml_3::EvalML3 as DerivationSystem;
#[cfg(feature = "eval-ml-4")]
pub use eval_ml_4::EvalML4 as DerivationSystem;
#[cfg(feature = "poly-ml-4")]
pub use poly_ml_4::PolyML4 as DerivationSystem;
