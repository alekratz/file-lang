#[macro_use]
pub mod span;
pub mod binding;
pub mod util;

pub mod types {
    use std::collections::HashMap;
    pub type Mapping<K, V> = HashMap<K, V>;
    pub use super::util::TypeMap;
}

pub mod prelude {
    pub use super::binding::*;
    pub use super::span::*;
    pub use super::types::*;
    pub use super::util::*;
}
