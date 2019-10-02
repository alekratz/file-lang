use std::fmt::{self, Display, Formatter};

macro_rules! ops {
    ($($op:ident => $text:expr),* $(,)?) => {
        /// The kind of operator being represented.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum OpKind {
            $($op),*
        }

        #[allow(dead_code)]
        impl OpKind {
            pub const ALL: &'static [OpKind] = &[
                $(OpKind::$op),*
            ];

            pub const CHARS: &'static [char] = &[
                $($text),*
            ];

            pub fn from_char(c: char) -> Option<Self> {
                match c {
                    $(
                        $text => Some(Self :: $op),
                    )*
                    _ => None,
                }
            }

            pub fn to_char(&self) -> char {
                match self {
                    $(
                        Self :: $op => $text,
                    )*
                }
            }
        }
    };
}

impl Display for OpKind {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "{}", self.to_char())
    }
}

ops! {
    Bang => '!',
    At => '@',
    Dollar => '$',
    Percent => '%',
    Caret => '^',
    Amp => '&',
    Splat => '*',
    FSlash => '/',
    BSlash => '\\',
    Bar => '|',
    Less => '<',
    Greater => '>',
    Minus => '-',
    Eq => '=',
    Plus => '+',
    Tilde => '~',
}
