macro_rules! make_literal_types {
    ($(($name:ident, $type:ty) $(,)?)*) => {
        $(
            #[derive(Debug, Clone)]
            pub(crate) struct $name {
                pub value: $type,
                pub is_return: bool,
            }
            impl $name {
                pub fn new(value: $type) -> Self {
                    Self {
                        value,
                        is_return: false,
                    }
                }
            }
            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_fmt(format_args!("{:?}", self.value))
                }
            }
            impl ObjectTrait for $name {
                fn inner(&self) -> &dyn std::any::Any {
                    &self.value
                }
                fn set_return(&mut self) {
                    self.is_return = true;
                }
                fn is_return(&self) -> bool {
                    self.is_return
                }
            }
        )*
        #[enum_dispatch]
        #[derive(Debug, Clone)]
        pub(crate) enum Object {
            $(
                $name,
            )*
        }
        impl Display for Object {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!(
                    "{}",
                    match self {
                        $(
                            Object::$name(inner) => format!("{inner}"),
                        )*
                    }
                ))
            }
        }
    };
}