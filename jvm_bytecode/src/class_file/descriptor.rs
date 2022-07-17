use std::marker::PhantomData;

use super::ConstantKind;

pub struct DescriptorBuilder<T> {
    inner: String,
    t: PhantomData<T>,
}

pub struct MethodDescriptor;

pub enum FieldTypeKind {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Class(String),
    Short,
    Boolean,
    ArrRef(Box<FieldTypeKind>),
}

const FIELD_TYPE_BYTE: &str = "B";
const FIELD_TYPE_CHAR: &str = "C";
const FIELD_TYPE_DOUBLE: &str = "D";
const FIELD_TYPE_FLOAT: &str = "F";
const FIELD_TYPE_INT: &str = "I";
const FIELD_TYPE_LONG: &str = "J";
const FIELD_TYPE_SHORT: &str = "S";
const FIELD_TYPE_BOOLEAN: &str = "Z";
const FIELD_TYPE_ARR_REF: &str = "[";
const RET_TYPE_VOID: &str = "V";

impl DescriptorBuilder<MethodDescriptor> {
    #[inline]
    pub fn method() -> Self {
        Self {
            inner: String::new(),
            t: PhantomData,
        }
    }

    #[inline]
    pub fn params(mut self) -> DescriptorBuilderParams<MethodDescriptor> {
        self.inner.push('(');
        DescriptorBuilderParams {
            inner: self.inner,
            t: PhantomData,
        }
    }
}

pub struct DescriptorBuilderParams<T> {
    inner: String,
    t: PhantomData<T>,
}

impl DescriptorBuilderParams<MethodDescriptor> {
    #[inline]
    pub fn byte(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_BYTE);
        self
    }

    #[inline]
    pub fn char(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_CHAR);
        self
    }

    #[inline]
    pub fn double(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_DOUBLE);
        self
    }

    #[inline]
    pub fn float(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_FLOAT);
        self
    }

    #[inline]
    pub fn int(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_INT);
        self
    }

    #[inline]
    pub fn long(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_LONG);
        self
    }

    #[inline]
    pub fn class_ref(mut self, name: &str) -> Self {
        self.inner.push_str(&format!("L{};", name)[..]);
        self
    }

    #[inline]
    pub fn short(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_SHORT);
        self
    }

    #[inline]
    pub fn bool(mut self) -> Self {
        self.inner.push_str(FIELD_TYPE_BOOLEAN);
        self
    }

    #[inline]
    pub fn arr_ref(mut self, field_type: FieldTypeKind) -> Self {
        self.inner.push_str(FIELD_TYPE_ARR_REF);
        match field_type {
            FieldTypeKind::Byte => self.byte(),
            FieldTypeKind::Char => self.char(),
            FieldTypeKind::Double => self.double(),
            FieldTypeKind::Float => self.float(),
            FieldTypeKind::Int => self.int(),
            FieldTypeKind::Long => self.long(),
            FieldTypeKind::Class(name) => self.class_ref(&name[..]),
            FieldTypeKind::Short => self.short(),
            FieldTypeKind::Boolean => self.bool(),
            FieldTypeKind::ArrRef(inner_field_type) => self.arr_ref(*inner_field_type),
        }
    }

    #[inline]
    pub fn ret_type(mut self) -> DescriptorBuilderRetType<MethodDescriptor> {
        self.inner.push(')');
        DescriptorBuilderRetType {
            inner: self.inner,
            t: PhantomData,
        }
    }
}

pub struct DescriptorBuilderRetType<T> {
    inner: String,
    t: PhantomData<T>,
}

impl DescriptorBuilderRetType<MethodDescriptor> {
    #[inline]
    pub fn byte(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_BYTE);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn char(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_CHAR);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn double(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_DOUBLE);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn float(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_FLOAT);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn int(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_INT);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn long(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_LONG);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn class_ref(mut self, name: &str) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(&format!("L{};", name)[..]);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn short(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_SHORT);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn bool(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_BOOLEAN);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn arr_ref(mut self, field_type: FieldTypeKind) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(FIELD_TYPE_ARR_REF);
        match field_type {
            FieldTypeKind::Byte => self.byte(),
            FieldTypeKind::Char => self.char(),
            FieldTypeKind::Double => self.double(),
            FieldTypeKind::Float => self.float(),
            FieldTypeKind::Int => self.int(),
            FieldTypeKind::Long => self.long(),
            FieldTypeKind::Class(name) => self.class_ref(&name[..]),
            FieldTypeKind::Short => self.short(),
            FieldTypeKind::Boolean => self.bool(),
            FieldTypeKind::ArrRef(inner_field_type) => self.arr_ref(*inner_field_type),
        }
    }

    #[inline]
    pub fn void(mut self) -> DescriptorBuilderDone<MethodDescriptor> {
        self.inner.push_str(RET_TYPE_VOID);
        DescriptorBuilderDone {
            inner: self.inner,
            t: PhantomData,
        }
    }
}

pub struct DescriptorBuilderDone<T> {
    inner: String,
    t: PhantomData<T>,
}

impl<T> DescriptorBuilderDone<T> {
    #[inline]
    pub fn descriptor(self) -> String {
        self.inner
    }

    pub fn to_const_pool_entry(self) -> ConstantKind {
        ConstantKind::utf8_from_str(&self.inner[..])
    }
}

pub trait ToDescriptor<MethodDescriptor> {
    fn to_descriptor(&self) -> String;
}
