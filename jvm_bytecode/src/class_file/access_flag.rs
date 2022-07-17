use std::marker::PhantomData;

pub struct AccessFlagBuilder<T> {
    flag: u16,
    t: PhantomData<T>,
}

impl<T> AccessFlagBuilder<T> {
    pub fn flag(&self) -> u16 {
        self.flag
    }
}

pub struct ClassAccessFlags;

const CLASS_ACC_PUBLIC: u16 = 0x0001;
const CLASS_ACC_FINAL: u16 = 0x0010;
const CLASS_ACC_SUPER: u16 = 0x0020;
const CLASS_ACC_INTERFACE: u16 = 0x0200;
const CLASS_ACC_ABSTRACT: u16 = 0x0400;
const CLASS_ACC_SYNTHETIC: u16 = 0x1000;
const CLASS_ACC_ANNOTATION: u16 = 0x2000;
const CLASS_ACC_ENUM: u16 = 0x4000;
const CLASS_ACC_MODULE: u16 = 0x8000;

impl AccessFlagBuilder<ClassAccessFlags> {
    #[inline]
    pub fn class() -> Self {
        Self {
            flag: 0,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn acc_public(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_PUBLIC);
        self
    }

    #[inline]
    pub fn not_acc_public(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_PUBLIC);
        self
    }

    #[inline]
    pub fn acc_final(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_FINAL);
        self
    }

    #[inline]
    pub fn not_acc_final(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_FINAL);
        self
    }

    #[inline]
    pub fn acc_super(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_SUPER);
        self
    }

    #[inline]
    pub fn not_acc_super(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_SUPER);
        self
    }

    #[inline]
    pub fn acc_interface(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_INTERFACE);
        self
    }

    #[inline]
    pub fn not_acc_interface(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_INTERFACE);
        self
    }

    #[inline]
    pub fn acc_abstract(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_ABSTRACT);
        self
    }

    #[inline]
    pub fn not_acc_abstract(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_ABSTRACT);
        self
    }

    #[inline]
    pub fn acc_synthetic(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_SYNTHETIC);
        self
    }

    #[inline]
    pub fn not_acc_synthetic(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_SYNTHETIC);
        self
    }

    #[inline]
    pub fn acc_annotation(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_ANNOTATION);
        self
    }

    #[inline]
    pub fn not_acc_annotation(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_ANNOTATION);
        self
    }

    #[inline]
    pub fn acc_enum(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_ENUM);
        self
    }

    #[inline]
    pub fn not_acc_enum(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_ENUM);
        self
    }

    #[inline]
    pub fn acc_module(mut self) -> Self {
        set_u16_flag(&mut self.flag, CLASS_ACC_MODULE);
        self
    }

    #[inline]
    pub fn not_acc_module(mut self) -> Self {
        unset_u16_flag(&mut self.flag, CLASS_ACC_MODULE);
        self
    }
}

pub struct MethodAccessFlags;

const METHOD_ACC_PUBLIC: u16 = 0x0001;
const METHOD_ACC_PRIVATE: u16 = 0x0002;
const METHOD_ACC_PROTECTED: u16 = 0x0004;
const METHOD_ACC_STATIC: u16 = 0x0008;
const METHOD_ACC_FINAL: u16 = 0x0010;
const METHOD_ACC_SYNCHRONIZED: u16 = 0x0020;
const METHOD_ACC_BRIDGE: u16 = 0x0040;
const METHOD_ACC_VARARGS: u16 = 0x0080;
const METHOD_ACC_NATIVE: u16 = 0x0100;
const METHOD_ACC_ABSTRACT: u16 = 0x0400;
const METHOD_ACC_STRICT: u16 = 0x0800;
const METHOD_ACC_SYNTHETIC: u16 = 0x1000;

impl AccessFlagBuilder<MethodAccessFlags> {
    #[inline]
    pub fn method() -> Self {
        Self {
            flag: 0,
            t: PhantomData,
        }
    }

    #[inline]
    pub fn acc_public(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_PUBLIC);
        self
    }

    #[inline]
    pub fn not_acc_public(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_PUBLIC);
        self
    }

    #[inline]
    pub fn acc_private(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_PRIVATE);
        self
    }

    #[inline]
    pub fn not_acc_private(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_PRIVATE);
        self
    }

    #[inline]
    pub fn acc_protected(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_PROTECTED);
        self
    }

    #[inline]
    pub fn not_acc_protected(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_PROTECTED);
        self
    }

    #[inline]
    pub fn acc_static(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_STATIC);
        self
    }

    #[inline]
    pub fn not_acc_static(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_STATIC);
        self
    }

    #[inline]
    pub fn acc_final(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_FINAL);
        self
    }

    #[inline]
    pub fn not_acc_final(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_FINAL);
        self
    }

    #[inline]
    pub fn acc_synchronized(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_SYNCHRONIZED);
        self
    }

    #[inline]
    pub fn not_acc_synchronized(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_SYNCHRONIZED);
        self
    }

    #[inline]
    pub fn acc_bridge(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_BRIDGE);
        self
    }

    #[inline]
    pub fn not_acc_bridge(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_BRIDGE);
        self
    }

    #[inline]
    pub fn acc_varargs(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_VARARGS);
        self
    }

    #[inline]
    pub fn not_acc_varargs(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_VARARGS);
        self
    }

    #[inline]
    pub fn acc_native(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_NATIVE);
        self
    }

    #[inline]
    pub fn not_acc_native(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_NATIVE);
        self
    }

    #[inline]
    pub fn acc_abstract(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_ABSTRACT);
        self
    }

    #[inline]
    pub fn not_acc_abstract(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_ABSTRACT);
        self
    }

    #[inline]
    pub fn acc_strict(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_STRICT);
        self
    }

    #[inline]
    pub fn not_acc_strict(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_STRICT);
        self
    }

    #[inline]
    pub fn acc_synthetic(mut self) -> Self {
        set_u16_flag(&mut self.flag, METHOD_ACC_SYNTHETIC);
        self
    }

    #[inline]
    pub fn not_acc_synthetic(mut self) -> Self {
        unset_u16_flag(&mut self.flag, METHOD_ACC_SYNTHETIC);
        self
    }
}

#[inline]
fn set_u16_flag(val: &mut u16, flag: u16) {
    *val |= flag;
}

#[inline]
fn unset_u16_flag(val: &mut u16, flag: u16) {
    let mask = !flag;
    *val &= mask;
}
