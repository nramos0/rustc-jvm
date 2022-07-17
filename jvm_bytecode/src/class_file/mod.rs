use std::io::Write;

pub mod access_flag;
pub mod const_pool;
pub mod descriptor;

use const_pool::*;

use crate::instructions::Instruction;

use self::{
    access_flag::{AccessFlagBuilder, MethodAccessFlags},
    descriptor::{DescriptorBuilder, FieldTypeKind},
};

#[derive(Debug)]
pub struct ClassFile {
    /// Magic number: 0xCAFEBABE
    pub magic: u32,
    pub minor_version: u16,
    pub major_version: u16,
    // pub constant_pool_count: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    // pub interfaces_count: u16,
    pub interfaces: Vec<u16>,
    // pub fields_count: u16,
    pub fields: Vec<Field>,
    // pub methods_count: u16,
    pub methods: Vec<Method>,
    // pub attributes_count: u16,
    pub attributes: Vec<Attribute>,
}

impl ClassFile {
    pub fn write_to(&self, mut stream: impl Write) -> std::io::Result<()> {
        self.write_magic_num(&mut stream)?;
        self.write_minor_ver(&mut stream)?;
        self.write_major_ver(&mut stream)?;
        self.write_constant_pool_count(&mut stream)?;
        self.write_constant_pool(&mut stream)?;
        self.write_access_flags(&mut stream)?;
        self.write_this_class(&mut stream)?;
        self.write_super_class(&mut stream)?;
        self.write_interfaces_count(&mut stream)?;
        self.write_interfaces(&mut stream)?;
        self.write_fields_count(&mut stream)?;
        self.write_fields(&mut stream)?;
        self.write_methods_count(&mut stream)?;
        self.write_methods(&mut stream)?;
        self.write_attributes_count(&mut stream)?;
        self.write_attributes(&mut stream)?;
        Ok(())
    }

    pub fn write_magic_num(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u32(self.magic, stream)?;
        Ok(())
    }

    pub fn write_minor_ver(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.minor_version, stream)?;
        Ok(())
    }

    pub fn write_major_ver(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.major_version, stream)?;
        Ok(())
    }

    pub fn write_constant_pool_count(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(
            (self.constant_pool.pool.len() + 1).try_into().unwrap(),
            stream,
        )?;
        Ok(())
    }

    pub fn write_constant_pool(&self, stream: &mut impl Write) -> std::io::Result<()> {
        for constant in self
            .constant_pool
            .pool
            .iter()
            .filter(|c| !matches!(c, ConstantKind::Placeholder))
        {
            constant.write_to(stream)?;
        }
        Ok(())
    }

    pub fn write_access_flags(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.access_flags, stream)?;
        Ok(())
    }

    pub fn write_this_class(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.this_class, stream)?;
        Ok(())
    }

    pub fn write_super_class(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.super_class, stream)?;
        Ok(())
    }

    pub fn write_interfaces_count(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.interfaces.len().try_into().unwrap(), stream)?;
        Ok(())
    }

    pub fn write_interfaces(&self, stream: &mut impl Write) -> std::io::Result<()> {
        for interface_ref in &self.interfaces {
            write_u16(*interface_ref, stream)?;
        }
        Ok(())
    }

    pub fn write_fields_count(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.fields.len().try_into().unwrap(), stream)?;
        Ok(())
    }

    pub fn write_fields(&self, stream: &mut impl Write) -> std::io::Result<()> {
        for field in &self.fields {
            field.write_to(stream)?;
        }
        Ok(())
    }

    pub fn write_methods_count(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.methods.len().try_into().unwrap(), stream)?;
        Ok(())
    }

    pub fn write_methods(&self, stream: &mut impl Write) -> std::io::Result<()> {
        for method in &self.methods {
            method.write_to(stream)?;
        }
        Ok(())
    }

    pub fn write_attributes_count(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.attributes.len().try_into().unwrap(), stream)?;
        Ok(())
    }

    pub fn write_attributes(&self, stream: &mut impl Write) -> std::io::Result<()> {
        for attribute in &self.attributes {
            attribute.write_to(stream)?;
        }
        Ok(())
    }
}

#[inline]
pub fn write_u16(val: u16, stream: &mut impl Write) -> std::io::Result<()> {
    stream.write_all(&val.to_be_bytes()[..])?;
    Ok(())
}

#[inline]
pub fn write_u32(val: u32, stream: &mut impl Write) -> std::io::Result<()> {
    stream.write_all(&val.to_be_bytes()[..])?;
    Ok(())
}

pub struct ClassFileBuilder(ConstantPool);

impl ClassFileBuilder {
    pub fn build() -> Self {
        ClassFileBuilder(ConstantPool::new())
    }

    pub fn version(self, minor_version: u16, major_version: u16) -> ClassFileBuilderVerison {
        ClassFileBuilderVerison {
            minor_version,
            major_version,
            constant_pool: self.0,
        }
    }
}

pub struct ClassFileBuilderVerison {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
}

impl ClassFileBuilderVerison {
    pub fn access_flags(self, access_flags: u16) -> ClassFileBuilderAccessFlags {
        ClassFileBuilderAccessFlags {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags,
        }
    }
}

pub struct ClassFileBuilderAccessFlags {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
}

impl ClassFileBuilderAccessFlags {
    pub fn this_class(mut self, class_name: &str) -> ClassFileBuilderThisClass {
        let name_index = self
            .constant_pool
            .push(ConstantKind::utf8_from_str(class_name));

        let this_class = self
            .constant_pool
            .push(ConstantKind::Class(ClassConstant { name_index }));

        ClassFileBuilderThisClass {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class,
        }
    }
}

pub struct ClassFileBuilderThisClass {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
}

impl ClassFileBuilderThisClass {
    pub fn super_class(mut self, super_class: &str) -> ClassFileBuilderSuperClass {
        let name_index = self
            .constant_pool
            .push(ConstantKind::utf8_from_str(super_class));

        let super_class = self
            .constant_pool
            .push(ConstantKind::Class(ClassConstant { name_index }));

        ClassFileBuilderSuperClass {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class,
        }
    }
}

pub struct ClassFileBuilderSuperClass {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
}

impl ClassFileBuilderSuperClass {
    pub fn interfaces(self) -> ClassFileBuilderInterfaces {
        ClassFileBuilderInterfaces {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class: self.super_class,
            interfaces: vec![],
        }
    }
}

pub struct ClassFileBuilderInterfaces {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
}

impl ClassFileBuilderInterfaces {
    pub fn fields(self) -> ClassFileBuilderFields {
        ClassFileBuilderFields {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class: self.super_class,
            interfaces: self.interfaces,
            fields: vec![],
        }
    }
}

pub struct ClassFileBuilderFields {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<Field>,
}

impl ClassFileBuilderFields {
    pub fn methods(self) -> ClassFileBuilderMethods {
        ClassFileBuilderMethods {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class: self.super_class,
            interfaces: self.interfaces,
            fields: self.fields,
            methods: vec![],
        }
    }
}
pub struct ClassFileBuilderMethods {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl ClassFileBuilderMethods {
    pub fn attributes(self) -> ClassFileBuilderAttributes {
        ClassFileBuilderAttributes {
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class: self.super_class,
            interfaces: self.interfaces,
            fields: self.fields,
            methods: self.methods,
            attributes: vec![],
        }
    }

    pub fn method<F>(mut self, gen_method: F) -> Self
    where
        F: FnOnce(&mut ClassFileBuilderMethods) -> Method,
    {
        let method = gen_method(&mut self);
        self.methods.push(method);
        self
    }

    pub fn method_by_ref<F>(&mut self, gen_method: F)
    where
        F: FnOnce(&mut ClassFileBuilderMethods) -> Method,
    {
        let method = gen_method(self);
        self.methods.push(method);
    }
}

pub struct ClassFileBuilderAttributes {
    pub minor_version: u16,
    pub major_version: u16,
    pub constant_pool: ConstantPool,
    pub access_flags: u16,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    pub attributes: Vec<Attribute>,
}

impl ClassFileBuilderAttributes {
    pub fn to_class_file(self) -> ClassFile {
        ClassFile {
            magic: 0xCAFEBABE,
            minor_version: self.minor_version,
            major_version: self.major_version,
            constant_pool: self.constant_pool,
            access_flags: self.access_flags,
            this_class: self.this_class,
            super_class: self.super_class,
            interfaces: self.interfaces,
            fields: self.fields,
            methods: self.methods,
            attributes: self.attributes,
        }
    }
}

#[derive(Debug)]
pub struct Field {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<Attribute>,
}

impl Field {
    pub fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.access_flags, stream)?;
        write_u16(self.name_index, stream)?;
        write_u16(self.descriptor_index, stream)?;
        write_u16(self.attributes.len().try_into().unwrap(), stream)?;
        for attribute in &self.attributes {
            attribute.write_to(stream)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct Method {
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<Attribute>,
}

impl WriteTo for Method {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.access_flags, stream)?;
        write_u16(self.name_index, stream)?;
        write_u16(self.descriptor_index, stream)?;
        write_u16(self.attributes.len().try_into().unwrap(), stream)?;
        for attribute in &self.attributes {
            attribute.write_to(stream)?;
        }
        Ok(())
    }
}

lazy_static::lazy_static! {
    static ref MAIN_METHOD_DESCRIPTOR: String = DescriptorBuilder::method()
        .params()
        .arr_ref(FieldTypeKind::Class("java/lang/String".to_string()))
        .ret_type()
        .void()
        .descriptor();
}
pub struct MethodBuilder<'a> {
    pub class_ref: &'a mut ClassFileBuilderMethods,
}

impl<'a> MethodBuilder<'a> {
    pub fn build(class_ref: &'a mut ClassFileBuilderMethods) -> Self {
        MethodBuilder { class_ref }
    }

    pub fn main_method(class_ref: &'a mut ClassFileBuilderMethods) -> MethodBuilderAttributes {
        MethodBuilder::build(class_ref)
            .access_flags(AccessFlagBuilder::method().acc_public().acc_static())
            .name("main")
            .descriptor(&MAIN_METHOD_DESCRIPTOR)
            .attributes()
    }

    pub fn access_flags(
        self,
        access_flags: AccessFlagBuilder<MethodAccessFlags>,
    ) -> MethodBuilderAccessFlags<'a> {
        MethodBuilderAccessFlags {
            class_ref: self.class_ref,
            access_flags: access_flags.flag(),
        }
    }
}

pub struct MethodBuilderAccessFlags<'a> {
    pub class_ref: &'a mut ClassFileBuilderMethods,
    pub access_flags: u16,
}

impl<'a> MethodBuilderAccessFlags<'a> {
    pub fn name(self, name: &'a str) -> MethodBuilderName {
        let name_index = self
            .class_ref
            .constant_pool
            .push(ConstantKind::utf8_from_str(name));

        MethodBuilderName {
            class_ref: self.class_ref,
            access_flags: self.access_flags,
            name_index,
        }
    }
}

pub struct MethodBuilderName<'a> {
    pub class_ref: &'a mut ClassFileBuilderMethods,
    pub access_flags: u16,
    pub name_index: u16,
}

impl<'a> MethodBuilderName<'a> {
    pub fn descriptor(self, descriptor: &'a str) -> MethodBuilderDescriptor {
        let descriptor_index = self
            .class_ref
            .constant_pool
            .push(ConstantKind::utf8_from_str(descriptor));

        MethodBuilderDescriptor {
            class_ref: self.class_ref,
            access_flags: self.access_flags,
            name_index: self.name_index,
            descriptor_index,
        }
    }
}

pub struct MethodBuilderDescriptor<'a> {
    pub class_ref: &'a mut ClassFileBuilderMethods,
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
}

impl<'a> MethodBuilderDescriptor<'a> {
    pub fn attributes(self) -> MethodBuilderAttributes<'a> {
        MethodBuilderAttributes {
            class_ref: self.class_ref,
            access_flags: self.access_flags,
            name_index: self.name_index,
            descriptor_index: self.descriptor_index,
            attributes: vec![],
        }
    }
}

pub struct MethodBuilderAttributes<'a> {
    pub class_ref: &'a mut ClassFileBuilderMethods,
    pub access_flags: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<Attribute>,
}

pub struct NameDesc {
    pub name_index: u16,
    pub descriptor_index: u16,
}

impl<'a> MethodBuilderAttributes<'a> {
    pub fn attribute<F>(mut self, gen_attr: F) -> Self
    where
        F: FnOnce(&mut MethodBuilderAttributes) -> Attribute,
    {
        let attr = gen_attr(&mut self);
        self.attributes.push(attr);
        self
    }

    pub fn code<F>(mut self, gen_attr: F) -> Self
    where
        F: FnOnce(&mut MethodBuilderAttributes) -> CodeAttribute,
    {
        let attribute_name_index = self.class_ref.constant_pool.get_utf8_index("Code");
        let code_attr = gen_attr(&mut self);
        self.attributes.push(Attribute {
            attribute_name_index,
            kind: AttributeKind::Code(code_attr),
        });
        self
    }

    pub fn to_method(self) -> Method {
        Method {
            access_flags: self.access_flags,
            name_index: self.name_index,
            descriptor_index: self.descriptor_index,
            attributes: self.attributes,
        }
    }
}

#[derive(Debug)]
pub struct Attribute {
    pub attribute_name_index: u16,
    // pub attribute_length: u32,
    pub kind: AttributeKind,
}

impl Attribute {
    fn attribute_length(&self) -> u32 {
        self.kind.write_to_byte_len()
    }
}

impl WriteToByteLen for Attribute {
    fn write_to_byte_len(&self) -> u32 {
        2 + 4 + self.kind.write_to_byte_len()
    }
}

impl WriteTo for Attribute {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.attribute_name_index, stream)?;
        write_u32(self.attribute_length(), stream)?;
        self.kind.write_to(stream)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum AttributeKind {
    Code(CodeAttribute),
}

impl WriteTo for AttributeKind {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        match self {
            AttributeKind::Code(val) => val.write_to(stream),
        }
    }
}

impl WriteToByteLen for AttributeKind {
    fn write_to_byte_len(&self) -> u32 {
        match self {
            AttributeKind::Code(val) => val.write_to_byte_len(),
        }
    }
}

#[derive(Debug)]
pub struct Exception {
    pub start_pc: u16,
    pub end_pc: u16,
    pub handler_pc: u16,
    pub catch_type: u16,
}

impl WriteToByteLen for Exception {
    fn write_to_byte_len(&self) -> u32 {
        2 + 2 + 2 + 2
    }
}

impl WriteTo for Exception {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.start_pc, stream)?;
        write_u16(self.end_pc, stream)?;
        write_u16(self.handler_pc, stream)?;
        write_u16(self.catch_type, stream)?;

        Ok(())
    }
}

pub struct CodeAttribute {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<u8>,
    pub exception_table: Vec<Exception>,
    pub attributes: Vec<Attribute>,
}

impl std::fmt::Debug for CodeAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CodeAttribute")
            .field("max_stack", &self.max_stack)
            .field("max_locals", &self.max_locals)
            .field("code", &{
                let code = &self.code;
                let mut instr_vec: Vec<Instruction> = vec![];

                let mut i = 0usize;
                while i < code.len() {
                    let byte = code[i];
                    let instr =
                        Instruction::from_byte(byte, code[i + 1..].iter().copied()).unwrap();
                    i += instr.byte_len();
                    instr_vec.push(instr);
                }

                instr_vec
            })
            .finish()
    }
}

impl WriteToByteLen for CodeAttribute {
    fn write_to_byte_len(&self) -> u32 {
        2 + 2
            + 4
            + self.code.len() as u32
            + 2
            + self
                .exception_table
                .iter()
                .map(|e| e.write_to_byte_len())
                .reduce(|accum, item| accum + item)
                .unwrap_or(0)
            + 2
            + self
                .attributes
                .iter()
                .map(|attr| attr.write_to_byte_len())
                .reduce(|accum, item| accum + item)
                .unwrap_or(0)
    }
}

impl WriteTo for CodeAttribute {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        write_u16(self.max_stack, stream)?;
        write_u16(self.max_locals, stream)?;
        write_u32(self.code.len() as u32, stream)?;
        stream.write_all(&self.code[..])?;
        write_u16(self.exception_table.len() as u16, stream)?;
        for exception in &self.exception_table {
            exception.write_to(stream)?;
        }
        write_u16(self.attributes.len() as u16, stream)?;
        for attribute in &self.attributes {
            attribute.write_to(stream)?;
        }
        Ok(())
    }
}

/// Write a struct's bytes into something that is Write
pub trait WriteTo {
    fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()>;
}

/// The number of bytes written for a struct by write_to
pub trait WriteToByteLen: WriteTo {
    fn write_to_byte_len(&self) -> u32;
}
