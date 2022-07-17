use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::io::Write;

use super::descriptor::{DescriptorBuilder, FieldTypeKind};

pub struct ConstantPool {
    pub pool: Vec<ConstantKind>,
    pub utf8_map: HashMap<String, u16>,
    pub string_map: HashMap<String, u16>,
    pub method_ref_map: HashMap<(String, String, u16), u16>,
    pub name_and_type_map: HashMap<(String, String), u16>,
    pub java_ctx: JavaContext,
}

impl std::fmt::Debug for ConstantPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConstantPool")
            .field(
                "pool",
                &self
                    .pool
                    .iter()
                    .enumerate()
                    .map(|(i, constant)| (i + 1, constant))
                    .collect::<Vec<_>>(),
            )
            .field("utf8_map", &self.utf8_map)
            .field("string_map", &self.string_map)
            .field("method_ref_map", &self.method_ref_map)
            .field("name_and_type_map", &self.name_and_type_map)
            .field("java_ctx", &self.java_ctx)
            .finish()
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            utf8_map: HashMap::new(),
            string_map: HashMap::new(),
            method_ref_map: HashMap::new(),
            name_and_type_map: HashMap::new(),
            java_ctx: JavaContext {
                system: None,
                system_out: None,
                print_stream: None,
                println_string: None,
                println_long: None,
                arrays: None,
                arr_long_to_string: None,
            },
        }
    }
}

#[derive(Debug)]
pub struct JavaContext {
    pub system: Option<u16>,         // ClassConstant
    pub system_out: Option<u16>,     // FieldRef
    pub print_stream: Option<u16>,   // ClassConstant
    pub println_string: Option<u16>, // MethodRef
    pub println_long: Option<u16>,   // MethodRef

    pub arrays: Option<u16>,             // ClassConstant
    pub arr_long_to_string: Option<u16>, // MethodRef: java/util/Arrays.toString:([J)Ljava/lang/String
}

impl ConstantPool {
    pub fn get_utf8_index(&mut self, s: &str) -> u16 {
        match self.utf8_map.entry(s.to_string()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                self.pool.push(ConstantKind::utf8_from_str(s));
                let index = self.pool.len() as u16;
                *entry.insert(index)
            }
        }
    }

    pub fn get_string_index(&mut self, s: &str) -> u16 {
        match self.string_map.entry(s.to_string()) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                self.pool.push(ConstantKind::utf8_from_str(s));
                let string_index = self.pool.len() as u16;
                self.pool
                    .push(ConstantKind::String(StringConstant { string_index }));
                *entry.insert(self.pool.len() as u16)
            }
        }
    }

    pub fn get_method_ref_index(&mut self, name: &str, descriptor: &str, class_index: u16) -> u16 {
        let name_and_type_index = self.get_name_and_type_index(name, descriptor);

        match self
            .method_ref_map
            .entry((name.to_string(), descriptor.to_string(), class_index))
        {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                self.pool.push(ConstantKind::MethodRef(MethodRefConstant {
                    class_index,
                    name_and_type_index,
                }));
                *entry.insert(self.pool.len() as u16)
            }
        }
    }

    pub fn get_name_and_type_index(&mut self, name: &str, descriptor: &str) -> u16 {
        let name_index = self.get_utf8_index(name);
        let descriptor_index = self.get_utf8_index(descriptor);

        match self
            .name_and_type_map
            .entry((name.to_string(), descriptor.to_string()))
        {
            Entry::Occupied(entry) => return *entry.get(),
            Entry::Vacant(entry) => {
                self.pool
                    .push(ConstantKind::NameAndType(NameAndTypeConstant {
                        name_index,
                        descriptor_index,
                    }));
                *entry.insert(self.pool.len() as u16)
            }
        }
    }

    pub fn get_long_index(&mut self, long: usize) -> u16 {
        let bytes = long.to_be_bytes();
        let len = self.push(ConstantKind::Long(LongConstant {
            high_bytes: ((bytes[0] as u32) << 24)
                + ((bytes[1] as u32) << 16)
                + ((bytes[2] as u32) << 8)
                + bytes[3] as u32,
            low_bytes: ((bytes[4] as u32) << 24)
                + ((bytes[5] as u32) << 16)
                + ((bytes[6] as u32) << 8)
                + bytes[7] as u32,
        }));

        // long constants take up two spaces in the constant pool,
        // by pushing a placeholder and ignoring it during class file output,
        // we can cause references to future constants to have their indices
        // incremented by 1, effectively allowing the long two take up two indices
        self.push(ConstantKind::Placeholder);

        len
    }

    pub fn get_int_index(&mut self, int: u32) -> u16 {
        self.push(ConstantKind::Integer(IntegerConstant { bytes: int }))
    }

    pub fn len(&self) -> u16 {
        self.pool.len() as u16
    }

    pub fn push(&mut self, constant: ConstantKind) -> u16 {
        self.pool.push(constant);
        self.len()
    }

    pub fn include_java_system(&mut self) -> u16 {
        // add java/lang/System class constant
        if let Some(index) = self.java_ctx.system {
            index
        } else {
            let name_index = self.get_utf8_index("java/lang/System");
            self.push(ConstantKind::Class(ClassConstant { name_index }));
            let len = self.len();
            self.java_ctx.system = Some(len);
            len
        }
    }

    pub fn include_java_system_out(&mut self) -> u16 {
        if let Some(index) = self.java_ctx.system_out {
            index
        } else {
            let name_index = self.get_utf8_index("out");
            let descriptor_index = self.get_utf8_index("Ljava/io/PrintStream;");
            self.push(ConstantKind::NameAndType(NameAndTypeConstant {
                name_index,
                descriptor_index,
            }));
            let name_and_type_index = self.len();

            let class_index = self.include_java_system();
            self.push(ConstantKind::FieldRef(FieldRefConstant {
                class_index,
                name_and_type_index,
            }));

            let len = self.len();
            self.java_ctx.system_out = Some(len);
            len
        }
    }

    pub fn include_java_print_stream(&mut self) -> u16 {
        // add java/io/PrintStream class constant
        if let Some(index) = self.java_ctx.print_stream {
            index
        } else {
            let name_index = self.get_utf8_index("java/io/PrintStream");
            self.push(ConstantKind::Class(ClassConstant { name_index }));
            let len = self.len();
            self.java_ctx.print_stream = Some(len);
            len
        }
    }

    pub fn include_java_println_string(&mut self) -> u16 {
        if let Some(index) = self.java_ctx.println_string {
            index
        } else {
            let name_index = self.get_utf8_index("println");
            let descriptor_index = self.get_utf8_index("(Ljava/lang/String;)V");
            self.push(ConstantKind::NameAndType(NameAndTypeConstant {
                name_index,
                descriptor_index,
            }));
            let name_and_type_index = self.len();

            let class_index = self.include_java_print_stream();
            self.push(ConstantKind::MethodRef(MethodRefConstant {
                class_index,
                name_and_type_index,
            }));

            let len = self.len();
            self.java_ctx.println_string = Some(len);
            len
        }
    }

    pub fn include_java_println_long(&mut self) -> u16 {
        if let Some(index) = self.java_ctx.println_string {
            index
        } else {
            let name_index = self.get_utf8_index("println");
            let descriptor_index = self.get_utf8_index("(J)V");
            self.push(ConstantKind::NameAndType(NameAndTypeConstant {
                name_index,
                descriptor_index,
            }));
            let name_and_type_index = self.len();

            let class_index = self.include_java_print_stream();
            self.push(ConstantKind::MethodRef(MethodRefConstant {
                class_index,
                name_and_type_index,
            }));

            let len = self.len();
            self.java_ctx.println_long = Some(len);
            len
        }
    }

    pub fn include_java_arrays(&mut self) -> u16 {
        // add java/util/Arrays class constant
        if let Some(index) = self.java_ctx.arrays {
            index
        } else {
            let name_index = self.get_utf8_index("java/util/Arrays");
            self.push(ConstantKind::Class(ClassConstant { name_index }));
            let len = self.len();
            self.java_ctx.arrays = Some(len);
            len
        }
    }

    pub fn include_java_arr_long_to_string(&mut self) -> u16 {
        if let Some(index) = self.java_ctx.arr_long_to_string {
            index
        } else {
            let name_index = self.get_utf8_index("toString");
            let descriptor_index = self.get_utf8_index(
                &DescriptorBuilder::method()
                    .params()
                    .arr_ref(FieldTypeKind::Long)
                    .ret_type()
                    .class_ref("java/lang/String")
                    .descriptor(),
            );
            self.push(ConstantKind::NameAndType(NameAndTypeConstant {
                name_index,
                descriptor_index,
            }));
            let name_and_type_index = self.len();

            let class_index = self.include_java_arrays();
            self.push(ConstantKind::MethodRef(MethodRefConstant {
                class_index,
                name_and_type_index,
            }));

            let len = self.len();
            self.java_ctx.arr_long_to_string = Some(len);
            len
        }
    }
}

#[derive(Debug)]
pub struct ClassConstant {
    pub name_index: u16,
}

impl ClassConstant {
    pub fn new(name_index: u16) -> Self {
        ClassConstant { name_index }
    }
}

const CONST_TAG_CLASS: u8 = 7;
impl ConstantKindTagAndInfo for ClassConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_CLASS
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.name_index.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct FieldRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

impl FieldRefConstant {
    pub fn new(class_index: u16, name_and_type_index: u16) -> Self {
        FieldRefConstant {
            class_index,
            name_and_type_index,
        }
    }
}

const CONST_TAG_FIELDREF: u8 = 9;
impl ConstantKindTagAndInfo for FieldRefConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_FIELDREF
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.class_index.to_be_bytes()[..]);
        info.extend(&self.name_and_type_index.to_be_bytes()[..]);
        info
    }
}

#[derive(Debug)]
pub struct MethodRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

impl MethodRefConstant {
    pub fn new(class_index: u16, name_and_type_index: u16) -> Self {
        MethodRefConstant {
            class_index,
            name_and_type_index,
        }
    }
}

const CONST_TAG_METHODREF: u8 = 10;
impl ConstantKindTagAndInfo for MethodRefConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_METHODREF
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.class_index.to_be_bytes()[..]);
        info.extend(&self.name_and_type_index.to_be_bytes()[..]);
        info
    }
}

#[derive(Debug)]
pub struct InterfaceMethodRefConstant {
    pub class_index: u16,
    pub name_and_type_index: u16,
}

const CONST_TAG_INTERFACE_METHODREF: u8 = 11;
impl ConstantKindTagAndInfo for InterfaceMethodRefConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_INTERFACE_METHODREF
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.class_index.to_be_bytes()[..]);
        info.extend(&self.name_and_type_index.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct StringConstant {
    pub string_index: u16,
}

impl StringConstant {
    pub fn new(string_index: u16) -> Self {
        StringConstant { string_index }
    }
}

const CONST_TAG_STRING: u8 = 8;
impl ConstantKindTagAndInfo for StringConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_STRING
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.string_index.to_be_bytes()[..]);
        info
    }
}

pub struct IntegerConstant {
    pub bytes: u32,
}

impl std::fmt::Debug for IntegerConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IntegerConstant")
            .field("val", &self.bytes)
            .finish()
    }
}

impl IntegerConstant {
    pub fn new(bytes: u32) -> Self {
        IntegerConstant { bytes }
    }
}

const CONST_TAG_INTEGER: u8 = 3;
impl ConstantKindTagAndInfo for IntegerConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_INTEGER
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.bytes.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct FloatConstant {
    pub bytes: u32,
}

impl FloatConstant {
    pub fn new(bytes: u32) -> Self {
        FloatConstant { bytes }
    }
}

const CONST_TAG_FLOAT: u8 = 4;
impl ConstantKindTagAndInfo for FloatConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_FLOAT
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.bytes.to_be_bytes()[..]);
        info
    }
}
pub struct LongConstant {
    pub high_bytes: u32,
    pub low_bytes: u32,
}

impl std::fmt::Debug for LongConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LongConstant")
            .field(
                "val",
                &(((self.high_bytes as u64) << 32) + self.low_bytes as u64),
            )
            .finish()
    }
}

impl LongConstant {
    pub fn new(high_bytes: u32, low_bytes: u32) -> Self {
        LongConstant {
            high_bytes,
            low_bytes,
        }
    }
}

const CONST_TAG_LONG: u8 = 5;
impl ConstantKindTagAndInfo for LongConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_LONG
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.high_bytes.to_be_bytes()[..]);
        info.extend(&self.low_bytes.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct DoubleConstant {
    pub high_bytes: u32,
    pub low_bytes: u32,
}

impl DoubleConstant {
    pub fn new(high_bytes: u32, low_bytes: u32) -> Self {
        DoubleConstant {
            high_bytes,
            low_bytes,
        }
    }
}

const CONST_TAG_DOUBLE: u8 = 6;
impl ConstantKindTagAndInfo for DoubleConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_DOUBLE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.high_bytes.to_be_bytes()[..]);
        info.extend(&self.low_bytes.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct NameAndTypeConstant {
    pub name_index: u16,
    pub descriptor_index: u16,
}

impl NameAndTypeConstant {
    pub fn new(name_index: u16, descriptor_index: u16) -> Self {
        NameAndTypeConstant {
            name_index,
            descriptor_index,
        }
    }
}

const CONST_TAG_NAME_AND_TYPE: u8 = 12;
impl ConstantKindTagAndInfo for NameAndTypeConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_NAME_AND_TYPE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.name_index.to_be_bytes()[..]);
        info.extend(&self.descriptor_index.to_be_bytes()[..]);
        info
    }
}
pub struct Utf8Constant {
    pub bytes: Vec<u8>,
}

impl std::fmt::Debug for Utf8Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Utf8Constant")
            .field(
                "bytes",
                &std::str::from_utf8(&self.bytes[2..]).unwrap_or("UTF8 BYTES ERROR"),
            )
            .finish()
    }
}

impl Utf8Constant {
    pub fn new(bytes: Vec<u8>) -> Self {
        Utf8Constant { bytes }
    }
}

const CONST_TAG_UTF8: u8 = 1;
impl ConstantKindTagAndInfo for Utf8Constant {
    fn tag(&self) -> u8 {
        CONST_TAG_UTF8
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.bytes);
        info
    }
}

#[derive(Debug)]
pub struct MethodHandleConstant {
    pub reference_kind: u8,
    pub reference_index: u16,
}

impl MethodHandleConstant {
    pub fn new(reference_kind: u8, reference_index: u16) -> Self {
        MethodHandleConstant {
            reference_kind,
            reference_index,
        }
    }
}

const CONST_TAG_METHOD_HANDLE: u8 = 15;
impl ConstantKindTagAndInfo for MethodHandleConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_METHOD_HANDLE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![self.reference_kind];
        info.extend(&self.reference_index.to_be_bytes()[..]);
        info
    }
}

#[derive(Debug)]
pub struct MethodTypeConstant {
    pub descriptor_index: u16,
}

impl MethodTypeConstant {
    pub fn new(descriptor_index: u16) -> Self {
        MethodTypeConstant { descriptor_index }
    }
}

const CONST_TAG_METHOD_TYPE: u8 = 16;
impl ConstantKindTagAndInfo for MethodTypeConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_METHOD_TYPE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.descriptor_index.to_be_bytes()[..]);
        info
    }
}

#[derive(Debug)]
pub struct DynamicConstant {
    pub bootstrap_method_attr_index: u16,
    pub name_and_type_index: u16,
}

impl DynamicConstant {
    pub fn new(bootstrap_method_attr_index: u16, name_and_type_index: u16) -> Self {
        DynamicConstant {
            bootstrap_method_attr_index,
            name_and_type_index,
        }
    }
}

const CONST_TAG_DYNAMIC: u8 = 17;
impl ConstantKindTagAndInfo for DynamicConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_DYNAMIC
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.bootstrap_method_attr_index.to_be_bytes()[..]);
        info.extend(&self.name_and_type_index.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct InvokeDynamicConstant {
    pub bootstrap_method_attr_index: u16,
    pub name_and_type_index: u16,
}

impl InvokeDynamicConstant {
    pub fn new(bootstrap_method_attr_index: u16, name_and_type_index: u16) -> Self {
        InvokeDynamicConstant {
            bootstrap_method_attr_index,
            name_and_type_index,
        }
    }
}

const CONST_TAG_INVOKE_DYNAMIC: u8 = 18;
impl ConstantKindTagAndInfo for InvokeDynamicConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_INVOKE_DYNAMIC
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.bootstrap_method_attr_index.to_be_bytes()[..]);
        info.extend(&self.name_and_type_index.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct ModuleConstant {
    pub name_index: u16,
}

impl ModuleConstant {
    pub fn new(name_index: u16) -> Self {
        ModuleConstant { name_index }
    }
}

const CONST_TAG_MODULE: u8 = 19;
impl ConstantKindTagAndInfo for ModuleConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_MODULE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.name_index.to_be_bytes()[..]);
        info
    }
}
#[derive(Debug)]
pub struct PackageConstant {
    pub name_index: u16,
}

impl PackageConstant {
    pub fn new(name_index: u16) -> Self {
        PackageConstant { name_index }
    }
}

const CONST_TAG_PACKAGE: u8 = 20;
impl ConstantKindTagAndInfo for PackageConstant {
    fn tag(&self) -> u8 {
        CONST_TAG_PACKAGE
    }

    fn info(&self) -> Vec<u8> {
        let mut info = vec![];
        info.extend(&self.name_index.to_be_bytes()[..]);
        info
    }
}

trait ConstantKindTagAndInfo {
    fn tag(&self) -> u8;
    fn info(&self) -> Vec<u8>;
}

#[derive(Debug)]
pub enum ConstantKind {
    Class(ClassConstant),
    FieldRef(FieldRefConstant),
    MethodRef(MethodRefConstant),
    InterfaceMethodRef(InterfaceMethodRefConstant),
    String(StringConstant),
    Integer(IntegerConstant),
    Float(FloatConstant),
    Long(LongConstant),
    Double(DoubleConstant),
    NameAndType(NameAndTypeConstant),
    Utf8(Utf8Constant),
    MethodHandle(MethodHandleConstant),
    MethodType(MethodTypeConstant),
    Dynamic(DynamicConstant),
    InvokeDynamic(InvokeDynamicConstant),
    Module(ModuleConstant),
    Package(PackageConstant),
    Placeholder,
}

impl ConstantKind {
    pub fn tag(&self) -> u8 {
        match self {
            ConstantKind::Class(val) => val.tag(),
            ConstantKind::FieldRef(val) => val.tag(),
            ConstantKind::MethodRef(val) => val.tag(),
            ConstantKind::InterfaceMethodRef(val) => val.tag(),
            ConstantKind::String(val) => val.tag(),
            ConstantKind::Integer(val) => val.tag(),
            ConstantKind::Float(val) => val.tag(),
            ConstantKind::Long(val) => val.tag(),
            ConstantKind::Double(val) => val.tag(),
            ConstantKind::NameAndType(val) => val.tag(),
            ConstantKind::Utf8(val) => val.tag(),
            ConstantKind::MethodHandle(val) => val.tag(),
            ConstantKind::MethodType(val) => val.tag(),
            ConstantKind::Dynamic(val) => val.tag(),
            ConstantKind::InvokeDynamic(val) => val.tag(),
            ConstantKind::Module(val) => val.tag(),
            ConstantKind::Package(val) => val.tag(),
            ConstantKind::Placeholder => 0,
        }
    }

    pub fn info(&self) -> Vec<u8> {
        match self {
            ConstantKind::Class(val) => val.info(),
            ConstantKind::FieldRef(val) => val.info(),
            ConstantKind::MethodRef(val) => val.info(),
            ConstantKind::InterfaceMethodRef(val) => val.info(),
            ConstantKind::String(val) => val.info(),
            ConstantKind::Integer(val) => val.info(),
            ConstantKind::Float(val) => val.info(),
            ConstantKind::Long(val) => val.info(),
            ConstantKind::Double(val) => val.info(),
            ConstantKind::NameAndType(val) => val.info(),
            ConstantKind::Utf8(val) => val.info(),
            ConstantKind::MethodHandle(val) => val.info(),
            ConstantKind::MethodType(val) => val.info(),
            ConstantKind::Dynamic(val) => val.info(),
            ConstantKind::InvokeDynamic(val) => val.info(),
            ConstantKind::Module(val) => val.info(),
            ConstantKind::Package(val) => val.info(),
            ConstantKind::Placeholder => vec![],
        }
    }
}

impl ConstantKind {
    pub fn write_to(&self, stream: &mut impl Write) -> std::io::Result<()> {
        stream.write_all(&[self.tag()])?;
        stream.write_all(&self.info()[..])?;
        Ok(())
    }

    pub fn utf8_from_str(s: &str) -> Self {
        let bytes = s.as_bytes();
        let size: u16 = bytes.len().try_into().unwrap();

        let mut info = vec![];
        info.extend(size.to_be_bytes());
        info.extend(bytes);

        ConstantKind::Utf8(Utf8Constant { bytes: info })
    }
}
