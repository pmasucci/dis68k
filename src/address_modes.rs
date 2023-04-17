use crate::Size;

pub enum DataRegister {
    D0,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
}

impl DataRegister {
    pub fn new(register: u16) -> DataRegister {
        match register {
            0b000 => DataRegister::D0,
            0b001 => DataRegister::D1,
            0b010 => DataRegister::D2,
            0b011 => DataRegister::D3,
            0b100 => DataRegister::D4,
            0b101 => DataRegister::D5,
            0b110 => DataRegister::D6,
            0b111 => DataRegister::D7,
            _ => panic!("Illegal Data Register"),
        }
    }
}

pub enum AddressRegister {
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
}

impl AddressRegister {
    pub fn new(register: u16) -> AddressRegister {
        match register {
            0b000 => AddressRegister::A0,
            0b001 => AddressRegister::A1,
            0b010 => AddressRegister::A2,
            0b011 => AddressRegister::A3,
            0b100 => AddressRegister::A4,
            0b101 => AddressRegister::A5,
            0b110 => AddressRegister::A6,
            0b111 => AddressRegister::A7,
            _ => panic!("Illegal Address Register"),
        }
    }
}

pub enum AddressMode {
    DataDirect(DataRegister),
    AddressDirect(AddressRegister),
    AddressIndirect(AddressRegister),
    AddressIndirectPostIncrement(AddressRegister),
    AddressIndirectPredecrement(AddressRegister),
    AddressIndirectDisplacement(AddressRegister),
    AddressIndirectIndex(AddressRegister),
    ProgramCounterDisplacement,
    ProgramCounterIndex,
    AbsoluteShort,
    AbsoluteLong,
    Immediate,
    None,
}

impl AddressMode {
    pub fn new(mode: u16, register: u16) -> AddressMode {
        use AddressMode::*;
        match mode {
            0x0 => DataDirect(DataRegister::new(register)),
            0x1 => AddressDirect(AddressRegister::new(register)),
            0x2 => AddressIndirect(AddressRegister::new(register)),
            0x3 => AddressIndirectPostIncrement(AddressRegister::new(register)),
            0x4 => AddressIndirectPredecrement(AddressRegister::new(register)),
            0x5 => AddressIndirectDisplacement(AddressRegister::new(register)),
            0x6 => AddressIndirectIndex(AddressRegister::new(register)),
            0x7 => match register {
                0x0 => AbsoluteShort,
                0x1 => AbsoluteLong,
                0x2 => ProgramCounterDisplacement,
                0x3 => ProgramCounterIndex,
                0x4 => Immediate,
                _ => panic!("invalid address mode"),
            },
            _ => panic!("invalid address mode"),
        }
    }

    pub fn extensions(&self, size: &Size) -> usize {
        match self {
            AddressMode::AddressIndirectDisplacement(_)
            | AddressMode::AddressIndirectIndex(_)
            | AddressMode::ProgramCounterDisplacement
            | AddressMode::ProgramCounterIndex
            | AddressMode::AbsoluteShort => 2,
            AddressMode::AbsoluteLong => 4,
            AddressMode::Immediate => size.displacement(),
            _ => 0,
        }
    }
}
