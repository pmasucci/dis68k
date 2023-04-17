use std::fmt;

pub enum Size {
    Byte,
    Word,
    Long,
    None,
}

pub enum Format {
    Default,
    Alternate,
    OneBit,
}

impl Size {
    pub fn displacement(&self) -> usize {
        match self {
            Size::Byte | Size::Word => 2,
            Size::Long => 4,
            Size::None => 0,
        }
    }

    pub fn from_bits(bits: u16, format: Format) -> Size {
        match format {
            Format::Default => match bits {
                0b00 => Size::Byte,
                0b01 => Size::Word,
                0b10 => Size::Long,
                _ => Size::None,
            },
            Format::Alternate => match bits {
                0b01 => Size::Byte,
                0b11 => Size::Word,
                0b10 => Size::Long,
                _ => Size::None,
            },
            Format::OneBit => match bits {
                0b0 => Size::Word,
                0b1 => Size::Long,
                _ => Size::None,
            },
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Size::Byte => write!(f, "b"),
            Size::Word => write!(f, "w"),
            Size::Long => write!(f, "l"),
            Size::None => write!(f, ""),
        }
    }
}
