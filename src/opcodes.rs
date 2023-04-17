use crate::{
    address_modes::AddressMode,
    size::{Format, Size},
};
use std::fmt;
use std::ops::Range;

pub enum Condition {
    True,
    False,
    Higher,
    LowerOrSame,
    CarryClear,
    CarrySet,
    NotEqual,
    Equal,
    OverflowClear,
    OverflowSet,
    Plus,
    Minus,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
}

impl Condition {
    pub fn new(code: u16) -> Condition {
        match code {
            0x0 => Condition::True,
            0x1 => Condition::False,
            0x2 => Condition::Higher,
            0x3 => Condition::LowerOrSame,
            0x4 => Condition::CarryClear,
            0x5 => Condition::CarrySet,
            0x6 => Condition::NotEqual,
            0x7 => Condition::Equal,
            0x8 => Condition::OverflowClear,
            0x9 => Condition::OverflowSet,
            0xa => Condition::Plus,
            0xb => Condition::Minus,
            0xc => Condition::GreaterOrEqual,
            0xd => Condition::LessThan,
            0xe => Condition::GreaterThan,
            0xf => Condition::LessOrEqual,
            _ => panic!("Illegal Condition Code"),
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mnemonic = match self {
            Condition::True => "T",
            Condition::False => "F",
            Condition::Higher => "HI",
            Condition::LowerOrSame => "LS",
            Condition::CarryClear => "CC",
            Condition::CarrySet => "CS",
            Condition::NotEqual => "NE",
            Condition::Equal => "EQ",
            Condition::OverflowClear => "VC",
            Condition::OverflowSet => "VS",
            Condition::Plus => "PL",
            Condition::Minus => "MI",
            Condition::GreaterOrEqual => "GE",
            Condition::LessThan => "LT",
            Condition::GreaterThan => "GT",
            Condition::LessOrEqual => "LE",
        };

        write!(f, "{}", mnemonic)
    }
}

pub enum Target {
    Immediate,
    Register,
}

pub enum OpCode {
    ORICCR,
    ORISR,
    ORI,
    ANDICCR,
    ANDISR,
    ANDI,
    SUBI,
    ADDI,
    EORICCR,
    EORISR,
    EORI,
    CMPI,
    BTST(Target),
    BCHG(Target),
    BCLR(Target),
    BSET(Target),
    MOVEP,
    MOVEA,
    MOVE,
    MOVEFROMSR,
    MOVETOCCR,
    MOVETOSR,
    NEGX,
    CLR,
    NEG,
    NOT,
    EXT,
    NBCD,
    SWAP,
    PEA,
    ILLEGAL,
    TAS,
    TST,
    TRAP,
    LINK,
    UNLK,
    MOVEUSP,
    RESET,
    NOP,
    STOP,
    RTE,
    RTS,
    TRAPV,
    RTR,
    JSR,
    JMP,
    MOVEM,
    LEA,
    CHK,
    ADDQ,
    SUBQ,
    Scc(Condition),
    DBcc(Condition),
    BRA,
    BSR,
    Bcc(Condition),
    MOVEQ,
    DIVU,
    DIVS,
    SBCD,
    OR,
    SUB,
    SUBX,
    SUBA,
    EOR,
    CMPM,
    CMP,
    CMPA,
    MULU,
    MULS,
    ABCD,
    EXG,
    AND,
    ADD,
    ADDX,
    ADDA,
    ASLM,
    ASRM,
    LSLM,
    LSRM,
    ROXLM,
    ROXRM,
    ROLM,
    RORM,
    ASL(Target),
    ASR(Target),
    LSL(Target),
    LSR(Target),
    ROXL(Target),
    ROXR(Target),
    ROL(Target),
    ROR(Target),
    Unimplemented,
}

const OPCODES: [OpCode; 148] = [
    OpCode::ORICCR,
    OpCode::ORISR,
    OpCode::ORI,
    OpCode::ANDICCR,
    OpCode::ANDISR,
    OpCode::ANDI,
    OpCode::SUBI,
    OpCode::ADDI,
    OpCode::EORICCR,
    OpCode::EORISR,
    OpCode::EORI,
    OpCode::CMPI,
    OpCode::BTST(Target::Immediate),
    OpCode::BCHG(Target::Immediate),
    OpCode::BCLR(Target::Immediate),
    OpCode::BSET(Target::Immediate),
    OpCode::BTST(Target::Register),
    OpCode::BCHG(Target::Register),
    OpCode::BCLR(Target::Register),
    OpCode::BSET(Target::Register),
    OpCode::MOVEP,
    OpCode::MOVEA,
    OpCode::MOVE,
    OpCode::MOVEFROMSR,
    OpCode::MOVETOCCR,
    OpCode::MOVETOSR,
    OpCode::NEGX,
    OpCode::CLR,
    OpCode::NEG,
    OpCode::NOT,
    OpCode::EXT,
    OpCode::NBCD,
    OpCode::SWAP,
    OpCode::PEA,
    OpCode::ILLEGAL,
    OpCode::TAS,
    OpCode::TST,
    OpCode::TRAP,
    OpCode::LINK,
    OpCode::UNLK,
    OpCode::MOVEUSP,
    OpCode::RESET,
    OpCode::NOP,
    OpCode::STOP,
    OpCode::RTE,
    OpCode::RTS,
    OpCode::TRAPV,
    OpCode::RTR,
    OpCode::JSR,
    OpCode::JMP,
    OpCode::MOVEM,
    OpCode::LEA,
    OpCode::CHK,
    OpCode::ADDQ,
    OpCode::SUBQ,
    OpCode::Scc(Condition::True),
    OpCode::Scc(Condition::False),
    OpCode::Scc(Condition::Higher),
    OpCode::Scc(Condition::LowerOrSame),
    OpCode::Scc(Condition::CarryClear),
    OpCode::Scc(Condition::CarrySet),
    OpCode::Scc(Condition::NotEqual),
    OpCode::Scc(Condition::Equal),
    OpCode::Scc(Condition::OverflowClear),
    OpCode::Scc(Condition::OverflowSet),
    OpCode::Scc(Condition::Plus),
    OpCode::Scc(Condition::Minus),
    OpCode::Scc(Condition::GreaterOrEqual),
    OpCode::Scc(Condition::LessThan),
    OpCode::Scc(Condition::GreaterThan),
    OpCode::Scc(Condition::LessOrEqual),
    OpCode::DBcc(Condition::True),
    OpCode::DBcc(Condition::False),
    OpCode::DBcc(Condition::Higher),
    OpCode::DBcc(Condition::LowerOrSame),
    OpCode::DBcc(Condition::CarryClear),
    OpCode::DBcc(Condition::CarrySet),
    OpCode::DBcc(Condition::NotEqual),
    OpCode::DBcc(Condition::Equal),
    OpCode::DBcc(Condition::OverflowClear),
    OpCode::DBcc(Condition::OverflowSet),
    OpCode::DBcc(Condition::Plus),
    OpCode::DBcc(Condition::Minus),
    OpCode::DBcc(Condition::GreaterOrEqual),
    OpCode::DBcc(Condition::LessThan),
    OpCode::DBcc(Condition::GreaterThan),
    OpCode::DBcc(Condition::LessOrEqual),
    OpCode::BRA,
    OpCode::BSR,
    OpCode::Bcc(Condition::Higher),
    OpCode::Bcc(Condition::LowerOrSame),
    OpCode::Bcc(Condition::CarryClear),
    OpCode::Bcc(Condition::CarrySet),
    OpCode::Bcc(Condition::NotEqual),
    OpCode::Bcc(Condition::Equal),
    OpCode::Bcc(Condition::OverflowClear),
    OpCode::Bcc(Condition::OverflowSet),
    OpCode::Bcc(Condition::Plus),
    OpCode::Bcc(Condition::Minus),
    OpCode::Bcc(Condition::GreaterOrEqual),
    OpCode::Bcc(Condition::LessThan),
    OpCode::Bcc(Condition::GreaterThan),
    OpCode::Bcc(Condition::LessOrEqual),
    OpCode::MOVEQ,
    OpCode::DIVU,
    OpCode::DIVS,
    OpCode::SBCD,
    OpCode::OR,
    OpCode::SUB,
    OpCode::SUBX,
    OpCode::SUBA,
    OpCode::EOR,
    OpCode::CMPM,
    OpCode::CMP,
    OpCode::CMPA,
    OpCode::MULU,
    OpCode::MULS,
    OpCode::ABCD,
    OpCode::EXG,
    OpCode::AND,
    OpCode::ADD,
    OpCode::ADDX,
    OpCode::ADDA,
    OpCode::ASLM,
    OpCode::ASRM,
    OpCode::LSLM,
    OpCode::LSRM,
    OpCode::ROXLM,
    OpCode::ROXRM,
    OpCode::ROLM,
    OpCode::RORM,
    OpCode::ASL(Target::Immediate),
    OpCode::ASL(Target::Register),
    OpCode::ASR(Target::Immediate),
    OpCode::ASR(Target::Register),
    OpCode::LSL(Target::Immediate),
    OpCode::LSL(Target::Register),
    OpCode::LSR(Target::Immediate),
    OpCode::LSR(Target::Register),
    OpCode::ROXL(Target::Immediate),
    OpCode::ROXL(Target::Register),
    OpCode::ROXR(Target::Immediate),
    OpCode::ROXR(Target::Register),
    OpCode::ROL(Target::Immediate),
    OpCode::ROL(Target::Register),
    OpCode::ROR(Target::Immediate),
    OpCode::ROR(Target::Register),
    OpCode::Unimplemented,
];

impl OpCode {
    /*
        returns the length of extension words in bytes
    */
    pub fn extensions(&self, size: &Size) -> usize {
        match self {
            OpCode::ORICCR
            | OpCode::ORISR
            | OpCode::ANDICCR
            | OpCode::ANDISR
            | OpCode::EORICCR
            | OpCode::EORISR
            | OpCode::STOP
            | OpCode::MOVEM => 2,
            OpCode::ORI | OpCode::ANDI | OpCode::EORI => size.displacement(),
            OpCode::BRA | OpCode::BSR | OpCode::Bcc(_) => size.displacement(),
            _ => 0,
        }
    }
    /*
        returns the size of the instruction
    */
    pub fn size(&self, code: u16) -> Size {
        use OpCode::*;
        match self {
            ORICCR | ORISR | ORI | ANDICCR | ANDISR | ANDI | SUBI | ADDI | EORICCR | EORISR
            | EORI | CMPI | NEGX | CLR | NEG | NOT | TST | ADDQ | SUBQ | OR | SUB | SUBX | EOR
            | CMPM | CMP | AND | ADD | ADDX | ASL(_) | ASR(_) | LSL(_) | LSR(_) | ROXL(_)
            | ROXR(_) | ROL(_) | ROR(_) => Size::from_bits(get_bits(7..9, code), Format::Default),
            MOVEA | MOVE => Size::from_bits(get_bits(13..15, code), Format::Alternate),
            MOVEP | EXT | MOVEM => Size::from_bits(get_bits(7..8, code), Format::OneBit),
            SUBA | CMPA | ADDA => Size::from_bits(get_bits(9..10, code), Format::OneBit),
            Bcc(_) => get_displacement_size(code),
            MOVEFROMSR | MOVETOSR | SWAP | LINK | CHK | DIVU | DIVS | MULU | MULS => Size::Word,
            MOVETOCCR | NBCD | TAS | Scc(_) | SBCD | ABCD => Size::Byte,
            PEA | MOVEUSP | LEA | MOVEQ | EXG => Size::Long,
            _ => panic!("invalid size in opcode {}, {:016b}", self, code),
        }
    }

    pub fn address_mode(&self, code: u16) -> AddressMode {
        use OpCode::*;
        //skipping MOVE for now cause it has two address modes??
        match self {
            ORI | ANDI | SUBI | ADDI | EORI | CMPI | BTST(_) | BCHG(_) | BCLR(_) | BSET(_)
            | MOVEA | MOVEFROMSR | MOVETOCCR | MOVETOSR | NEGX | CLR | NEG | NOT | NBCD | PEA
            | TAS | TST | JSR | JMP | MOVEM | LEA | CHK | ADDQ | SUBQ | Scc(_) | DIVU | DIVS
            | OR | SUB | SUBA | EOR | CMP | CMPA | MULU | MULS | AND | ADD | ADDA | ASLM | ASRM
            | LSLM | LSRM | ROXLM | ROXRM | ROLM | RORM => {
                AddressMode::new(get_bits(4..7, code), get_bits(1..4, code))
            }
            _ => AddressMode::None,
        }
    }

    pub fn compare(&self, code: u16) -> bool {
        // println!("----------------");
        // println!("{}", self);
        // println!("{:016b} code", code);
        // println!("{:016b} bitmask", self.bitmask());
        // println!("{:016b} static", self.static_bits());
        (code & self.bitmask()) ^ self.static_bits() == 0
    }
    pub fn static_bits(&self) -> u16 {
        use OpCode::*;
        match self {
            ORICCR => 0x003C,
            ORISR => 0x007C,
            ORI => 0x0000,
            ANDICCR => 0x023C,
            ANDISR => 0x027C,
            ANDI => 0x0200,
            SUBI => 0x0400,
            ADDI => 0x0600,
            EORICCR => 0x0A3C,
            EORISR => 0x0A7C,
            EORI => 0x0A00,
            CMPI => 0x0C00,
            BTST(Target::Immediate) => 0x0800,
            BCHG(Target::Immediate) => 0x0840,
            BCLR(Target::Immediate) => 0x0880,
            BSET(Target::Immediate) => 0x08C0,
            BTST(Target::Register) => 0x0100,
            BCHG(Target::Register) => 0x0140,
            BCLR(Target::Register) => 0x0180,
            BSET(Target::Register) => 0x01C0,
            MOVEP => 0x0108,
            MOVEA => 0x0040,
            MOVE => 0x0000,
            MOVEFROMSR => 0x40C0,
            MOVETOCCR => 0x44C0,
            MOVETOSR => 0x46C0,
            NEGX => 0x4000,
            CLR => 0x4200,
            NEG => 0x4400,
            NOT => 0x4600,
            EXT => 0x4880,
            NBCD => 0x4800,
            SWAP => 0x4840,
            PEA => 0x4840,
            ILLEGAL => 0x4AFC,
            TAS => 0x4AC0,
            TST => 0x4A00,
            TRAP => 0x4E40,
            LINK => 0x4E50,
            UNLK => 0x4E58,
            MOVEUSP => 0x4E60,
            RESET => 0x4E70,
            NOP => 0x4E71,
            STOP => 0x4E72,
            RTE => 0x4E73,
            RTS => 0x4E75,
            TRAPV => 0x4E76,
            RTR => 0x4E77,
            JSR => 0x4E80,
            JMP => 0x4EC0,
            MOVEM => 0x4880,
            LEA => 0x41C0,
            CHK => 0x4180,
            ADDQ => 0x5000,
            SUBQ => 0x5100,
            Scc(Condition::True) => 0x50C0,
            Scc(Condition::False) => 0x51C0,
            Scc(Condition::Higher) => 0x52C0,
            Scc(Condition::LowerOrSame) => 0x53C0,
            Scc(Condition::CarryClear) => 0x54C0,
            Scc(Condition::CarrySet) => 0x55C0,
            Scc(Condition::NotEqual) => 0x56C0,
            Scc(Condition::Equal) => 0x57C0,
            Scc(Condition::OverflowClear) => 0x58C0,
            Scc(Condition::OverflowSet) => 0x59C0,
            Scc(Condition::Plus) => 0x5AC0,
            Scc(Condition::Minus) => 0x5BC0,
            Scc(Condition::GreaterOrEqual) => 0x5CC0,
            Scc(Condition::LessThan) => 0x5DC0,
            Scc(Condition::GreaterThan) => 0x5EC0,
            Scc(Condition::LessOrEqual) => 0x5FC0,
            DBcc(Condition::True) => 0x50C8,
            DBcc(Condition::False) => 0x51C8,
            DBcc(Condition::Higher) => 0x52C8,
            DBcc(Condition::LowerOrSame) => 0x53C8,
            DBcc(Condition::CarryClear) => 0x54C8,
            DBcc(Condition::CarrySet) => 0x55C8,
            DBcc(Condition::NotEqual) => 0x56C8,
            DBcc(Condition::Equal) => 0x57C8,
            DBcc(Condition::OverflowClear) => 0x58C8,
            DBcc(Condition::OverflowSet) => 0x59C8,
            DBcc(Condition::Plus) => 0x5AC8,
            DBcc(Condition::Minus) => 0x5BC8,
            DBcc(Condition::GreaterOrEqual) => 0x5CC8,
            DBcc(Condition::LessThan) => 0x5DC8,
            DBcc(Condition::GreaterThan) => 0x5EC8,
            DBcc(Condition::LessOrEqual) => 0x5FC8,
            BRA | Bcc(Condition::True) => 0x6000,
            BSR | Bcc(Condition::False) => 0x6100,
            Bcc(Condition::Higher) => 0x6200,
            Bcc(Condition::LowerOrSame) => 0x6300,
            Bcc(Condition::CarryClear) => 0x6400,
            Bcc(Condition::CarrySet) => 0x6500,
            Bcc(Condition::NotEqual) => 0x6600,
            Bcc(Condition::Equal) => 0x6700,
            Bcc(Condition::OverflowClear) => 0x6800,
            Bcc(Condition::OverflowSet) => 0x6900,
            Bcc(Condition::Plus) => 0x6A00,
            Bcc(Condition::Minus) => 0x6B00,
            Bcc(Condition::GreaterOrEqual) => 0x6C00,
            Bcc(Condition::LessThan) => 0x6D00,
            Bcc(Condition::GreaterThan) => 0x6E00,
            Bcc(Condition::LessOrEqual) => 0x6F00,
            MOVEQ => 0x7000,
            DIVU => 0x80C0,
            DIVS => 0x81C0,
            SBCD => 0x8100,
            OR => 0x8000,
            SUB => 0x9000,
            SUBX => 0x9100,
            SUBA => 0x90C0,
            EOR => 0xB100,
            CMPM => 0xB108,
            CMP => 0xB000,
            CMPA => 0xB0C0,
            MULU => 0xC0C0,
            MULS => 0xC1C0,
            ABCD => 0xC100,
            EXG => 0xC100,
            AND => 0xC000,
            ADD => 0xD000,
            ADDX => 0xD100,
            ADDA => 0xD0C0,
            ASLM => 0xE1C0,
            ASRM => 0xE0C0,
            LSLM => 0xE3C0,
            LSRM => 0xE2C0,
            ROXLM => 0xE5C0,
            ROXRM => 0xE4C0,
            ROLM => 0xE7C0,
            RORM => 0xE6C0,
            ASL(Target::Immediate) => 0xE100,
            ASL(Target::Register) => 0xE120,
            ASR(Target::Immediate) => 0xE000,
            ASR(Target::Register) => 0xE020,
            LSL(Target::Immediate) => 0xE108,
            LSL(Target::Register) => 0xE128,
            LSR(Target::Immediate) => 0xE008,
            LSR(Target::Register) => 0xE028,
            ROXL(Target::Immediate) => 0xE110,
            ROXL(Target::Register) => 0xE130,
            ROXR(Target::Immediate) => 0xE010,
            ROXR(Target::Register) => 0xE030,
            ROL(Target::Immediate) => 0xE118,
            ROL(Target::Register) => 0xE138,
            ROR(Target::Immediate) => 0xE018,
            ROR(Target::Register) => 0xE038,
            Unimplemented => panic!("don't do that!"),
        }
    }

    pub fn bitmask(&self) -> u16 {
        use OpCode::*;
        match self {
            // literals
            ORICCR | ORISR | ANDICCR | ANDISR | EORICCR | EORISR | ILLEGAL | RESET | NOP | STOP
            | RTE | RTS | TRAPV | RTR => 0xFFFF,
            // Size - Mode - Address
            ORI | ANDI | SUBI | ADDI | EORI | CMPI | NEGX | CLR | NEG | NOT | TST => 0xFF00,
            // Mode - Address
            BTST(Target::Immediate)
            | BCHG(Target::Immediate)
            | BCLR(Target::Immediate)
            | BSET(Target::Immediate)
            | MOVEFROMSR
            | MOVETOCCR
            | MOVETOSR
            | NBCD
            | PEA
            | TAS
            | JSR
            | JMP => 0xFFC0,
            // Address - Mode - Address
            BTST(Target::Register)
            | BCHG(Target::Register)
            | BCLR(Target::Register)
            | BSET(Target::Register)
            | LEA
            | CHK
            | DIVU
            | DIVS
            | MULU
            | MULS => 0xF1C0,
            // Address - Direction - Size - Mode - Address
            MOVEP => 0xF108,
            // Size - Address - Mode - Address
            MOVEA => 0xC1C0,
            // Size - Address - Mode - Address
            MOVE => 0xC000,
            // 1 bit Size - Address
            EXT => 0xFFB8,
            // Vector
            TRAP => 0xFFF0,
            // Address
            SWAP | LINK | UNLK => 0xFFF8,
            // Direction - Address
            MOVEUSP => 0xFFF0,
            // Direction - Size - Mode - Address
            MOVEM => 0xFB80,
            // Data - Size - Mode - Address
            ADDQ | SUBQ => 0xF100,
            // Condition - Mode - Address
            Scc(_) => 0xF0C0,
            // Condition - Address
            DBcc(_) => 0xF0F8,
            // Condition - Displacement
            BRA | BSR | Bcc(_) => 0xFF00,
            // Address - Data
            MOVEQ => 0xF100,
            // Address - 1 bit Mode - Address
            SBCD | ABCD => 0xF1F0,
            // Address - Direction - Size - Mode - Address
            OR | SUB | AND | ADD => 0xF000,
            // Address - Size - 1 bit Mode - Address
            SUBX | ADDX => 0xF130,
            // Address - 1 bit Size - Mode - Address
            SUBA | CMPA | ADDA => 0xF0C0,
            // Address - Size - Mode - Address
            EOR | CMP => 0xF100,
            // Address - Size - Address
            CMPM => 0xF138,
            // Address - Split Mode? - Address
            EXG => 0xF130,
            // 1 bit Direction - Mode - Address
            ASLM | ASRM | LSLM | LSRM | ROXLM | ROXRM | ROLM | RORM => 0xFEC0,
            // Rotation - 1 bit Direction - 1 bit Mode - Direction
            ASL(_) | ASR(_) | LSL(_) | LSR(_) | ROXL(_) | ROXR(_) | ROL(_) | ROR(_) => 0xF018,
            Unimplemented => 0xFFFF,
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OpCode::ORICCR => write!(f, "ORI.CCR"),
            OpCode::ORISR => write!(f, "ORI.SR"),
            OpCode::ORI => write!(f, "ORI"),
            OpCode::ANDICCR => write!(f, "ANDI.CCR"),
            OpCode::ANDISR => write!(f, "ANDI.SR"),
            OpCode::ANDI => write!(f, "ANDI"),
            OpCode::SUBI => write!(f, "SUBI"),
            OpCode::ADDI => write!(f, "ADDI"),
            OpCode::EORICCR => write!(f, "EORI.CCR"),
            OpCode::EORISR => write!(f, "EORI.SR"),
            OpCode::EORI => write!(f, "EORI"),
            OpCode::CMPI => write!(f, "CMPI"),
            OpCode::BTST(_) => write!(f, "BTST"),
            OpCode::BCHG(_) => write!(f, "BCHG"),
            OpCode::BCLR(_) => write!(f, "BCLR"),
            OpCode::BSET(_) => write!(f, "BSET"),
            OpCode::MOVEP => write!(f, "MOVEP"),
            OpCode::MOVEA => write!(f, "MOVEA"),
            OpCode::MOVE => write!(f, "MOVE"),
            OpCode::MOVEFROMSR => write!(f, "MOVE"),
            OpCode::MOVETOCCR => write!(f, "MOVE"),
            OpCode::MOVETOSR => write!(f, "MOVE"),
            OpCode::NEGX => write!(f, "NEGX"),
            OpCode::CLR => write!(f, "CLR"),
            OpCode::NEG => write!(f, "NEG"),
            OpCode::NOT => write!(f, "NOT"),
            OpCode::EXT => write!(f, "EXT"),
            OpCode::NBCD => write!(f, "NBCD"),
            OpCode::SWAP => write!(f, "SWAP"),
            OpCode::PEA => write!(f, "PEA"),
            OpCode::ILLEGAL => write!(f, "ILLEGAL"),
            OpCode::TAS => write!(f, "TAS"),
            OpCode::TST => write!(f, "TST"),
            OpCode::TRAP => write!(f, "TRAP"),
            OpCode::LINK => write!(f, "LINK"),
            OpCode::UNLK => write!(f, "UNLK"),
            OpCode::MOVEUSP => write!(f, "MOVE"),
            OpCode::RESET => write!(f, "RESET"),
            OpCode::NOP => write!(f, "NOP"),
            OpCode::STOP => write!(f, "STOP"),
            OpCode::RTE => write!(f, "RTE"),
            OpCode::RTS => write!(f, "RTS"),
            OpCode::TRAPV => write!(f, "TRAPV"),
            OpCode::RTR => write!(f, "RTR"),
            OpCode::JSR => write!(f, "JSR"),
            OpCode::JMP => write!(f, "JMP"),
            OpCode::MOVEM => write!(f, "MOVEM"),
            OpCode::LEA => write!(f, "LEA"),
            OpCode::CHK => write!(f, "CHK"),
            OpCode::ADDQ => write!(f, "ADDQ"),
            OpCode::SUBQ => write!(f, "SUBQ"),
            OpCode::Scc(cond) => write!(f, "S{}", cond),
            OpCode::DBcc(cond) => write!(f, "DB{}", cond),
            OpCode::BRA => write!(f, "BRA"),
            OpCode::BSR => write!(f, "BSR"),
            OpCode::Bcc(cond) => write!(f, "B{}", cond),
            OpCode::MOVEQ => write!(f, "MOVEQ"),
            OpCode::DIVU => write!(f, "DIVU"),
            OpCode::DIVS => write!(f, "DIVS"),
            OpCode::SBCD => write!(f, "SBCD"),
            OpCode::OR => write!(f, "OR"),
            OpCode::SUB => write!(f, "SUB"),
            OpCode::SUBX => write!(f, "SUBX"),
            OpCode::SUBA => write!(f, "SUBA"),
            OpCode::EOR => write!(f, "EOR"),
            OpCode::CMPM => write!(f, "CMPM"),
            OpCode::CMP => write!(f, "CMP"),
            OpCode::CMPA => write!(f, "CMPA"),
            OpCode::MULU => write!(f, "MULU"),
            OpCode::MULS => write!(f, "MULS"),
            OpCode::ABCD => write!(f, "ABCD"),
            OpCode::EXG => write!(f, "EXG"),
            OpCode::AND => write!(f, "AND"),
            OpCode::ADD => write!(f, "ADD"),
            OpCode::ADDX => write!(f, "ADDX"),
            OpCode::ADDA => write!(f, "ADDA"),
            OpCode::ASLM | OpCode::ASL(_) => write!(f, "ASL"),
            OpCode::ASRM | OpCode::ASR(_) => write!(f, "ASR"),
            OpCode::LSLM | OpCode::LSL(_) => write!(f, "LSL"),
            OpCode::LSRM | OpCode::LSR(_) => write!(f, "LSR"),
            OpCode::ROXLM | OpCode::ROXL(_) => write!(f, "ROXL"),
            OpCode::ROXRM | OpCode::ROXR(_) => write!(f, "ROXR"),
            OpCode::ROLM | OpCode::ROL(_) => write!(f, "ROL"),
            OpCode::RORM | OpCode::ROR(_) => write!(f, "ROR"),
            OpCode::Unimplemented => write!(f, "UNIMPLEMENTED"),
        }
    }
}

pub fn decode(code: u16) -> OpCode {
    // println!("{:016b}", code);
    OPCODES
        .into_iter()
        .find(|opcode| opcode.compare(code))
        .unwrap()
}

fn get_bits(bits: Range<u8>, byte: u16) -> u16 {
    let mut mask: u16 = 0;
    let rshift = bits.start - 1;
    bits.for_each(|bit| mask = mask | 1 << bit - 1);

    (byte & mask) >> rshift
}

/*
    Size for opcodes that use Displacement are either 8 bits included in the
    16 bit instruction word or, when those 8 bits are 0, the 16 bits immediately
    following the instruction word.
*/
fn get_displacement_size(code: u16) -> Size {
    let bits = get_bits(1..9, code);
    match bits {
        0 => Size::Word,
        _ => Size::None, //Cheating here. It's technically Byte sized. TODO
    }
}
