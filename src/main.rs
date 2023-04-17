mod address_modes;
mod opcodes;
mod size;

use crate::address_modes::{AddressMode, AddressRegister, DataRegister};
use crate::opcodes::{decode, Condition, OpCode};
use crate::size::Size;
use std::env;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::prelude::*;

struct Instruction {
    opcode: OpCode,
    // code: u16,
    size: Size,
    data: u32,
    address_mode: AddressMode,
    program_counter_increment: usize,
}

impl Instruction {
    fn new(opcode: OpCode, size: Size, address_mode: AddressMode) -> Instruction {
        let program_counter_increment =
            opcode.extensions(&size) + address_mode.extensions(&size) + 2; // bytes for extension words, byes for address mode + 2 bytes for instruction itself
        Instruction {
            opcode,
            size,
            data: 0,
            program_counter_increment,
            address_mode,
            // code,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.size {
            Size::None => write!(f, "{}", self.opcode),
            _ => write!(f, "{}.{}", self.opcode, self.size),
        }
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut pc = 512;
    let file_path = &args[1];
    let mut buffer = Vec::new();

    let mut file = File::open(file_path)?;
    let bytes_read = if let Ok(bytes_read) = file.read_to_end(&mut buffer) {
        bytes_read
    } else {
        0
    };

    println!("bytes read: {}", bytes_read);
    let mut go = true;
    let mut increment;
    while go && pc < bytes_read {
        (go, increment) = read_instruction(&buffer, pc);
        pc += increment;
    }
    Ok(())
}

fn read_instruction(buffer: &Vec<u8>, pc: usize) -> (bool, usize) {
    let word = (buffer[pc] as u16) << 8 | buffer[pc + 1] as u16;
    let possible_data = get_long_word(&buffer, pc);
    let instruction = find_opcode(word, possible_data);
    println!(
        "{:05x} {:04x} {} inc: {}",
        (pc),
        word,
        instruction,
        instruction.program_counter_increment
    );
    if pc == 0x200 {
        print_nibbles(word);
    }
    if matches!(instruction.opcode, OpCode::Unimplemented) {
        print_nibbles(word);
        return (false, 2);
    }
    (true, instruction.program_counter_increment)
}

fn find_opcode(code: u16, data: u32) -> Instruction {
    let op = decode(code);
    let size = op.size(code);
    let address_mode = op.address_mode(code);
    Instruction::new(op, size, address_mode)
}

fn get_long_word(buffer: &Vec<u8>, pc: usize) -> u32 {
    (buffer[pc + 2] as u32) << 24        // High 8 bits of Word
        | (buffer[pc + 3] as u32) << 16  // Low 8 bits of Word / 8 bits of Byte
        | (buffer[pc + 4] as u32) << 8   // Last 16 bits for Long Word
        | (buffer[pc + 5] as u32)
}

fn print_nibbles(bits: u16) {
    let first_nibble = bits >> 12;
    let second_nibble = (bits & 0x0F00) >> 8;
    let third_nibble = (bits & 0x00F0) >> 4;
    let last_nibble = bits & 0x000F;
    println!(
        "{:04b} {:04b} {:04b} {:04b}",
        first_nibble, second_nibble, third_nibble, last_nibble
    );
}
