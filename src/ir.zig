/// There are 256 usable registers in total.
pub const Register = u8;

pub const Instruction = union(enum) {
	LoadString: struct {
		constant: []const u8,
		destination: Register
	},
	LoadNumber: struct {
		number: f32,
		destionation: Register
	},
	LoadNil: struct {
		destionation: Register
	},
	GetEnv: struct {
		index: Register,
		destination: Register
	},
	SetEnv: struct {
		index: Register,
		source: Register
	},
	SetLocal: struct {
		id: u32,
		source: Register
	},
	CallFunction: struct {
		function: Register, // there register in which the function has been loaded
		numArgs: Register, // register containing the number of arguments
		argStart: Register, // the start of argument, assumes (argStart + numArgs - 1) <= std.math.maxInt(Register)
	}
};
