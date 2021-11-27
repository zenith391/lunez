const std = @import("std");
const mecha =  @import("mecha");
const Parser = mecha.Parser;
const oneOf = mecha.oneOf;
const discard = mecha.discard;
const combine = mecha.combine;
const many = mecha.many;
const map = mecha.map;
const utf8 = mecha.utf8;
const ascii = mecha.ascii;
const opt = mecha.opt;
const ref = mecha.ref;

const Allocator = std.mem.Allocator;
var alloc: *Allocator = undefined;

const ws = discard(many(oneOf(.{
    utf8.char(0x20), // space
    utf8.char(0x0A), // line feed
    utf8.char(0x0D), // carriage return
    utf8.char(0x09), // horizontal tab
}), .{ .collect = false }));

const literalString = oneOf(.{
    combine(.{
        discard(utf8.char('\'')),
        many(stringChar, .{ .collect = false }),
        discard(utf8.char('\''))
    }),
    combine(.{
        discard(utf8.char('"')),
        many(stringChar, .{ .collect = false }),
        discard(utf8.char('"'))
    })
});

const stringChar = oneOf(.{
    utf8.range(0x0020, '"' - 1),
    utf8.range('"' + 1, '\\' - 1),
    utf8.range('\\' + 1, 0x10FFFF)
});

const name = many(oneOf(.{
    ascii.alphanum,
    ascii.range('_', '_')
}), .{ .collect = false, .min = 1 });

const number = many(oneOf(.{
    ascii.digit(10),
    ascii.range('.', '.')
}), .{ .collect = false, .min = 1 });

const namelist = many(name, .{ .min = 1, .separator = combine(.{
    ascii.char(','), discard(opt(ws))
})});

const lvar = oneOf(.{
    map(Expr, varPreExpConv, combine(.{ name, discard(opt(ws)), many(
        combine(.{
            ascii.char('.'), discard(opt(ws)), ref(lvar_ref)
        }), .{ .min = 1 }) })),
    map(Expr, varNameConv, name),
});

fn lvar_ref() Parser(Expr) {
    return lvar;
}

fn varNameConv(arg: anytype) Expr {
    return Expr { .Var = .{ .Name = arg } };
}

fn varPreExpConv(arg: anytype) Expr {
    var lhs = Expr { .Var = .{ .Name = arg[0] } };
    
    for (arg[1]) |rhs| {
        var lhsDupe = alloc.create(Expr) catch unreachable;
        lhsDupe.* = lhs;

        var rhsDupe = alloc.create(Expr) catch unreachable;
        rhsDupe.* = rhs;

        // what's detected as var is actually a string
        if (rhsDupe.* == .Var) {
            const new = .{ .LiteralString = rhsDupe.Var.Name };
            rhsDupe.* = new;
        }

        lhs = Expr {
            .Index = .{
                .lhs = lhsDupe,
                .rhs = rhsDupe,
            }
        };
    }
    return lhs;
}

const varlist = many(lvar, .{ .min = 1, .separator = combine(.{
    ascii.char(','), discard(opt(ws))
})});

pub const FunctionCall = struct {
    callee: Var,
    args: []const Expr
};

pub const Var = union(enum) {
    Name: []const u8,

    pub fn format(value: Var, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (value) {
            .Name => |varName| {
                try writer.writeAll(varName);
            }
        }
    }
};

const TableConstructor = struct {
    pub const TableEntry = struct {
        key: Expr,
        value: Expr
    };

    entries: []const TableEntry
};

pub const Expr = union(enum) {
    Nil: void,
    Var: Var,
    LiteralString: []const u8,
    Number: f64,
    Boolean: bool,
    BinaryOperation: struct {
        lhs: *const Expr,
        op: []const u8,
        rhs: *const Expr
    },
    FunctionCall: FunctionCall,
    FunctionDefinition: struct {
        stats: []Stat,
        argNames: [][]const u8
    },
    TableConstructor: TableConstructor,
    Index: struct {
        lhs: *const Expr,
        rhs: *const Expr
    }
};

pub const Stat = union(enum) {
    NoOp: void, // ;
    Definition: struct {
        vars: []const Var,
        exprs: []const Expr
    },
    LocalDefinition: struct {
        names: [][]const u8,
        exprs: []const Expr
    },
    If: struct {
        /// The condition expression
        expr: Expr,
        statements: []const Stat
    },
    Return: struct {
        expressions: []const Expr
    },
    FunctionCall: FunctionCall
};

const boolean = mecha.asStr(oneOf(.{
    mecha.string("true"),
    mecha.string("false")
}));

const functiondef =
    combine(.{
        discard(opt(ws)), mecha.string("function"), discard(opt(ws)),
        ascii.char('('), opt(namelist), ascii.char(')'), discard(opt(ws)),
        ref(block_ref), discard(opt(ws)),
        mecha.string("end"), discard(opt(ws))
    });

const field = combine(.{
    ascii.char('['), ws, ref(exp_ref), ws, ascii.char(']'), ws,
    ascii.char('='), ws,
    ref(exp_ref)
});

const fieldsep = oneOf(.{ ascii.char(','), ascii.char(';') });

const fieldlist = combine(.{
    field,
    many(combine(.{
        ws, fieldsep, ws, field
    }), .{}),
    opt(fieldsep)
});

const tableconstructor =
    combine(.{
        ascii.char('{'), discard(opt(ws)),
        opt(fieldlist),
        discard(opt(ws)), ascii.char('}')
    });

const constexpr = oneOf(.{
    map(Expr, functionDefExprConv, functiondef),
    map(Expr, numberConv, number),
    map(Expr, booleanConv, boolean),
    map(Expr, stringConv, literalString),
    map(Expr, functionCallExprConv, functioncall),
    map(Expr, tableConstructorConv, tableconstructor),
    map(Expr, parenthesisConv, combine(.{
        discard(opt(ws)), ascii.char('('), discard(opt(ws)), ref(exp_ref), discard(opt(ws)), ascii.char(')'), discard(opt(ws))
    })),
    lvar
});

const exp = oneOf(.{
    compOp,
    concatOp,
    binop2,
    binop1,
    constexpr
});

const compOp = map(Expr, binopConv, combine(.{
    many(combine(.{
        concatOp, discard(opt(ws)), mecha.asStr(oneOf(.{ mecha.string("=="), mecha.string("~=") }))
    }), .{ .min = 1 }), discard(opt(ws)), ref(exp_ref)
}));

const concatOp = oneOf(.{
    map(Expr, binopConv, combine(.{
        many(combine(.{
            binop2, discard(opt(ws)), mecha.asStr(mecha.string("..")), discard(opt(ws))
        }), .{ .min = 1 }), ref(exp_ref)
    })),
    binop2
});

const binop2 = oneOf(.{
    map(Expr, binopConv, combine(.{
        many(combine(.{
            binop1, discard(opt(ws)), mecha.asStr(oneOf(.{
                mecha.string("*"), mecha.string("/"), mecha.string("//"),
                mecha.string("%") })), discard(opt(ws))
        }), .{ .min = 1 }), ref(binop2_ref)
    })),
    binop1
});

const binop1 = oneOf(.{
    map(Expr, binopConv, combine(.{
        many(combine(.{
            constexpr, discard(opt(ws)), mecha.asStr(oneOf(.{ mecha.string("+"), mecha.string("-") })), discard(opt(ws))
        }), .{ .min = 1 }), ref(binop1_ref)
    })),
    constexpr
});

fn binop2_ref() Parser(Expr) {
    return binop2;
}

fn binop1_ref() Parser(Expr) {
    return binop1;
}

fn exp_ref() Parser(Expr) {
    return exp;
}

fn parenthesisConv(arg: Expr) Expr {
    return arg;
}

fn booleanConv(arg: []const u8) Expr {
    return Expr { .Boolean = std.mem.eql(u8, arg, "true") };
}

fn stringConv(arg: []const u8) Expr {
    return Expr { .LiteralString = arg };
}

fn numberConv(arg: []const u8) Expr {
    return Expr { .Number = std.fmt.parseFloat(f64, arg) catch unreachable };
}

fn nameConv(arg: []const u8) Expr {
    if (std.mem.eql(u8, arg, "nil")) {
        return Expr { .Nil = .{} };
    }
    return Expr { .Var = .{ .Name = arg } };
}

fn functionCallExprConv(arg: anytype) Expr {
    return Expr {
        .FunctionCall = .{
            .callee = arg[0].Var,
            .args = arg[1] orelse &[0]Expr {}
        }
    };
}

fn functionDefExprConv(arg: anytype) Expr {
    return Expr { .FunctionDefinition = .{
        .stats = arg[1],
        .argNames = arg[0] orelse &[0][]const u8 {}
    } };
}

fn tableConstructorConv(arg: anytype) Expr {
    var entries = std.ArrayList(TableConstructor.TableEntry).init(alloc);
    if (arg) |flist| {
        // first entry
        entries.append(.{
            .key = flist[0][0],
            .value = flist[0][1]
        }) catch unreachable;

        // other entries
        for (flist[1]) |entry| {
            entries.append(.{
                .key = entry[0],
                .value = entry[1]
            }) catch unreachable;
        }
    }

    return Expr {
        .TableConstructor = .{
            .entries = entries.toOwnedSlice()
        }
    };
}

fn binopConv(arg: anytype) Expr {
    const lhs = alloc.create(Expr) catch unreachable;
    // This handles operator precedence and left associativity

    //std.log.debug("arg = {d}, op = {s}", .{arg[0].len, arg[0][arg[0].len-1][1]});
    if (arg[0].len == 1) {
        //std.log.debug("lhs: {any}", .{arg[0][0][0]});
        lhs.* = arg[0][0][0];
    } else {
        const newArgs: @TypeOf(arg) = .{ arg[0][0..arg.len-1], arg[0][1][0] };
        lhs.* = binopConv(newArgs);
    }

    const rhs = alloc.create(Expr) catch unreachable;
    //std.log.debug("rhs: {any}", .{arg[1]});
    rhs.* = arg[1];

    return Expr { .BinaryOperation = .{ .lhs = lhs, .op = arg[0][arg[0].len-1][1], .rhs = rhs }};
}

const explist = combine(.{
    many(exp, .{ .min = 1, .separator = combine(.{
        ascii.char(','), discard(opt(ws))
    })})
});

fn explist_ref() Parser([]Expr) {
    return explist;
}

fn noOpConv(arg: void) Stat {
    _ = arg;
    return Stat { .NoOp = .{} };
}

fn definitionConv(arg: anytype) Stat {
    var vars = alloc.alloc(Var, arg[0].len) catch unreachable;
    for (arg[0]) |expr, i| vars[i] = expr.Var;
    return Stat {
        .Definition = .{
            .vars = vars,
            .exprs = arg[1]
        }
    };
}

fn localDefinitionConv(arg: anytype) Stat {
    return Stat {
        .LocalDefinition = .{
            .names = arg[0],
            .exprs = arg[1]
        }
    };
}

fn functionCallStatConv(arg: anytype) Stat {
    return Stat {
        .FunctionCall = .{
            .callee = arg[0].Var,
            .args = arg[1] orelse &[0]Expr {}
        }
    };
}

fn localFunctionDefConv(arg: anytype) Stat {
    var names = alloc.alloc([]const u8, 1) catch unreachable;
    var exprs = alloc.alloc(Expr, 1) catch unreachable;
    names[0] = arg[0];
    exprs[0] = Expr { .FunctionDefinition = .{
        .stats = arg[2],
        .argNames = arg[1] orelse &[0][]const u8 {}
    } };
    return Stat {
        .LocalDefinition = .{
            .names = names,
            .exprs = exprs
        }
    };
}

fn functionDefConv(arg: anytype) Stat {
    var vars = alloc.alloc(Var, 1) catch unreachable;
    var exprs = alloc.alloc(Expr, 1) catch unreachable;
    vars[0] = arg[0].Var;
    exprs[0] = Expr { .FunctionDefinition = .{
        .stats = arg[2],
        .argNames = arg[1] orelse &[0][]const u8 {}
    } };
    return Stat {
        .Definition = .{
            .vars = vars,
            .exprs = exprs
        }
    };
}

fn ifStatConv(arg: anytype) Stat {
    return Stat {
        .If = .{
            .expr = arg[0],
            .statements = arg[1]
        }
    };
}

fn doBlockStatConv(arg: anytype) Stat {
    return Stat {
        .If = .{
            .expr = .{ .Boolean = true },
            .statements = arg
        }
    };
}

fn retStatConv(arg: anytype) Stat {
    return Stat {
        .Return = .{
            .expressions = arg orelse &[0]Expr {}
        }
    };
}

const args = oneOf(.{
    combine(.{ ascii.char('('), discard(opt(ws)), opt(ref(explist_ref)), discard(opt(ws)), ascii.char(')') })
});

const functioncall = oneOf(.{
    combine(.{
        discard(opt(ws)), lvar, discard(opt(ws)), args, discard(opt(ws))
    })
});

const funcname = name;

const shortlocalfunctiondef =
    combine(.{
        discard(opt(ws)), mecha.string("local"), discard(ws), mecha.string("function"), discard(ws), name, discard(opt(ws)),
        ascii.char('('), opt(namelist), ascii.char(')'), discard(opt(ws)),
        ref(block_ref), discard(opt(ws)),
        mecha.string("end"), discard(opt(ws))
    });

const shortfunctiondef =
    combine(.{
        discard(opt(ws)), mecha.string("function"), discard(ws), lvar, discard(opt(ws)),
        ascii.char('('), opt(namelist), ascii.char(')'), discard(opt(ws)),
        ref(block_ref), discard(opt(ws)),
        mecha.string("end"), discard(opt(ws))
    });

const stat = oneOf(.{
    map(Stat, noOpConv, combine(.{ ascii.char(';'), discard(opt(ws)) })),
    map(Stat, definitionConv, combine(.{
        varlist, discard(opt(ws)), ascii.char('='), discard(opt(ws)), explist, discard(opt(ws))
    })),
    map(Stat, functionCallStatConv, functioncall),
    map(Stat, localDefinitionConv, combine(.{
        discard(opt(ws)), mecha.string("local"), discard(opt(ws)), namelist, discard(opt(ws)), ascii.char('='), discard(opt(ws)), explist, discard(opt(ws))
    })),
    map(Stat, functionDefConv, shortfunctiondef),
    map(Stat, localFunctionDefConv, shortlocalfunctiondef),
    map(Stat, ifStatConv, combine(.{
        discard(opt(ws)), mecha.string("if"), discard(ws), exp, discard(ws), mecha.string("then"), discard(opt(ws)), ref(block_ref),
        discard(opt(ws)), mecha.string("end"), discard(opt(ws))
    })),
    map(Stat, doBlockStatConv, combine(.{
        discard(opt(ws)), mecha.string("do"), discard(ws), ref(block_ref), mecha.string("end"), discard(opt(ws))
    }))
});

const retstat = map(Stat, retStatConv,
    combine(.{ mecha.string("return"), discard(ws), opt(explist), discard(opt(ascii.char(';'))) })
);

fn blockConv(arg: anytype) []Stat {
    const slice: []const Stat = blk: {
        if (arg[1]) |ret| {
            break :blk &[1]Stat { ret };
        } else {
            break :blk &[0]Stat {};
        }
    };
    return std.mem.concat(alloc, Stat, &[_][]const Stat { arg[0], slice }) catch unreachable;
}

const block = map([]Stat, blockConv, combine(.{
    many(stat, .{ }),
    opt(retstat)
}));

fn block_ref() Parser([]Stat) {
    return block;
}

pub const chunk = combine(.{ block, mecha.eos });

pub fn parse(allocator: *Allocator, source: []const u8) !mecha.ParserResult(@TypeOf(chunk)) {
    alloc = allocator;
    return (try chunk(allocator, source)).value;
}
