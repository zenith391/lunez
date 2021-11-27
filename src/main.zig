const std = @import("std");
const parser = @import("parser.zig");
const Allocator = std.mem.Allocator;

/// Value for the call stack to be limited at in order to avoid an actual stack overflow from whom Lunez doesn't know how to recover.
/// It is implemented by throwing a 'stack overflow' error.
/// Set to 0 to disable.
pub const MAX_CALL_STACK_SIZE = 1000;

const LuaFunction = struct {
    stats: []const parser.Stat,
    argNames: []const []const u8
};

const LuaTable = std.HashMap(LuaValue, LuaValue, ValueContext, std.hash_map.default_max_load_percentage);

const ValueContext = struct {
    pub fn hash(self: ValueContext, value: LuaValue) u64 {
        _ = self;
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&value));
    }

    pub fn eql(self: ValueContext, a: LuaValue, b: LuaValue) bool {
        _ = self;
        return std.mem.eql(u8, std.mem.asBytes(&a), std.mem.asBytes(&b));
    }
};

const LuaValues = std.StringHashMap(LuaValue);

const LuaValue = union(enum) {
    Number: f64,
    String: []const u8,
    Boolean: bool,
    Nil: void,
    CFunction: fn(env: *LuaEnv, args: []const LuaValue) anyerror![]LuaValue,
    Function: LuaFunction,
    Table: LuaTable,

    pub fn getTypeName(self: LuaValue) []const u8 {
        return switch (self) {
            .Number => return "number",
            .Boolean => return "boolean",
            .String => return "string",
            .Nil => return "nil",
            .CFunction => return "function",
            .Function => return "function",
            .Table => return "table"
        };
    }

    pub fn equals(self: LuaValue, other: LuaValue) bool {
        if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;

        return switch (self) {
            .Number => |num| num == other.Number,
            .Boolean => |boolean| boolean == other.Boolean,
            .String => |str| std.mem.eql(u8, str, other.String),
            .Nil => other == .Nil,
            else => false
        };
    }
};
const nil = LuaValue { .Nil = .{} };

const VariableAvailability = enum {
    Local,
    Global,
    None
};

const LuaEnv = struct {
    allocator: *Allocator,
    callStack: CallStack,
    // TODO: replace upvalues by local variables in the first call frame (which lives for the duration of the whole program)
    upvalues: LuaValues,

    pub fn init(allocator: *Allocator) LuaEnv {
        return LuaEnv {
            .callStack = CallStack.init(allocator),
            .upvalues  = LuaValues.init(allocator),
            .allocator = allocator
        };
    }

    pub fn getAvailability(self: *const LuaEnv, variable: parser.Var) VariableAvailability {
        var i: usize = self.callStack.items.len;
        while (i > 0) : (i -= 1) {
            const frame = self.callStack.items[i - 1];
            if (frame.locals.contains(variable.Name)) {
                return .Local;
            }
        }
        return if (self.upvalues.contains(variable.Name)) .Global else .None;
    }

    pub fn getCurrentFrame(self: *const LuaEnv) *CallFrame {
        return &self.callStack.items[self.callStack.items.len - 1];
    }

    /// Get the closest call scope's value with this name or if none, the global with this name, or if none, nil
    pub fn getVar(self: *LuaEnv, variable: parser.Var) LuaValue {
        //std.log.scoped(.env).debug("Get var {}", .{variable});
        var i: usize = self.callStack.items.len;
        while (i > 0) : (i -= 1) {
            const frame = self.callStack.items[i - 1];
            if (frame.locals.get(variable.Name)) |value| {
                // const parseVar = variable;
                // if (value != .Nil and parseVar == .PrefixExpression) {
                //     return resolveExpr(self, parseVar.PrefixExpression.exp.*);
                // }

                return value;
            }
        }
        if (self.upvalues.get(variable.Name)) |value| {
            return value;
        }

        return nil;
    }

    pub fn setUpvalue(self: *LuaEnv, variable: parser.Var, value: LuaValue) !void {
        //std.log.scoped(.env).debug("Set upvalue {} to value {}", .{variable, value});
        try self.upvalues.put(variable.Name, value);
    }

    pub fn setLocal(self: *LuaEnv, variable: parser.Var, value: LuaValue) !void {
        //std.log.scoped(.env).debug("Set local {} to value {}", .{variable, value});
        var i: usize = self.callStack.items.len;
        while (i > 0) : (i -= 1) {
            const frame = &self.callStack.items[i - 1];
            if (frame.locals.contains(variable.Name)) {
                frame.locals.putAssumeCapacity(variable.Name, value);
                return;
            }
        }
        try self.getCurrentFrame().locals.put(variable.Name, value);
    }

    const ThrowError = error {
        LuaError
    };

    /// This function calls the message handler and always returns error.LuaError
    /// in order to interrupt the current control flow.
    pub fn throwString(self: *LuaEnv, comptime msg: []const u8, fmt: anytype) ThrowError {
        var buf: [4096]u8 = undefined;
        return self.throwError(LuaValue { .String = std.fmt.bufPrint(&buf, msg, fmt) catch unreachable });
    }

    /// This function calls the message handler and always returns error.LuaError
    /// in order to interrupt the current control flow.
    pub fn throwError(self: *LuaEnv, msg: LuaValue) ThrowError {
        while (true) {
            var callFrame = self.callStack.pop();
            callFrame.deinit();
            if (callFrame.messageHandler) |msgHandler| {
                msgHandler(self, msg);
                return error.LuaError; // convenience error to stop the current control flow
            }
        }
    }

    pub fn deinit(self: *LuaEnv) void {
        while (self.callStack.popOrNull()) |*callFrame| {
            callFrame.deinit();
        }

        self.callStack.deinit();
        self.upvalues.deinit();
    }
};

const CallFrame = struct {
    locals: LuaValues,
    statements: []const parser.Stat,
    statIdx: usize = 0,
    /// Called on error
    messageHandler: ?fn(env: *LuaEnv, msg: LuaValue) void = null,

    pub fn init(allocator: *Allocator, statements: []const parser.Stat) CallFrame {
        return CallFrame {
            .locals = LuaValues.init(allocator),
            .statements = statements
        };
    }

    pub fn deinit(self: *CallFrame) void {
        self.locals.deinit();
    }
};

const CallStack = std.ArrayList(CallFrame);

const ResolveExpressionError = LuaEnv.ThrowError || FunctionCallError;

pub fn resolveExpr(env: *LuaEnv, expr: parser.Expr) ResolveExpressionError!LuaValue {
    switch (expr) {
        .LiteralString => |string| {
            return LuaValue { .String = string };
        },
        .Number => |number| {
            return LuaValue { .Number = number };
        },
        .Boolean => |boolean| {
            return LuaValue { .Boolean = boolean };
        },
        .Var => |str| {
            return env.getVar(str);
        },
        .Nil => return nil,
        .FunctionCall => |call| {
            const values = try callFunction(env, call);
            return if (values.len > 0) values[0] else nil; // TODO: handle multiple returns!
        },
        .FunctionDefinition => |def| {
            const func = LuaFunction {
                .stats = def.stats,
                .argNames = def.argNames
            };
            return LuaValue { .Function = func };
        },
        .BinaryOperation => |op| {
            const lhs = try resolveExpr(env, op.lhs.*);
            const rhs = try resolveExpr(env, op.rhs.*);
            if (std.mem.eql(u8, op.op, "+") or std.mem.eql(u8, op.op, "-")) {
                if (lhs != .Number) {
                    return env.throwString("attempt to perform arithmetic on a {s} value", .{ lhs.getTypeName() });
                } else if (rhs != .Number) {
                    return env.throwString("attempt to perform arithmetic on a {s} value", .{ rhs.getTypeName() });
                }
            }

            if (std.mem.eql(u8, op.op, "+")) {
                return LuaValue { .Number = lhs.Number + rhs.Number };
            } else if (std.mem.eql(u8, op.op, "-")) {
                return LuaValue { .Number = lhs.Number - rhs.Number };
            } else if (std.mem.eql(u8, op.op, "*")) {
                return LuaValue { .Number = lhs.Number * rhs.Number };
            } else if (std.mem.eql(u8, op.op, "/")) {
                return LuaValue { .Number = lhs.Number / rhs.Number };
            }

            if (std.mem.eql(u8, op.op, "..")) {
                if (lhs != .String or rhs != .String) {
                    return env.throwString("attempt to concatenate a {s} value", .{ rhs.getTypeName() });
                }
                const str = std.mem.concat(env.allocator, u8, &[_][]const u8{ lhs.String, rhs.String }) catch unreachable;
                return LuaValue { .String = str };
            }

            if (std.mem.eql(u8, op.op, "==")) {
                return LuaValue { .Boolean = lhs.equals(rhs) };
            } else if (std.mem.eql(u8, op.op, "~=")) {
                return LuaValue { .Boolean = !lhs.equals(rhs) };
            }

            return env.throwString("binary operation {s} has not yet been implemented", .{ op.op });
        }
    }
}

/// If there is a lua error, returns null instead of error.LuaError
/// This can be used to break out of execution loop as when error.LuaError is returned,
/// the error has already been catched and handled by the message handler.
pub fn resolveExprNullable(env: *LuaEnv, expr: parser.Expr) !?LuaValue {
    return resolveExpr(env, expr) catch |err| switch (err) {
        error.LuaError => return null,
        else => return err
    };
}

pub fn resolveExprs(env: *LuaEnv, expressions: []const parser.Expr) ![]const LuaValue {
    var values = try env.allocator.alloc(LuaValue, expressions.len);
    for (expressions) |expr, i| {
        values[i] = try resolveExpr(env, expr);
    }
    return values;
}

fn toString(env: *LuaEnv, value: LuaValue) ![]const u8 {
    const str: []const u8 = switch (value) {
        .CFunction => |func| return try std.fmt.allocPrint(env.allocator, "function 0x{x}", .{@ptrToInt(func)}),
        .Function => |func| return  try std.fmt.allocPrint(env.allocator, "function 0x{x}", .{@ptrToInt(func.stats.ptr)}),
        .String => |str| return str,
        .Number => |num| return try std.fmt.allocPrint(env.allocator, "{d}", .{num}),
        .Boolean => |boolean| return try std.fmt.allocPrint(env.allocator, "{}", .{boolean}),
        .Nil => return "nil",
        .Table => |table| return try std.fmt.allocPrint(env.allocator, "table 0x{x}", .{@ptrToInt(&table)}),
    };
    return str;
}

fn luaCreateError(env: *LuaEnv, args: []const LuaValue) LuaEnv.ThrowError![]LuaValue {
    const errorObject: LuaValue = blk: {
        if (args.len > 0) {
            break :blk args[0];
        } else {
            break :blk LuaValue { .String = "(error object is a nil value)" };
        }
    };

    return env.throwError(errorObject);
}

fn luaToString(env: *LuaEnv, args: []const LuaValue) ![]LuaValue {
    if (args.len != 1) {
        return env.throwString("bad argument #1 to 'tostring' (value expected)", .{});
    }

    const str = try toString(env, args[0]);
    const values = try env.allocator.alloc(LuaValue, 1);
    values[0] = LuaValue { .String = str };
    return values;
}

fn luaPrint(env: *LuaEnv, args: []const LuaValue) ![]LuaValue {
    _ = env;
    const stdout = std.io.getStdOut().writer();
    for (args) |arg| {
        try stdout.print("{s}\t", .{ try toString(env, arg) });
    }
    try stdout.print("\n", .{});
    return &[0]LuaValue {};
}

fn rootError(env: *LuaEnv, msg: LuaValue) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("error: ", .{}) catch unreachable;
    _ = luaPrint(env, &.{ msg }) catch unreachable;
    env.callStack.clearAndFree();
}

fn createTable(env: *LuaEnv, args: []const LuaValue) ![]LuaValue {
    _ = args;

    const values = try env.allocator.alloc(LuaValue, 1);
    values[0] = LuaValue {
        .Table = LuaTable.init(env.allocator)
    };
    return values;
}

const FunctionCallError = anyerror; // TODO: narrow it down
pub fn callFunction(env: *LuaEnv, call: parser.FunctionCall) FunctionCallError![]const LuaValue {
    const values = try resolveExprs(env, call.args);
    defer env.allocator.free(values);

    if (MAX_CALL_STACK_SIZE != 0 and env.callStack.items.len >= MAX_CALL_STACK_SIZE) {
        return env.throwString("stack overflow", .{});
    }

    const value = env.getVar(call.callee);
    switch (value) {
        .CFunction => |func| {
            return try func(env, values);
        },
        .Function => |func| {
            var newFrame = CallFrame.init(env.allocator, func.stats);
            try env.callStack.append(newFrame);
            for (func.argNames) |name, i| {
                if (i < values.len) {
                    try env.getCurrentFrame().locals.put(name, values[i]);
                }
            }
            return try evalLoop(env);
        },
        else => |v| {
            return env.throwString("attempt to call a {s} value (global '{}')", .{v.getTypeName(), call.callee});
        }
    }
    return &[0]LuaValue {};
}

pub fn evalLoop(env: *LuaEnv) anyerror![]const LuaValue {
    var targetSize: usize = env.callStack.items.len - 1; // only stop when current function is finished
    // const frame = env.getCurrentFrame();
    // defer {
    //     frame.deinit();
    //     _ = env.callStack.pop(); // pop it
    // }

    while (true) {
        // It can abrutly be under the target size if an error was thrown that went down through this call frame
        if (env.callStack.items.len <= targetSize) {
            // in that case, we propagate the LuaError
            return error.LuaError;
        }
        
        const frame = env.getCurrentFrame();
        if (frame.statements.len == frame.statIdx) {
            frame.deinit();
            _ = env.callStack.pop(); // pop it
            continue;
        }
        const stat = frame.statements[frame.statIdx];
        frame.statIdx += 1;
        switch (stat) {
            .NoOp => {},
            .Definition => |defs| {
                for (defs.vars) |variable, i| {
                    const expr = if (i < defs.exprs.len) defs.exprs[i]
                        else parser.Expr { .Nil = .{} };
                    const value = (try resolveExprNullable(env, expr)) orelse continue;
                    if (env.callStack.items.len == 0) break;
                    if (env.getAvailability(variable) == .Local) {
                        env.setLocal(variable, value) catch unreachable;
                    } else {
                        try env.setUpvalue(variable, value);
                    }
                }
            },
            .LocalDefinition => |defs| {
                const currentFrame = &env.callStack.items[env.callStack.items.len - 1];
                for (defs.names) |variable, i| {
                    const expr = if (i < defs.exprs.len) defs.exprs[i]
                        else parser.Expr { .Nil = .{} };
                    const value = (try resolveExprNullable(env, expr)) orelse continue;
                    if (env.callStack.items.len == 0) break;
                    try currentFrame.locals.put(variable, value);
                }
            },
            .FunctionCall => |call| {
                _ = callFunction(env, call) catch |err| switch (err) {
                    error.LuaError => continue,
                    else => return err
                };
            },
            .If => |ifStat| {
                const condition = (try resolveExprNullable(env, ifStat.expr)) orelse continue;
                const executeBlock = switch (condition) {
                    .Nil => false,
                    .Boolean => |boolean| boolean, // execute block if 'true' and do not execute if 'false'
                    else => true
                };

                if (executeBlock) {
                    var newFrame = CallFrame.init(env.allocator, ifStat.statements);
                    try env.callStack.append(newFrame);
                }
            },
            .Return => |retStat| {
                const values = try resolveExprs(env, retStat.expressions);
                while (env.callStack.items.len > targetSize) {
                    const f = env.getCurrentFrame();
                    f.deinit();
                    _ = env.callStack.pop();
                }
                return values;
            }
        }
    }

    return &[0]LuaValue {};
}

pub fn run(allocator: *Allocator, text: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const parsed = parser.parse(&arena.allocator, text) catch |err| switch (err) {
        error.ParserFailed => {
            // it didn't provide any useful information is stack trace anyway
            std.log.crit("parser failed", .{});
            return;
        },
        else => return err
    };

    //std.log.info("lua: {any}", .{parsed});
    var env = LuaEnv.init(&arena.allocator);
    defer env.deinit();

    // The top frame, the one corresponding to the executed chunk
    var defaultFrame = CallFrame.init(allocator, parsed);
    defaultFrame.messageHandler = rootError;
    try env.callStack.append(defaultFrame);

    // (very) basic standard library
    try env.setUpvalue(.{ .Name = "_VERSION" }, LuaValue { .String = "Lua 5.3" });
    try env.setUpvalue(.{ .Name = "print" }, LuaValue { .CFunction = luaPrint });
    try env.setUpvalue(.{ .Name = "tostring" }, LuaValue { .CFunction = luaToString });
    try env.setUpvalue(.{ .Name = "error" }, LuaValue { .CFunction = luaCreateError });
    try env.setUpvalue(.{ .Name = "newTable" }, LuaValue { .CFunction = createTable });

    _ = evalLoop(&env) catch |err| switch (err) {
        error.LuaError => {},
        else => return err
    };
}

pub fn do_file(allocator: *Allocator, path: []const u8) !void {
    const file = std.fs.cwd().openFile(path, .{ .read = true }) catch |err| {
        std.log.err("could not open file: {s}", .{@errorName(err)});
        return;
    };

    const all = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(all);
    try run(allocator, all);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .enable_memory_limit = true }) {};
    defer {
        //std.log.info("Used {} of memory.", .{std.fmt.fmtIntSizeDec(gpa.total_requested_bytes)});
        _ = gpa.deinit();
    }
    const allocator = &gpa.allocator;

    var args = try std.process.argsAlloc(allocator);
    defer allocator.free(args);
    
    //var logging = std.heap.loggingAllocator(allocator);

    if (args.len > 1) {
        const path = args[1];
        try do_file(allocator, path);
    } else {
        std.debug.print("Nelua 5  Copyright (C) zenith391\n", .{});
        const stdin = std.io.getStdIn().reader();
        while (true) {
            std.debug.print("> ", .{});
            const line = try stdin.readUntilDelimiterAlloc(allocator, '\n', std.math.maxInt(usize));
            defer allocator.free(line);
            run(allocator, line) catch |err| {
                std.log.err("{s}", .{@errorName(err)});
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
            };
        }
    }
}
