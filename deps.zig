const std = @import("std");
const Pkg = std.build.Pkg;
const string = []const u8;

pub const cache = ".zigmod/deps";

pub fn addAllTo(exe: *std.build.LibExeObjStep) void {
    @setEvalBranchQuota(1_000_000);
    for (packages) |pkg| {
        exe.addPackage(pkg.pkg.?);
    }
    inline for (std.meta.declarations(package_data)) |decl| {
        const pkg = @as(Package, @field(package_data, decl.name));
        var llc = false;
        inline for (pkg.system_libs) |item| {
            exe.linkSystemLibrary(item);
            llc = true;
        }
        inline for (pkg.c_include_dirs) |item| {
            exe.addIncludeDir(@field(dirs, decl.name) ++ "/" ++ item);
            llc = true;
        }
        inline for (pkg.c_source_files) |item| {
            exe.addCSourceFile(@field(dirs, decl.name) ++ "/" ++ item, pkg.c_source_flags);
            llc = true;
        }
        if (llc) {
            exe.linkLibC();
        }
    }
}

pub const Package = struct {
    directory: string,
    pkg: ?Pkg = null,
    c_include_dirs: []const string = &.{},
    c_source_files: []const string = &.{},
    c_source_flags: []const string = &.{},
    system_libs: []const string = &.{},
};

const dirs = struct {
    pub const _root = "";
    pub const _93jjp4rc0htn = cache ++ "/../..";
    pub const _9jt7n3xad653 = cache ++ "/git/github.com/Hejsil/mecha";
};

pub const package_data = struct {
    pub const _93jjp4rc0htn = Package{
        .directory = dirs._93jjp4rc0htn,
        .pkg = Pkg{ .name = "lunez", .path = .{ .path = dirs._93jjp4rc0htn ++ "/src/main.zig" }, .dependencies = null },
    };
    pub const _9jt7n3xad653 = Package{
        .directory = dirs._9jt7n3xad653,
        .pkg = Pkg{ .name = "mecha", .path = .{ .path = dirs._9jt7n3xad653 ++ "/mecha.zig" }, .dependencies = null },
    };
    pub const _root = Package{
        .directory = dirs._root,
    };
};

pub const packages = &[_]Package{
    package_data._93jjp4rc0htn,
    package_data._9jt7n3xad653,
};

pub const pkgs = struct {
    pub const lunez = package_data._93jjp4rc0htn;
    pub const mecha = package_data._9jt7n3xad653;
};

pub const imports = struct {
    pub const lunez = @import(".zigmod/deps/../../src/main.zig");
    pub const mecha = @import(".zigmod/deps/git/github.com/Hejsil/mecha/mecha.zig");
};
