const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

fn intToStr(buf: []u8, v: i32) []const u8 {
    return std.fmt.bufPrint(buf, "{d}", .{v}) catch unreachable;
}

fn Args(comptime fmt: []const u8) type {
    var types: []const type = &.{};
    for (fmt, 0..) |c, i| {
        if (c != '%') continue;
        if (fmt[i + 1] == 'd') types = types ++ [_]type{i32};
        if (fmt[i + 1] == 's') types = types ++ [_]type{[]const u8};
    }
    return @Tuple(types);
}

fn cfmt(alloc: Allocator, comptime fmt: []const u8, args: Args(fmt)) ![]u8 {
    var out: ArrayList(u8) = .empty;

    comptime var k = 0;
    comptime var i = 0;
    inline while (i < fmt.len) : (i += 1) {
        if (fmt[i] != '%') {
            try out.append(alloc, fmt[i]);
            continue;
        }

        i += 1;

        if (fmt[i] == 'd') {
            var buf: [16]u8 = undefined; // "-2147483648"
            const s = intToStr(&buf, args[k]);
            try out.appendSlice(alloc, s);
        }

        if (fmt[i] == 's') {
            try out.appendSlice(alloc, args[k]);
        }

        k += 1;
    }

    return out.toOwnedSlice(alloc);
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    const e = try cfmt(alloc, "Spin of %s equals %d/2", .{ "electron", 1 });
    defer alloc.free(e);
    std.debug.print("{s}\n", .{e});

    const d = try cfmt(alloc, "Spin of %s equals %d/2", .{ "delta baryon", 3 });
    defer alloc.free(d);
    std.debug.print("{s}\n", .{d});

    // _ = try cfmt(alloc, "Spin of %s equals %d/2", .{ 1, "proton" });   // compiler error
    // _ = try cfmt(alloc, "Spin of %s equals %d/2", .{"photon"});        // compiler error
}
