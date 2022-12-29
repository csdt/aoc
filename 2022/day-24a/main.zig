const std = @import("std");
const assert = std.debug.assert;
const expect = std.testing.expect;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();

const Dir = enum(u2) {
    north = 0,
    east = 1,
    south = 2,
    west = 3,

    const Vecs = [_]@Vector(2, usize){
        .{ -%@as(usize, 1), 0 }, // north
        .{ 0, 1 }, // east
        .{ 1, 0 }, // south
        .{ 0, -%@as(usize, 1) }, // west
    };

    pub fn parseByte(c: u8) Dir {
        return switch (c) {
            '^', 'n', 'N', 'u', 'U' => Dir.north,
            '>', 'e', 'E', 'r', 'R' => Dir.east,
            'v', 's', 'S', 'd', 'D' => Dir.south,
            '<', 'w', 'W', 'l', 'L' => Dir.west,
            else => {
                std.fmt.format(stderr, "Invalid byte for direction: '{s}' (0x{x:0>2})\n", .{&[1]u8{c}, c}) catch {};
                unreachable;
            },
        };
    }

    pub fn reverse(dir: Dir) Dir {
        return switch (dir) {
            .north => .south,
            .east => .west,
            .south => .north,
            .west => .east,
        };
    }

    pub fn formatByte(self: Dir) u8 {
        return "^>v<"[@enumToInt(self)];
    }
    pub fn format(self: Dir, comptime fmts: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = fmts;
        _ = options;
        try writer.writeByte(self.formatByte());
    }
};

const Cell = union(enum) {
    blizzard : u4,
    wall,
    player,

    const Wall = Cell{ .wall = {} };
    const Player = Cell{ .player = {} };
    const Empty = Cell{ .blizzard = 0 };

    fn parseByte(c: u8) Cell {
        return switch (c) {
            '.' => Cell{ .blizzard = 0 },
            '#' => Cell{ .wall = {} },
            'P', '&' => Cell{ .player = {} },
            else => blizzardFromDir(Dir.parseByte(c)),
        };
    }
    fn blizzardFromDir(dir: Dir) Cell {
        return Cell{ .blizzard = @as(u4, 1) << @enumToInt(dir) };
    }

    pub fn formatByte(self: Cell) u8 {
        return switch (self) {
            .blizzard => |dir_set| switch (@popCount(u4, dir_set)) {
                0 => '.',
                1 => @intToEnum(Dir, @intCast(u2, @ctz(u4, dir_set))).formatByte(),
                else => |n| '0' + @as(u8, n),
            },
            .wall => '#',
            .player => '+',
        };
    }
    pub fn format(self: Cell, comptime fmts: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = fmts;
        _ = options;
        try writer.writeByte(self.formatByte());
    }

    fn printSlice(cells: []const Cell, writer: anytype) @TypeOf(writer).Error!void {
        for (cells) |cell| {
            try writer.writeByte(cell.formatByte());
        }
    }
    pub fn formatSlice(cells: []const Cell, comptime fmts: []const u8, options: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        _ = fmts;

        const min_width = if (options.width) |w| w else 0;
        const width = cells.len;
        const padding = if (width < min_width) min_width - width else 0;

        if (padding == 0) {
            return printSlice(cells, writer);
        }

        switch (options.alignment) {
            .Left => {
                try printSlice(cells, writer);
                try writer.writeByteNTimes(options.fill, padding);
            },
            .Center => {
                const left_padding = padding / 2;
                const right_padding = (padding + 1) / 2;
                try writer.writeByteNTimes(options.fill, left_padding);
                try printSlice(cells, writer);
                try writer.writeByteNTimes(options.fill, right_padding);
            },
            .Right => {
                try writer.writeByteNTimes(options.fill, padding);
                try printSlice(cells, writer);
            },
        }
    }

    pub fn sliceFormatter(cells: []const Cell) std.fmt.Formatter(formatSlice) {
        return .{ .data = cells };
    }
};

pub fn addVecInBox(a: @Vector(2, usize), b: @Vector(2, usize), box: @Vector(2, usize)) @Vector(2, usize) {
    const sa = @bitCast(@Vector(2, isize), a);
    const sb = @bitCast(@Vector(2, isize), b);
    const sbox = @bitCast(@Vector(2, isize), box);
    var sum = sa + sb;
    sum = @select(isize, sum >= @splat(2, @as(isize, 0)), sum, sum + sbox);
    sum = @select(isize, sum < sbox, sum, sum - sbox);
    return @bitCast(@Vector(2, usize), sum);
}


const Simulator = struct {
    data: [2][]Cell,
    width: usize,
    height: usize,
    iter: usize = 0,

    fn print(sim: Simulator, out: anytype) !void {
        var data = sim.data[sim.iter&1];
        var off: usize = 0;
        while (off < data.len) : (off += sim.width) {
            try std.fmt.format(out, "{}\n", .{ Cell.sliceFormatter(data[off..(off+sim.width)]) });
        }
    }

    fn step(sim: *Simulator) void {
        const w = sim.width;
        const h = sim.height;
        const src = sim.data[sim.iter&1];
        sim.iter += 1;
        const dst = sim.data[sim.iter&1];

        // Fill with the map shape
        for (range(h)) |_, i| {
            for (range(w)) |_, j| {
                dst[i*w+j] = switch (src[i*w+j]) {
                    .wall => Cell.Wall,
                    else => Cell.Empty,
                };
            }
        }

        const box = @Vector(2, usize){h, w};

        // Compute next time step
        for (range(h)) |_, i| {
            for (range(w)) |_, j| {
                const cell = src[i*w+j];
                switch (cell) {
                    .blizzard => |dirs| for (Dir.Vecs) |vec, dir_value| {
                        const dir_mask = @as(u4, 1) << @intCast(u2, dir_value);
                        if (dirs & dir_mask == 0) {
                            continue;
                        }

                        var pos = @Vector(2, usize){i, j};
                        while (true) {
                            pos = addVecInBox(pos, vec, box);
                            switch (dst[pos[0] * w + pos[1]]) {
                                .wall => {},
                                else => break,
                            }
                        }
                        const dst_cell = &dst[pos[0] * w + pos[1]];
                        dst_cell.* = switch (dst_cell.*) {
                            .blizzard => |dir_set| Cell{ .blizzard = dir_set | dir_mask},
                            .wall => unreachable,
                            .player => Cell{ .blizzard = dir_mask },
                        };

                    },
                    .player => for (Dir.Vecs ++ [1]@Vector(2, usize){.{0, 0}}) |vec| {
                        const pos = @Vector(2, usize){i, j} +% vec;
                        if (@reduce(.Or, pos >= box)) {
                            continue;
                        }

                        const dst_cell = &dst[pos[0] * w + pos[1]];
                        switch (dst_cell.*) {
                            .blizzard => |dir_set| if (dir_set == 0) {
                                dst_cell.* = Cell.Player;
                            },
                            else => {},
                        }
                    },
                    else => {},
                } // switch (cell)
            } // for (range(w))
        } // for (range(h))
    } // fn step(...)
};


pub fn process(allocator: std.mem.Allocator, input: []const u8) !void {
    var map1 = std.ArrayList(Cell).init(allocator);
    defer map1.deinit();
    var width : usize = 0;
    var height : usize = 0;

    var lines = std.mem.tokenize(u8, input, "\n");
    while (lines.next()) |line| {
        width = @maximum(width, line.len);
        
        try map1.ensureUnusedCapacity(line.len);
        for (line) |c| {
            map1.appendAssumeCapacity(Cell.parseByte(if (height == 0 and c == '.') '&' else c));
        }
        height += 1;
    }

    var map2 = try allocator.alloc(Cell, map1.items.len);
    defer allocator.free(map2);

    var sim = Simulator{ .data = .{ map1.items, map2 }, .width = width, .height = height };

    // First trip
    while (true) {
        sim.step();

        switch (sim.data[sim.iter&1][(height - 1) * width + width - 2]) {
            .player => break,
            else => {},
        }
    }

    try std.fmt.format(stdout, "Steps: {}\n", .{sim.iter});

    // Clear the multiverse
    for (sim.data[sim.iter&1]) |*cell| {
        switch (cell.*) {
            .player => cell.* = Cell.Empty,
            else => {},
        }
    }
    sim.data[sim.iter&1][(height - 1) * width + width - 2] = Cell.Player;

    // Second trip
    while (true) {
        sim.step();

        switch (sim.data[sim.iter&1][1]) {
            .player => break,
            else => {},
        }
    }

    try std.fmt.format(stdout, "Steps: {}\n", .{sim.iter});

    // Clear the multiverse
    for (sim.data[sim.iter&1]) |*cell| {
        switch (cell.*) {
            .player => cell.* = Cell.Empty,
            else => {},
        }
    }
    sim.data[sim.iter&1][1] = Cell.Player;

    // Last trip
    while (true) {
        sim.step();

        switch (sim.data[sim.iter&1][(height - 1) * width + width - 2]) {
            .player => break,
            else => {},
        }
    }
    try std.fmt.format(stdout, "Steps: {}\n", .{sim.iter});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var allocator = arena.allocator();
    var arg_it = try std.process.argsWithAllocator(allocator);
    defer arg_it.deinit();

    // skip my own exe name
    _ = arg_it.skip();

    var inputFilename = arg_it.next(allocator) orelse "-" catch "-";
    if (std.mem.eql(u8, inputFilename, "-")) {
        inputFilename = "/dev/stdin";
    }
    var input = try std.fs.cwd().readFileAlloc(allocator, inputFilename, std.math.maxInt(usize));

    var timer = try std.time.Timer.start();
    try process(allocator, input);
    var duration = timer.read();
    try std.fmt.format(stdout, "elapsed time: {}\n", .{std.fmt.fmtDuration(duration)});

    _ = input;
    defer allocator.free(input);
}

fn iterInto(iter: anytype, output: anytype) usize {
    var n: usize = 0;
    for (output) |*out| {
        if (iter.next()) |element| {
            out.* = element;
        } else {
            return n;
        }
        n += 1;
    }

    return n;
}

fn iterEat(iter: anytype) usize {
    var n: usize = 0;

    while (iter.next()) |_| {
        n += 1;
    }

    return n;
}

fn iterIntoExact(iter: anytype, output: anytype) !void {
    var n = iterInto(iter, output);
    if (n < output.len) {
        return error.tooFewElements;
    }
    if (iter.next()) |_| {
        return error.tooManyElements;
    }
}

fn range(n: usize) []const void {
    return @as([*]const void, undefined)[0..n];
}

fn gcd(comptime T: type, x: T, y: T) T {
    if (x == 0) {
        return y;
    }

    return gcd(T, @mod(y, x), x);
}

fn lcm(comptime T: type, x: T, y: T) T {
    return x * y / gcd(T, x, y);
}

fn removePrefix(input: []const u8, comptime prefix: []const u8) []const u8 {
    if (input.len < prefix.len or !std.mem.eql(u8, input[0..prefix.len], prefix)) {
        std.fmt.format(stderr,
            \\ Error: input does not start with prefix:
            \\    input: "{s}"
            \\   prefix: "{s}"
        , .{ input, prefix }) catch void{};
        unreachable;
    }
    return input[prefix.len..input.len];
}

fn Vec2D(comptime T: type) type {
    return struct {
        const Self = @This();
        i: T,
        j: T,

        fn init(i: T, j: T) Self {
            return .{ .i = i, .j = j };
        }
        fn set(v: T) Self {
            return .{ .i = v, .j = v };
        }

        fn add(lhs: Self, rhs: Self) Self {
            return Self.init(lhs.i + rhs.i, lhs.j + rhs.j);
        }
        fn sub(lhs: Self, rhs: Self) Self {
            return Self.init(lhs.i - rhs.i, lhs.j - rhs.j);
        }
        fn min(lhs: Self, rhs: Self) Self {
            return Self.init(@minimum(lhs.i, rhs.i), @minimum(lhs.j, rhs.j));
        }
        fn max(lhs: Self, rhs: Self) Self {
            return Self.init(@maximum(lhs.i, rhs.i), @maximum(lhs.j, rhs.j));
        }
        fn eq(lhs: Self, rhs: Self) bool {
            return lhs.i == rhs.i and lhs.j == rhs.j;
        }

        fn as(self: Self, comptime U: type) Vec2D(U) {
            return .{ .i = @as(U, self.i), .j = @as(U, self.j) };
        }
        fn cast(self: Self, comptime U: type) Vec2D(U) {
            return .{ .i = @intCast(U, self.i), .j = @intCast(U, self.j) };
        }

        fn l1(self: Self) T {
            var i = std.math.absInt(self.i) catch unreachable;
            var j = std.math.absInt(self.j) catch unreachable;
            return i + j;
        }
    };
}
fn Grid(comptime T: type) type {
    return struct {
        const Self = @This();
        rows: []const []T,

        fn init(allocator: std.mem.Allocator, n: usize) !Self {
            const data = try allocator.alloc(T, n * n);
            errdefer allocator.free(data);
            const grid = try allocator.alloc([]T, n);
            errdefer allocator.free(grid);

            for (grid) |*row, i| {
                row.* = data[(i * n)..(i * n + n)];
            }

            return Self{ .rows = grid };
        }

        fn deinit(self: Self, allocator: std.mem.Allocator) void {
            var n = self.rows.len;
            var data = @ptrCast([*]T, self.rows[0])[0..(n * n)];
            allocator.free(data);
            allocator.free(self.rows);
        }

        fn print(self: Self, out: anytype) !void {
            for (self.rows) |row| {
                try std.fmt.format(out, "{s}\n", .{row});
            }
        }

        fn at(self: Self, pos: anytype) ?*T {
            if (pos.i < 0 or pos.i >= self.rows.len) {
                return null;
            }
            var row = self.rows[@intCast(usize, pos.i)];
            if (pos.j < 0 or pos.j >= row.len) {
                return null;
            }
            return &row[@intCast(usize, pos.j)];
        }

        fn fill(self: Self, val: T) void {
            for (self.rows) |row| {
                for (row) |*cell| {
                    cell.* = val;
                }
            }
        }
    };
}
