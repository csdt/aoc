const std = @import("std");
const assert = std.debug.assert;
const stdout = std.io.getStdOut().writer();

fn funnel_shift_l(comptime T: type, high: T, low: T, n: std.math.Log2Int(T)) T {
    if (n == 0) {
        return high;
    }
    return (high << n) | (low >> @intCast(std.math.Log2Int(T), @bitSizeOf(T) - @as(usize, n)));
}
fn funnel_shift_r(comptime T: type, high: T, low: T, n: std.math.Log2Int(T)) T {
    if (n == 0) {
        return low;
    }
    return (low >> n) | (high << @intCast(std.math.Log2Int(T), @bitSizeOf(T) - @as(usize, n)));
}

fn Simulator(comptime T: type) type {
    return struct {
        const Self = @This();
        const Int = T;
        const ShiftInt = std.math.Log2Int(Int);
        const Nbits = @bitSizeOf(Int);
        const Nbytes = @sizeOf(Int);
        const BitsPerByte = Nbits / Nbytes;
        comptime {
            assert(Nbits % Nbytes == 0);
        }

        height: usize,
        width: usize,
        pitch: usize,
        north_data: []const Int,
        south_data: []const Int,
        east_data: []const Int,
        west_data: []const Int,
        player_data: []Int,
        iter: usize,

        noinline fn init(allocator: std.mem.Allocator, height: usize, width: usize, pitch: usize, north: []const Int, south: []const Int, east: []const Int, west: []const Int) std.mem.Allocator.Error!Self {
            assert(height > 0);
            assert(width > 0);
            assert(pitch * Nbits >= width);
            assert(north.len >= height * pitch);
            assert(south.len >= height * pitch);
            assert(east.len >= height * pitch);
            assert(west.len >= height * pitch);
            // east and west rows are doubled to make it easier to wrap around
            // east and west maps are defined 8 times to make it faster to use bit offsets
            // player map requires a double buffer and a border for the dilation
            var east_data = try allocator.alloc(Int, 2*BitsPerByte*height*pitch);
            errdefer allocator.free(east_data);
            var west_data = try allocator.alloc(Int, 2*BitsPerByte*height*pitch);
            errdefer allocator.free(west_data);
            var player_data = try allocator.alloc(Int, 2*(height + 2)*pitch);
            errdefer allocator.free(player_data);

            const size = pitch*height;
            const p = (width + Nbits - 1) / Nbits; // in case pitch is larger than strictly necessary
            const bit_padding = @intCast(ShiftInt, Nbits * p - width);
            var offset: usize = undefined;

            // fill east
            offset = 0;
            for (range(height)) |_| {
                const src = east.ptr + offset;
                const dst = blk: {
                    var array : [BitsPerByte][*]Int = undefined;
                    inline for (crange(BitsPerByte)) |s| {
                        array[s] = east_data.ptr + (2*s*size + 2*offset);
                    }
                    break :blk array;
                };
                offset += pitch;


                // copy aligned
                std.mem.copy(Int, dst[0][pitch..(pitch + p)], src[0..p]);

                // bit shift
                var prev = src[p - 1] << bit_padding;
                for (range(p)) |_, j| {
                    const cur = src[j];
                    inline for (crange(BitsPerByte)[1..BitsPerByte]) |s| {
                        dst[s][pitch + j] = funnel_shift_l(Int, cur, prev, s);
                    }
                    prev = cur;
                }

                // duplicate row
                for (range(BitsPerByte)) |_, s| {
                    prev = dst[s][pitch + p - 1];
                    for (range(p)) |_, j| {
                        const cur = dst[s][pitch + j];
                        dst[s][pitch - p + j] = funnel_shift_l(Int, cur, prev, bit_padding);
                        prev = cur;
                    }
                }
            }

            // fill west
            offset = 0;
            for (range(height)) |_| {
                const src = west.ptr + offset;
                const dst = blk: {
                    var array : [BitsPerByte][*]Int = undefined;
                    inline for (crange(BitsPerByte)) |s| {
                        array[s] = west_data.ptr + (2*s*size + 2*offset);
                    }
                    break :blk array;
                };
                offset += pitch;

                // copy aligned
                std.mem.copy(Int, dst[0][0..p], src[0..p]);
                const start = (width + 1) / Nbits;
                const end = (2 * width + Nbits - 1) / Nbits;

                // duplicate row
                var prev = src[p-1] << bit_padding;
                for (range(2*p - start)) |_, j| {
                    const cur = dst[0][j];
                    dst[0][start + j] = funnel_shift_r(Int, cur, prev, bit_padding);
                    prev = cur;
                }

                // bit shifts
                var cur = dst[0][0];
                for (range(end)) |_, j| {
                    const next = dst[0][j+1];
                    inline for (crange(BitsPerByte)[1..BitsPerByte]) |s| {
                        dst[s][j] = funnel_shift_r(Int, next, cur, s);
                    }
                    cur = next;
                }
            }

            return Self{
                .height = height,
                .width = width,
                .pitch = pitch,
                .north_data = north,
                .south_data = south,
                .east_data = east_data,
                .west_data = west_data,
                .player_data = player_data,
                .iter = 0,
            };
        }

        fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.east_data);
            allocator.free(self.west_data);
            allocator.free(self.player_data);
        }

        fn getEastPtr(self: Self, iter: usize) [*]align(1) const Int {
            const n = iter % self.width;
            const bit_offset = n % BitsPerByte;
            const byte_offset = n / BitsPerByte;
            var ptr = @ptrToInt(self.east_data.ptr);
            ptr += self.pitch * Nbytes - byte_offset;
            ptr += bit_offset * self.height * self.pitch * 2 * Nbytes;
            return @intToPtr([*]align(1) const Int, ptr);
        }

        fn getWestPtr(self: Self, iter: usize) [*]align(1) const Int {
            const p = (self.width + Nbits - 1) / Nbits; // in case pitch is larger than strictly necessary
            const n = iter % self.width;
            const bit_offset = n % BitsPerByte;
            const byte_offset = n / BitsPerByte;
            var ptr = @ptrToInt(self.west_data.ptr);
            ptr += byte_offset + (self.pitch - p) * Nbytes;
            ptr += bit_offset * self.height * self.pitch * 2 * Nbytes;
            return @intToPtr([*]align(1) const Int, ptr);
        }

        fn getPlayerPtr(self: Self, iter: usize) [*]Int {
            var ptr = self.player_data.ptr + self.pitch;
            if (iter % 2 == 0) {
                ptr += (self.height + 2) * self.pitch;
            }
            return ptr;
        }

        noinline fn step(self: *Self) void {
            self.iter += 1;
            const iter = self.iter;
            const pitch = self.pitch;
            const height = self.height;
            const size = height * pitch;
            const north_start = self.north_data.ptr;
            const south_start = self.south_data.ptr;
            const north_end = north_start + size;
            const south_end = south_start + size;
            var north = north_start + (iter % height) * pitch;
            var south = south_start + (height - (iter % height)) * pitch;
            if (south == south_end) {
                south = south_start;
            }

            var east = self.getEastPtr(iter);
            var west = self.getWestPtr(iter);
            var src = self.getPlayerPtr(iter - 1);
            var dst = self.getPlayerPtr(iter);

            assert(pitch > 0);

            for (range(height)) |_| {
                const above = src - pitch;
                const below = src + pitch;
                var left : Int = 0;
                var center = src[0];
                for (range(pitch - 1)) |_, j| {
                    const right = src[j+1];
                    const up = above[j];
                    const down = below[j];

                    const left1 = funnel_shift_l(Int, center, left, 1);
                    const right1 = funnel_shift_r(Int, right, center, 1);

                    dst[j] = (left1 | right1 | up | down | center) & ~(north[j] | south[j] | east[j] | west[j]);
                    left = center;
                    center = right;
                }
                { // last column
                    const j = pitch - 1;
                    const right = 0;
                    const up = above[j];
                    const down = below[j];

                    const left1 = funnel_shift_l(Int, center, left, 1);
                    const right1 = funnel_shift_r(Int, right, center, 1);

                    dst[j] = (left1 | right1 | up | down | center) & ~(north[j] | south[j] | east[j] | west[j]);
                }

                north += pitch;
                if (north == north_end) {
                    north = north_start;
                }
                south += pitch;
                if (south == south_end) {
                    south = south_start;
                }
                east += 2*pitch;
                west += 2*pitch;
                src += pitch;
                dst += pitch;
            }
        }
    };
}


pub fn process(allocator: std.mem.Allocator, input: []const u8) !void {
    const Sim = Simulator(usize);
    var timer = try std.time.Timer.start();
    var height : usize = 0;

    var lines = std.mem.tokenize(u8, input, "\n");
    const width = lines.next().?.len - 2;

    const pitch = (width + Sim.Nbits - 1) / Sim.Nbits; // divCeil
    const offwall_mask = std.math.boolMask(Sim.Int, true) << @intCast(Sim.ShiftInt, width % Sim.Nbits);

    var map_north = try std.ArrayList(Sim.Int).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_south = try std.ArrayList(Sim.Int).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_east  = try std.ArrayList(Sim.Int).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_west  = try std.ArrayList(Sim.Int).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    defer {
        map_north.deinit();
        map_south.deinit();
        map_east.deinit();
        map_west.deinit();
    }

    var offset: usize = 0;
    while (lines.next()) |line| {
        if (line[1] == '#') {
            break;
        }

        assert(offset == height*pitch);

        assert(map_north.items.len == height*pitch);
        assert(map_south.items.len == height*pitch);
        assert(map_east.items.len == height*pitch);
        assert(map_west.items.len == height*pitch);

        try map_north.appendNTimes(0, pitch);
        try map_south.appendNTimes(0, pitch);
        try map_east.appendNTimes(0, pitch);
        try map_west.appendNTimes(0, pitch);

        const row_north = map_north.items[offset..map_north.items.len];
        const row_south = map_south.items[offset..map_south.items.len];
        const row_east = map_east.items[offset..map_east.items.len];
        const row_west = map_west.items[offset..map_west.items.len];

        offset += pitch;
        height += 1;
        
        for (line[1..(line.len - 1)]) |c, x| {
            const I: usize = x / Sim.Nbits;
            const i = @intCast(Sim.ShiftInt, x % Sim.Nbits);

            row_north[I] |= @as(Sim.Int, @boolToInt(c == '^')) << i;
            row_south[I] |= @as(Sim.Int, @boolToInt(c == 'v')) << i;
            row_east[I]  |= @as(Sim.Int, @boolToInt(c == '>')) << i;
            row_west[I]  |= @as(Sim.Int, @boolToInt(c == '<')) << i;
        }
        // East of the wall, let's just assume there is an infinite blizzard constantly blowing north and south
        // This avoid the need for extra masking during the simulation step
        row_north[pitch-1] |= offwall_mask;
        row_south[pitch-1] |= offwall_mask;
    }
    const parse_time = timer.lap();

    var sim = try Sim.init(allocator, height, width, pitch, map_north.items, map_south.items, map_east.items, map_west.items);
    defer sim.deinit(allocator);

    const preprocess_time = timer.lap();

    const J = (width - 1) / Sim.Nbits;
    const j = @intCast(Sim.ShiftInt, (width - 1) % Sim.Nbits);
    { // First travel
        std.mem.set(Sim.Int, sim.player_data, 0);
        (sim.getPlayerPtr(sim.iter) - pitch)[0] = 1;
        while (true) {
            //try std.fmt.format(stdout, "processing iter {}\n", .{sim.iter});
            sim.step();

            if ((sim.getPlayerPtr(sim.iter)[(height - 1) * pitch + J] >> j) & 1 == 1) {
                sim.iter += 1;
                break;
            }
        }
    }
    const first_travel = sim.iter;

    { // Second travel Back
        std.mem.set(Sim.Int, sim.player_data, 0);
        sim.getPlayerPtr(sim.iter)[height * pitch + J] = @as(Sim.Int, 1) << j;
        while (true) {
            sim.step();

            if (sim.getPlayerPtr(sim.iter)[0] & 1 == 1) {
                sim.iter += 1;
                break;
            }
        }
    }
    const second_travel = sim.iter;

    { // Third travel (again)
        std.mem.set(Sim.Int, sim.player_data, 0);
        (sim.getPlayerPtr(sim.iter) - pitch)[0] = 1;
        while (true) {
            sim.step();

            if ((sim.getPlayerPtr(sim.iter)[(height - 1) * pitch + J] >> j) & 1 == 1) {
                sim.iter += 1;
                break;
            }
        }
    }
    const third_travel = sim.iter;

    const process_time = timer.lap();

    try std.fmt.format(stdout,
        \\Result:
        \\  1st travel: {}
        \\  2nd travel: {}
        \\  3rd travel: {}
        \\
        \\Timings:
        \\  parsing: {}
        \\  pre-process: {}
        \\  process: {} ({}/step)
        \\  total: {}
        \\
    , .{
        first_travel,
        second_travel,
        third_travel,
        std.fmt.fmtDuration(parse_time),
        std.fmt.fmtDuration(preprocess_time),
        std.fmt.fmtDuration(process_time),
        std.fmt.fmtDuration(process_time / (third_travel - 3)),
        std.fmt.fmtDuration(parse_time + preprocess_time + process_time),
    });
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
    defer allocator.free(input);
    try process(allocator, input);
}

fn range(n: usize) []const void {
    return @as([*]const void, undefined)[0..n];
}

fn crange(comptime n: comptime_int) [n]comptime_int {
    comptime {
        var array : [n]comptime_int = undefined;
        inline for (array) |*el, i| {
            el.* = i;
        }
        return array;
    }
}
