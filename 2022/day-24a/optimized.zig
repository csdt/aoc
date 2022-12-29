const std = @import("std");
const assert = std.debug.assert;
const stdout = std.io.getStdOut().writer();

fn funnel_shift_l(comptime T: type, high: T, low: T, n: std.math.Log2Int(T)) T {
    if (n == 0) {
        return high;
    }
    return (high << n) | (low >> @truncate(std.math.Log2Int(T), @bitSizeOf(T) - @as(usize, n)));
}
fn funnel_shift_r(comptime T: type, high: T, low: T, n: std.math.Log2Int(T)) T {
    if (n == 0) {
        return low;
    }
    return (low >> n) | (high << @truncate(std.math.Log2Int(T), @bitSizeOf(T) - @as(usize, n)));
}

const Simulator = struct {
    height: usize,
    width: usize,
    pitch: usize,
    north_data: []const u64,
    south_data: []const u64,
    east_data: []const u64,
    west_data: []const u64,
    player_data: []u64,
    iter: usize,

    noinline fn init(allocator: std.mem.Allocator, height: usize, width: usize, pitch: usize, north: []const u64, south: []const u64, east: []const u64, west: []const u64) std.mem.Allocator.Error!Simulator {
        assert(height > 0);
        assert(width > 0);
        assert(pitch * 64 >= width);
        assert(north.len >= height * pitch);
        assert(south.len >= height * pitch);
        assert(east.len >= height * pitch);
        assert(west.len >= height * pitch);
        // east and west rows are doubled to make it easier to wrap around
        // east and west maps are defined 8 times to make it faster to use bit offsets
        // player map requires a double buffer and a border for the dilation
        var east_data = try allocator.alloc(u64, 16*height*pitch);
        errdefer allocator.free(east_data);
        var west_data = try allocator.alloc(u64, 16*height*pitch);
        errdefer allocator.free(west_data);
        var player_data = try allocator.alloc(u64, 2*(height + 2)*(pitch+1));
        errdefer allocator.free(player_data);

        const size = pitch*height;
        const p = (width + 63) / 64; // in case pitch is larger than strictly necessary
        const bit_padding = @intCast(u6, 64 * p - width);
        var offset: usize = undefined;

        // fill east
        offset = 0;
        for (range(height)) |_| {
            const src = east.ptr + offset;
            const dst = blk: {
                var array : [8][*]u64 = undefined;
                inline for (crange(8)) |s| {
                    array[s] = east_data.ptr + (2*s*size + 2*offset);
                }
                break :blk array;
            };
            offset += pitch;


            // copy aligned
            std.mem.copy(u64, dst[0][pitch..(pitch + p)], src[0..p]);

            // bit shift
            var prev = src[p - 1] << bit_padding;
            for (range(p)) |_, j| {
                const cur = src[j];
                inline for (crange(8)[1..8]) |s| {
                    dst[s][pitch + j] = funnel_shift_l(u64, cur, prev, s);
                }
                prev = cur;
            }

            // duplicate row
            for (range(8)) |_, s| {
                prev = dst[s][pitch + p - 1];
                for (range(p)) |_, j| {
                    const cur = dst[s][pitch + j];
                    dst[s][pitch - p + j] = funnel_shift_l(u64, cur, prev, bit_padding);
                    prev = cur;
                }
            }
        }

        // fill west
        offset = 0;
        for (range(height)) |_| {
            const src = west.ptr + offset;
            const dst = blk: {
                var array : [8][*]u64 = undefined;
                inline for (crange(8)) |s| {
                    array[s] = west_data.ptr + (2*s*size + 2*offset);
                }
                break :blk array;
            };
            offset += pitch;

            // copy aligned
            std.mem.copy(u64, dst[0][0..p], src[0..p]);
            const start = (width + 1) / 64;
            const end = (2 * width + 63) / 64;

            // duplicate row
            var prev = src[p-1] << bit_padding;
            for (range(2*p - start)) |_, j| {
                const cur = dst[0][j];
                dst[0][start + j] = funnel_shift_r(u64, cur, prev, bit_padding);
                prev = cur;
            }

            // bit shifts
            var cur = dst[0][0];
            for (range(end)) |_, j| {
                const next = dst[0][j+1];
                inline for (crange(8)[1..8]) |s| {
                    dst[s][j] = funnel_shift_r(u64, next, cur, s);
                }
                cur = next;
            }
        }

        return Simulator{
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

    fn deinit(self: *Simulator, allocator: std.mem.Allocator) void {
        allocator.free(self.east_data);
        allocator.free(self.west_data);
        allocator.free(self.player_data);
    }

    fn getEastPtr(self: Simulator, iter: usize) [*]align(1) const u64 {
        const n = iter % self.width;
        const bit_offset = n % 8;
        const byte_offset = n / 8;
        var ptr = @ptrToInt(self.east_data.ptr);
        ptr += self.pitch * @sizeOf(u64) - byte_offset;
        ptr += bit_offset * self.height * self.pitch * 2 * @sizeOf(u64);
        return @intToPtr([*]align(1) const u64, ptr);
    }

    fn getWestPtr(self: Simulator, iter: usize) [*]align(1) const u64 {
        const p = (self.width + 63) / 64; // in case pitch is larger than strictly necessary
        const n = iter % self.width;
        const bit_offset = n % 8;
        const byte_offset = n / 8;
        var ptr = @ptrToInt(self.west_data.ptr);
        ptr += byte_offset + (self.pitch - p) * @sizeOf(u64);
        ptr += bit_offset * self.height * self.pitch * 2 * @sizeOf(u64);
        return @intToPtr([*]align(1) const u64, ptr);
    }

    fn getPlayerPtr(self: Simulator, iter: usize) [*]u64 {
        var ptr = self.player_data.ptr + self.pitch + 1;
        if (iter % 2 == 0) {
            ptr += (self.height + 2) * (self.pitch + 1);
        }
        return ptr;
    }

    // Trampoline helps the compiler to see that Simulator is not changed during the processing
    fn step(self: *Simulator) void {
        self.iter += 1;
        self.stepIter();
    }
    noinline fn stepIter(self: Simulator) void {
        const size = self.height * self.pitch;
        const north_start = self.north_data.ptr;
        const south_start = self.south_data.ptr;
        const north_end = north_start + size;
        const south_end = south_start + size;
        const p = (self.width + 63) / 64;
        var north = north_start + (self.iter % self.height) * self.pitch;
        var south = south_start + (self.height - (self.iter % self.height)) * self.pitch;
        if (south == south_end) {
            south = south_start;
        }

        var east = self.getEastPtr(self.iter);
        var west = self.getWestPtr(self.iter);
        var src = self.getPlayerPtr(self.iter - 1);
        var dst = self.getPlayerPtr(self.iter);

        for (range(self.height)) |_| {
            const above = src - (self.pitch + 1);
            const below = src + (self.pitch + 1);
            var left : u64 = 0;
            var center = src[0];
            for (range(p)) |_, j| {
                const right = src[j+1];
                const up = above[j];
                const down = below[j];

                const left1 = funnel_shift_l(u64, center, left, 1);
                const right1 = funnel_shift_r(u64, right, center, 1);

                dst[j] = (left1 | right1 | up | down | center) & ~(north[j] | south[j] | east[j] | west[j]);
                left = center;
                center = right;
            }

            north += self.pitch;
            if (north == north_end) {
                north = north_start;
            }
            south += self.pitch;
            if (south == south_end) {
                south = south_start;
            }
            east += 2*self.pitch;
            west += 2*self.pitch;
            src += self.pitch + 1;
            dst += self.pitch + 1;
        }
    }
};


pub fn process(allocator: std.mem.Allocator, input: []const u8) !void {
    var timer = try std.time.Timer.start();
    var height : usize = 0;

    var lines = std.mem.tokenize(u8, input, "\n");
    const width = lines.next().?.len - 2;

    const pitch = (width + 63) / 64; // divCeil
    const offwall_mask = std.math.boolMask(u64, true) << @intCast(u6, width % 64);

    var map_north = try std.ArrayList(u64).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_south = try std.ArrayList(u64).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_east  = try std.ArrayList(u64).initCapacity(allocator, (input.len / (width + 2)) * pitch);
    var map_west  = try std.ArrayList(u64).initCapacity(allocator, (input.len / (width + 2)) * pitch);
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
            const I: u64 = x / 64;
            const i: u6 = @intCast(u6, x % 64);

            row_north[I] |= @as(u64, @boolToInt(c == '^')) << i;
            row_south[I] |= @as(u64, @boolToInt(c == 'v')) << i;
            row_east[I] |= @as(u64, @boolToInt(c == '>')) << i;
            row_west[I] |= @as(u64, @boolToInt(c == '<')) << i;
        }
        // East of the wall, let's just assume there is an infinite blizzard constantly blowing north and south
        // This avoid the need for extra masking during the simulation step
        row_north[pitch-1] |= offwall_mask;
        row_south[pitch-1] |= offwall_mask;
    }
    const parse_time = timer.lap();

    var sim = try Simulator.init(allocator, height, width, pitch, map_north.items, map_south.items, map_east.items, map_west.items);
    defer sim.deinit(allocator);

    const preprocess_time = timer.lap();

    const J = (width - 1) / 64;
    const j = @intCast(u6, (width - 1) % 64);
    { // First travel
        std.mem.set(u64, sim.player_data, 0);
        (sim.getPlayerPtr(sim.iter) - (pitch + 1))[0] = 1;
        while (true) {
            //try std.fmt.format(stdout, "processing iter {}\n", .{sim.iter});
            sim.step();

            if ((sim.getPlayerPtr(sim.iter)[(height - 1) * (pitch + 1) + J] >> j) & 1 == 1) {
                sim.iter += 1;
                break;
            }
        }
    }
    const first_travel = sim.iter;

    { // Second travel Back
        std.mem.set(u64, sim.player_data, 0);
        sim.getPlayerPtr(sim.iter)[height * (pitch + 1) + J] = @as(u64, 1) << j;
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
        std.mem.set(u64, sim.player_data, 0);
        (sim.getPlayerPtr(sim.iter) - (pitch + 1))[0] = 1;
        while (true) {
            sim.step();

            if ((sim.getPlayerPtr(sim.iter)[(height - 1) * (pitch + 1) + J] >> j) & 1 == 1) {
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
