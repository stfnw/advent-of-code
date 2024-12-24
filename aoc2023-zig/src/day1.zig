// https://adventofcode.com/2023/day/1

const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn run(allocator: Allocator) !void {
    var file = try std.fs.cwd().openFile("../../advent-of-code-data/aoc2023/Day1.txt", .{});
    defer file.close();

    const stat = try file.stat();

    const buffer = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(buffer);

    var lines = std.mem.splitScalar(u8, buffer, '\n');
    var sum: u32 = 0;

    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        var firsti: usize = 0;
        while (firsti < line.len) : (firsti += 1) {
            if (std.ascii.isDigit(line[firsti])) {
                break;
            }
        }

        var lasti: usize = line.len - 1;
        while (lasti >= 0) : (lasti -= 1) {
            if (std.ascii.isDigit(line[lasti])) {
                break;
            }
        }

        const first = line[firsti] - '0';
        const last = line[lasti] - '0';

        sum += first * 10 + last;
    }

    std.debug.print("Day1: {d}\n", .{sum});
}
