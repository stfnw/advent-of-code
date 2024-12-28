// https://adventofcode.com/2023/day/2

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Cube = struct {
    red: u32,
    green: u32,
    blue: u32,
};

pub fn run(allocator: Allocator) !void {
    var file = try std.fs.cwd().openFile("../../advent-of-code-data/aoc2023/Day2.txt", .{});
    defer file.close();

    const stat = try file.stat();

    const buffer = try file.readToEndAlloc(allocator, stat.size);
    defer allocator.free(buffer);

    var lines = std.mem.splitScalar(u8, buffer, '\n');

    var id_sum: u32 = 0;

    while (lines.next()) |line| {
        if (line.len == 0) {
            continue;
        }

        if (try isValidGame(line)) |id| {
            id_sum += id;
        } else {
            // ignore games that aren't valid
        }
    }

    std.debug.print("Day2: {d}\n", .{id_sum});
}

fn isValidGame(line: []const u8) !?u32 {
    var fields = std.mem.splitSequence(u8, line, ": ");
    const game_id = try std.fmt.parseInt(u32, fields.next().?[5..], 10);

    var cubes = std.mem.splitSequence(u8, fields.next().?, "; ");

    var is_valid = true;

    while (cubes.next()) |cube_| {
        var colors = std.mem.splitSequence(u8, cube_, ", ");

        var cube = Cube{ .red = 0, .green = 0, .blue = 0 };

        while (colors.next()) |color| {
            var it = std.mem.splitSequence(u8, color, " ");
            const n = try std.fmt.parseInt(u32, it.next().?, 10);
            const val = it.next().?;

            if (std.mem.eql(u8, val, "red")) {
                cube.red += n;
            } else if (std.mem.eql(u8, val, "green")) {
                cube.green += n;
            } else if (std.mem.eql(u8, val, "blue")) {
                cube.blue += n;
            } else {
                assert(false);
            }
        }

        is_valid = is_valid and isValidCube(cube);

        // std.debug.print("{d}     {any} {}\n", .{ game_id, cube, is_valid });
    }

    if (is_valid) {
        return game_id;
    } else {
        return null;
    }
}

fn isValidCube(cube: Cube) bool {
    return cube.red <= 12 and cube.green <= 13 and cube.blue <= 14;
}
