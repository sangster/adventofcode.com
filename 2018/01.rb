#!/bin/env ruby

$nums = IO.read(ARGV[0]).lines.map(&:to_i)

def part1
  $nums.reduce(:+)
end

def part2
  counts = Hash.new { |h, k| h[k] = 0 }

  # Initial state
  frequency = 0
  counts[0] = 1

  loop do
    frequency =
      $nums.reduce(frequency) do |frequency, num|
        frequency += num
        counts[frequency] += 1

        return frequency if counts[frequency] == 2

        frequency
      end
  end
end

puts format("1: %d\n2: %d", part1, part2)
