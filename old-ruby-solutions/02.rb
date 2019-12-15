#!/bin/env ruby

$strings = IO.read(ARGV[0]).lines.map(&:strip)


class Part1
  def twos_threes(str)
    counts = Hash.new { |h, k| h[k] = 0 }
    str.each_char { |ch| counts[ch] += 1 }

    [ counts.values.any? { |c| c == 2 }, counts.values.any? { |c| c == 3 } ]
  end

  def call
    twos = 0
    threes = 0

    $strings.each do |str|
      has_two, has_three = twos_threes(str)
      twos += 1 if has_two
      threes += 1 if has_three
    end

    twos * threes
  end
end

class Part2
  def call
    sorted = $strings.sort
    prev = sorted.first

    sorted[1..-1].each do |str|
      diff_index = nil

      str.chars.each_with_index do |ch, idx|
        if prev[idx] != ch
          if diff_index.nil?
            diff_index = idx
          else # too many
            diff_index = nil
            break
          end
        end
      end

      return str[0 ... diff_index] + str[diff_index + 1 .. -1] if diff_index

      prev = str
    end
    nil
  end
end

printf("1: %d\n2: %s\n", Part1.new.call, Part2.new.call)
