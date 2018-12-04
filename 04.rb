#!/bin/env ruby

$lines = IO.read(ARGV[0]).lines.map(&:strip).sort

class Part1And2
  def initialize
    @minutes = Hash.new do |h, k|
      h[k] = Hash.new { |h2, k2| h2[k2] = 0 }
    end
  end

  def call_most_minutes
    accum_minutes!

    max_guard, mins =
      @minutes.entries.max_by { |(guard, mins)| mins.values.inject(:+) }
    max_minute = mins.entries.max_by { |(min, count)| count }.first

    max_guard.to_i * max_minute
  end

  def call_most_frequency
    accum_minutes!

    max_guard, mins = @minutes.max_by { |(guard, mins)| mins.values.max }
    max_minute = mins.max_by { |(_, count)| count }.first

    max_guard.to_i * max_minute
  end

  def parse(str)
    m = /^[^\s]+\s(\d+):(\d+).\s([^\s]+\s[^\s]+)/.match(str)

    res = { min: m[2].to_i, action: m[3][0] }
    res[:guard] = m[3][7..-1] if res[:action] == 'G'
    res
  end

  def accum_minutes!
    guard = nil
    prev = nil

    $lines.each do |str|
      record = parse(str)

      case record[:action]
      when 'G' # start
        guard = record[:guard]
      when 'w' # wake
        mins = prev[:min] ... record[:min]
        mins.each { |min| @minutes[guard][min] += 1 }
      end

      prev = record
    end
  end
end

# puts format("1: %s\n2: %p", Part1.new.call, part2)
puts format("1: %s\n2: %p",
            Part1And2.new.call_most_minutes,
            Part1And2.new.call_most_frequency)
