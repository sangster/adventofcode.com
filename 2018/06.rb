#!/bin/env ruby
require 'colorized_string'

$lines = IO.read(ARGV[0]).lines.map(&:strip)

class Part1And2
  def call_1
    c = $lines.map(&method(:parse))
    b = bounds(c)

    counts = Hash.new { |h, k| h[k] = 0 }
    all_coords(b).each_with_index.map do |(x, y), i|
      cl = closest(c, x, y)
      counts[cl] += 1 if cl
    end

    inf = infinite_indicies(c, b)
    counts.entries.reject { |(idx, _)| inf.include?(idx) }.map(&:last).max
  end

  def call_2
    c = $lines.map(&method(:parse))
    b = bounds(c)

    all_coords(b)
      .map { |x, y| dist_set(c, x, y).sum }
      .select { |n| n < 10_000 }
      .size
  end

  private

  def bounds(coords)
    { x1: coords.map(&:first).min, x2: coords.map(&:first).max,
      y1: coords.map(&:last).min,  y2: coords.map(&:last).max }
  end

  def colors_pairs
    @colors_pairs ||=
      ColorizedString.colors[1..-2].flat_map do |a|
        ([:default, :bold].map { |b| [a, b] } )
      end
  end

  def colorize(num, str)
    color, mode = colors_pairs[num % colors_pairs.size]
    ColorizedString[str]
      .colorize(color)
      .yield_self { |s| mode == :bold ? s.bold : s }
  end

  # Useage: ./06.rb input.txt | less -S
  def print_ascii_map
    coords = $lines.map(&method(:parse))
    b = bounds(coords)

    (b[:x1]..b[:x2]).each do |x|
      (b[:y1]..b[:y2]).each do |y|
        c = closest(coords, x, y)
        if c
          print colorize(c, format('%2d ', c))
        else
          print "   "
        end
      end
      print "\n"
    end
  end

  def closest(coords, x, y)
    dists = dist_set(coords, x, y)
    min_dist = dists.first
    min_idx  = []

    dists.each_with_index do |dist, i|
      if dist < min_dist
        min_dist = dist
        min_idx = [i]
      elsif dist == min_dist
        min_idx << i
      end
    end

    min_idx.first if min_idx.size == 1
  end

  def all_coords(b)
    Enumerator.new do |yielder|
      (b[:x1]..b[:x2]).each do |x|
        (b[:y1]..b[:y2]).each { |y| yielder.yield([x, y]) }
      end
    end
  end

  def parse(str)
    str.split(', ').map(&:to_i)
  end

  def dist_set(coords, x, y)
    coords.map { |c| dist(c[0], c[1], x, y) }
  end

  def dist(x1, y1, x2, y2)
    (x2 - x1).abs + (y2 - y1).abs
  end

  def infinite_indicies(coords, bounds)
    coords.each_index.select { |i| infinite?(coords[i], bounds) }
  end

  def infinite?(coord, bounds)
    x, y = coord
    x == bounds[:x1] || x == bounds[:x2] || y == bounds[:y1] || y == bounds[:y2]
  end
end

# Part1And2.new.print_ascii_map

puts format("1: %p\n2: %p",
            Part1And2.new.call_1,
            Part1And2.new.call_2)
