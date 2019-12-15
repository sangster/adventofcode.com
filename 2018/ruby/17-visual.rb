#!/bin/env ruby

require 'pry-byebug'
require 'colorize'
require 'forwardable'
require 'set'


SPRING_LOC = [0, 500]

CLAY  = '#'.light_black.freeze
WATER = '~'.cyan.freeze


Bounds = Struct.new(:xmin, :xmax, :ymin, :ymax) do
  def update(key, value)
    old = send(key)

    is_better =
      case key
      when :xmin, :ymin then old.nil? || value < old
      when :xmax, :ymax then old.nil? || value > old
      end

    send(:"#{key}=", value) if is_better
  end

  def width
    xmax - xmin + 1 + 2 # add column on left/right for spills
  end

  def depth
    ymax - ymin + 1
  end

  def new_grid
    Array.new(depth) { Array.new(width) { false } }
  end

  def include?(y:, x:)
    x >= xmin && x <= xmax && y >= ymin && y <= ymax
  end
end


class Map
  attr_reader :ranges, :bounds, :clay, :water

  def initialize(ranges)
    @ranges = ranges
    @bounds = Bounds.new
    @ranges.each do |range|
      @bounds.update(:xmin, range[:x].first + 1)
      @bounds.update(:xmax, range[:x].last + 1)
      @bounds.update(:ymin, range[:y].first)
      @bounds.update(:ymax, range[:y].last)
    end

    @clay  = bounds.new_grid
    @water = bounds.new_grid

    ranges.each do |range|
      range[:y].each do |y|
        range[:x].each do |x|
          @clay[y - bounds.ymin][x - bounds.xmin + 2] = true
        end
      end
    end
  end

  def translate(x:, y:)
    { x: x - bounds.xmin + 1, y: y - bounds.ymin }
  end

  def clay?(x:, y:)
    !!clay[y][x]
  end

  def inbounds?(x:, y:)
    x < bounds.width && y < bounds.depth
  end
end


class Puzzle
  def initialize(path)
    @file = IO.read(path)
  end

  def call_1
    map = parse(@file)

    drop = map.translate(y: map.bounds.ymin, x: SPRING_LOC[1])
    new_drop(map, y: drop[:y], x: drop[:x])

    # render(map)
    map.water.sum { |row| row.select { |water| water }.size }
  end

  def call_2
  end

  private

  # Drop water deeper down, to the next +y+
  # @return [Boolean] is boxed in
  def new_drop(map, y:, x:)
    # printf("new_drop(map, y: %d, x: %d)\n", y, x)
    return unless map.inbounds?(y: y, x: x)

    map.water[y][x] = true
    # render(map)

    if map.inbounds?(y: y + 1, x: x)
      should_spill =
        if map.clay?(y: y + 1, x: x)
          true
        else
          new_drop(map, y: y + 1, x: x)
        end

      if should_spill
        left  = spill(map, y: y, x: x, dir: -1)
        right = spill(map, y: y, x: x, dir: +1)
        left && right
      end
    end
  end

  # Flow horizontally, left or right
  # @return [Boolean] is boxed in
  def spill(map, y:, x:, dir:)
    # printf("spill(map, y: %d, x: %d, dir: %d)\n", y, x, dir)
    return true if map.water[y][x + dir]
    return false unless map.inbounds?(y: y, x: x + dir)
    return true if map.clay?(y: y, x: x + dir)

    map.water[y][x + dir] = true
    # render(map)

    boxed_down =
      map.clay?(y: y + 1, x: x + dir) || new_drop(map, y: y + 1, x: x +  dir)

    spill(map, y: y, x: x + dir, dir: dir) if boxed_down
  end

  def clear_screen
    printf('%s[2J%s[H', 27.chr, 27.chr)
  end

  def parse(body)
    ranges = body.lines.map(&method(:clay_ranges))
    Map.new(ranges)
  end

  def clay_ranges(line)
    Hash[
      line.split(', ')
        .map { |part| part.split('=') }
        .map { |part| [part.first.to_sym, part.last.scan(/\d+/)] }
        .map { |part| [part.first, part.last.map(&:to_i)] }
    ].transform_values! do |nums|
      Range.new(*(nums.size == 1 ? [nums.first, nums.first] : nums))
    end
  end

  def render(map)
    printf("-------------------\n")
    map.clay.each_with_index do |row, y|
      line =
        row.each_with_index
          .map { |cell, x| cell ? CLAY : (map.water[y][x] ? WATER : ' ') }
          .join
      printf("%s\n", line)
    end
    printf("\n\n")
  end
end

puzzle = Puzzle.new(ARGV[0])
# puzzle.call_1
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
