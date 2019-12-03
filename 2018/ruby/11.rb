#!/bin/env ruby
require 'pry-byebug'

$serial = IO.read(ARGV[0]).to_i
$size = 300

Cell = Struct.new(:x, :y) do
  def rack_id
    x + 10
  end

  def power
    @power ||= (rack_id * y + $serial) * rack_id / 100 % 10 - 5
  end
end

class Part1And2
  def initialize
    @grid = cells.to_a
    @grid.each_slice(300).each do |line|
      puts line.map { |c| format('%2d', c.power) }.join(' ')
    end
  end

  def call_1
    grp = largest_group(200)
    [grp.first.x, grp.first.y].join(',')
  end

  def call_2_old
    (1..$size).map do |grp|
      [grp, largest_group(grp).first].tap do |res|
        printf("%3d", grp)
      end
    end
  end

  def call_2
    # threads =
    #   (1..$size).map do |grp|
    #     Thread.new do
    #       [grp, largest_group(grp).first].tap do |res|
    #         printf("%3d", grp)
    #       end
    #     end
    #   end

    # threads.map(&:join)
    # threads.map(&:value)
  end

  private

  def cell(x, y)
    @grid[(x - 1) * $size + (y - 1)]
  end

  def largest_group(grp)
    cell_groups(grp).max { |a, b| sum_power(a) <=> sum_power(b) }
  end

  def sum_power(grp)
    grp.inject(0) { |sum, c| sum + c.power }
  end

  def cells
    Enumerator.new do |yielder|
      (1..$size).each do |x|
        (1..$size).each { |y| yielder.yield(Cell.new(x, y)) }
      end
    end
  end

  def cell_groups(grp)
    Enumerator.new do |yielder|
      (1..($size - grp + 1)).each do |x|
        (1..($size - grp + 1)).each do |y|
          yielder.yield(
            (0...grp).flat_map do |i|
              (0...grp).map { |j| cell(x + i, y + j) }
            end
          )
        end
      end
    end
  end
end

puzzle = Part1And2.new
# printf("1: %p\n2: %p\n",
#             puzzle.call_1,
#             puzzle.call_2)
