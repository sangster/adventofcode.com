#!/bin/env ruby
require 'pry-byebug'
require 'colorize'

class Puzzle
  SYMBOLS = '.#'
  def initialize(path)
    @initial, @map = parse(*IO.read(path).split("\n\n", 2))
    @mask = 2 ** 5 - 1
  end

  def call_1
    grow(20)
  end

  def call_2
    grow(50_000_000_000)
  end

  private

  def grow(generations)
    state = [@initial, 0]
    gen = 0
    while gen < generations
      printf("\r%.8f", gen * 100.0 / generations) if gen % 100000 == 0
      state = mutate(*state)
      gen += 1
    end
    printf("\n")

    sum_state(*state)
  end

  def mutate(state, center)
    state <<= 4
    new_state = 0
    idx = 0
    offset = nil

    while state != 0
      val = @map[state & @mask]
      new_state |= val << (idx + 2)

      offset = 4 - idx if val == 1 && !offset && idx <= 5

      idx += 1
      state >>= 1
    end

    new_state >>= 1 until new_state & 1 == 1

    [new_state, center + offset.to_i - 2]
  end

  def sum_state(state, center)
    (0...(state.size * 8))
      .map { |i| state[i] == 1 ? i - center : 0 }
      .sum
  end

  def print(states)
    max_center = states.map(&:last).max
    lines =
      states.map { |s, c| ('.' * (max_center - c)) + to_plant(s) }
    width = lines.map(&:size).max
    states.each_with_index do |state, i|
      printf("%s  %d\n", lines[i].ljust(width, '.'), sum_state(*state))
    end
  end

  def parse(initial_str, map_str)
    [
      to_2(initial_str.split(': ').last),
      # Hash[map_str.lines.map(&method(:parse_entity))],
      map_str.lines.map(&method(:parse_entity)).sort_by(&:first).map(&:last)
    ]
  end

  def to_2(str)
    str.reverse.tr('.#', '01').to_i(2)
  end

  def to_plant(num, width: 0)
    num.to_s(2).reverse.rjust(width, '0').tr('01', '.#')
  end

  def parse_entity(line)
    line.strip.split(' => ').map(&method(:to_2))
  end
end

puzzle = Puzzle.new(ARGV[0])
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
