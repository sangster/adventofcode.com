#!/bin/env ruby

require 'pry-byebug'

$ops = {
  # Addition
  addr: ->(r, a, b, c) { r[c] = r[a] + r[b] },
  addi: ->(r, a, b, c) { r[c] = r[a] + b },

  # Multiplication
  mulr: ->(r, a, b, c) { r[c] = r[a] * r[b] },
  muli: ->(r, a, b, c) { r[c] = r[a] * b },

  # Bitwise AND
  banr: ->(r, a, b, c) { r[c] = r[a] & r[b] },
  bani: ->(r, a, b, c) { r[c] = r[a] & b },

  # Bitwise OR
  borr: ->(r, a, b, c) { r[c] = r[a] | r[b] },
  bori: ->(r, a, b, c) { r[c] = r[a] | b },

  # Assignment
  setr: ->(r, a, b, c) { r[c] = r[a] },
  seti: ->(r, a, b, c) { r[c] = a },

  # Greater-than testing
  gtir: ->(r, a, b, c) { r[c] = a > r[b] ? 1 : 0 },
  gtri: ->(r, a, b, c) { r[c] = r[a] > b ? 1 : 0 },
  gtrr: ->(r, a, b, c) { r[c] = r[a] > r[b] ? 1 : 0 },

  # Equality testing
  eqir: ->(r, a, b, c) { r[c] = a == r[b] ? 1 : 0 },
  eqri: ->(r, a, b, c) { r[c] = r[a] == b ? 1 : 0 },
  eqrr: ->(r, a, b, c) { r[c] = r[a] == r[b] ? 1 : 0 },
}


class Puzzle
  def initialize(path)
    @file = IO.read(path)
    @part_1, @part_2 = @file.split("\n\n\n\n", 2).map(&:strip)
  end

  def call_1(min_count: 3)
    has_min_count =
      samples.select do |sample|
        $ops.values.select do |op|
          cmd = sample[:command]

          registry = sample[:before].dup
          op.call(registry, cmd[1], cmd[2], cmd[3])

          registry == sample[:after]
        end.size >= min_count
      end

    has_min_count.size
  end

  def call_2
    registry = [0, 0, 0, 0]
    opnames = get_operation_names

    test_program.each do |cmd|
      $ops[opnames[cmd[0]]].call(registry, cmd[1], cmd[2], cmd[3])
    end

    registry[0]
  end

  private

  def get_operation_names
    reduce_possibilities(sample_possibilities)
  end

  def sample_possibilities
    Array.new(16) { [] }.tap do |possibilities|
      samples.each do |sample|
        $ops.select do |name, op|
          cmd = sample[:command]

          registry = sample[:before].dup
          op.call(registry, cmd[1], cmd[2], cmd[3])

          possibilities[cmd[0]] << name if registry == sample[:after]
        end
      end
      possibilities.each(&:uniq!)
    end
  end

  def reduce_possibilities(possibilities)
    Array.new(possibilities.size).tap do |ops|
      possibilities.size.times do
        idx = possibilities.find_index { |possible| possible.size == 1 }
        opname = possibilities[idx].first
        ops[idx] = opname
        possibilities.each { |possible| possible.delete(opname) }
      end
    end
  end

  def samples
    @part_1.split("\n\n").map do |sample|
      lines = sample.lines
      { before:  numbers(lines[0]),
        command: numbers(lines[1]),
        after:   numbers(lines[2]) }
    end
  end

  def test_program
    @part_2.split("\n").map(&method(:numbers))
  end

  def numbers(str)
    str.scan(/\d+/).map(&:to_i)
  end
end


puzzle = Puzzle.new(ARGV[0])
printf("1: %p\n2: %p\n", puzzle.call_1, puzzle.call_2)
